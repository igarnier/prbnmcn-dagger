module Trace = struct
  type sample =
    | Sample :
        { uid : int;
          dist : 'a Dist.dist;
          var : 'a Cgraph.Var.t;
          score : Log_space.t
        }
        -> sample
    | Kernel_sample :
        { uid : int;
          dist : 'a Dist.kernel;
          var : ('a * 'a) Cgraph.Var.t;
          score : Log_space.t
        }
        -> sample

  type sample_trace = sample list

  type score = Score of { uid : int; score : Log_space.t }

  type score_trace = score list

  type t = { samples : sample_trace; scores : score_trace }

  let empty = { samples = []; scores = [] }

  let uid = function Sample { uid; _ } | Kernel_sample { uid; _ } -> uid
    [@@inline]

  let score = function
    | Sample { score; _ } | Kernel_sample { score; _ } -> score
    [@@inline]

  module Internal_for_tests = struct
    let rec equal_trace (trace1 : sample_trace) (trace2 : sample_trace) =
      match (trace1, trace2) with
      | ([], []) -> true
      | ([], _) | (_, []) -> false
      | (s1 :: tl1, s2 :: tl2) -> uid s1 = uid s2 && equal_trace tl1 tl2

    let pp fmtr trace =
      let open Format in
      pp_print_list
        ~pp_sep:(fun fmtr () -> fprintf fmtr ", ")
        (fun fmtr s ->
          let uid = uid s in
          fprintf fmtr "{%d}" uid)
        fmtr
        trace
  end
  [@@ocaml.warning "-32"]

  let total_sample trace =
    let rec loop list acc =
      match list with
      | [] -> acc
      | hd :: tl -> loop tl (Log_space.mul (score hd) acc)
    in
    loop trace Log_space.one

  let total_score trace =
    let rec loop list acc =
      match list with
      | [] -> acc
      | Score { score; _ } :: tl -> loop tl (Log_space.mul score acc)
    in
    loop trace Log_space.one

  let total trace =
    let total_sampling_score = total_sample trace.samples in
    ( total_sampling_score,
      Log_space.mul total_sampling_score (total_score trace.scores) )

  let cardinal { samples; _ } = List.length samples

  let rec add_sample s trace =
    match trace with
    | [] -> [s]
    | (Kernel_sample { uid = uid'; _ } as hd) :: tl
    | (Sample { uid = uid'; _ } as hd) :: tl ->
        let uid = uid s in
        if uid < uid' then hd :: add_sample s tl
        else if uid > uid' then s :: trace
        else trace

  let rec add_score (Score { uid; _ } as s) trace =
    match trace with
    | [] -> [s]
    | (Score { uid = uid'; _ } as hd) :: tl ->
        if uid < uid' then hd :: add_score s tl
        else if uid > uid' then s :: trace
        else trace

  let add_sample (s : sample) trace =
    { trace with samples = add_sample s trace.samples }

  let add_score (s : score) trace =
    { trace with scores = add_score s trace.scores }

  let rec intersect_samples trace1 trace2 =
    match (trace1, trace2) with
    | ([], _) | (_, []) -> []
    | (s1 :: tl1, s2 :: tl2) ->
        let uid1 = uid s1 in
        let uid2 = uid s2 in
        if uid1 < uid2 then intersect_samples trace1 tl2
        else if uid1 > uid2 then intersect_samples tl1 trace2
        else s1 :: intersect_samples tl1 tl2
    [@@ocaml.warning "-32"]

  let rec union_samples trace1 trace2 =
    match (trace1, trace2) with
    | ([], t) | (t, []) -> t
    | (s1 :: tl1, s2 :: tl2) ->
        let uid1 = uid s1 in
        let uid2 = uid s2 in
        if uid1 < uid2 then s2 :: union_samples trace1 tl2
        else if uid1 > uid2 then s1 :: union_samples tl1 trace2
        else (* assert s1 = s2 *)
          s1 :: union_samples tl1 tl2

  let rec union_scores trace1 trace2 =
    match (trace1, trace2) with
    | ([], t) | (t, []) -> t
    | ( (Score { uid = uid1; _ } as s1) :: tl1,
        (Score { uid = uid2; _ } as s2) :: tl2 ) ->
        if uid1 < uid2 then s2 :: union_scores trace1 tl2
        else if uid1 > uid2 then s1 :: union_scores tl1 trace2
        else (* assert s1 = s2 *)
          s1 :: union_scores tl1 tl2
    [@@ocaml.warning "-32"]

  let union t1 t2 =
    { samples = union_samples t1.samples t2.samples;
      scores = union_scores t1.scores t2.scores
    }
end

module Counter = struct
  let x = ref 0

  let gen () =
    let v = !x in
    incr x ;
    v
end

module Traced = Traced_monad.Make (Incremental_monad) (Trace)

module Dummy_state = struct
  type 'a t = unit
end

module Syntax = struct
  include Cps_monad.Make (Traced) (Dummy_state)
  include Lmh_generic.Make (Traced) (Dummy_state)

  type 'a t = (unit, 'a) m

  type 'a shared = 'a Traced.t

  let with_shared (m : 'a t) (f : 'a shared -> 'b t) : 'b t =
   fun ~handler ->
    let m = (m ~handler).cont (fun x () () -> x) () () in
    { cont = (fun k () -> (f m ~handler).cont k ()) }

  (* This would work for any mappable container. *)
  let with_shared_list (ms : 'a t list) (f : 'a shared list -> 'b t) : 'b t =
   fun ~handler ->
    let ms =
      List.map (fun m -> (m ~handler).cont (fun x () () -> x) () ()) ms
    in
    { cont = (fun k () -> (f ms ~handler).cont k ()) }

  let with_shared_array (ms : 'a t array) (f : 'a shared array -> 'b t) : 'b t =
   fun ~handler ->
    let ms =
      Array.map (fun m -> (m ~handler).cont (fun x () () -> x) () ()) ms
    in
    { cont = (fun k -> (f ms ~handler).cont k) }

  let use : 'a shared -> 'a t =
   fun node ~handler:_ -> { cont = (fun k () () -> k node () ()) }

  module Make_shared (C : sig
    type 'a t

    val map : 'a t -> ('a -> 'b) -> 'b t
  end) =
  struct
    let with_shared (ms : 'a t C.t) (f : 'a shared C.t -> 'b t) : 'b t =
     fun ~handler ->
      let ms = C.map ms (fun m -> (m ~handler).cont (fun x () () -> x) () ()) in
      { cont = (fun k () () -> (f ms ~handler).cont k () ()) }
  end

  module Infix = struct
    include Infix

    let ( let*! ) = with_shared

    let use = use
  end
end

let handler : RNG.t -> unit Syntax.handler =
  let open Syntax in
  fun rng_state ->
    { handler =
        (fun (type a) (dist : a Cps_monad.effect) k () ->
          match dist with
          | Dist dist -> (
              match (Cgraph.get dist).value with
              | Stateless { sample; ll = _ } ->
                  let pos = sample rng_state in
                  let var = Cgraph.Var.create pos in
                  let node = Cgraph.var var in
                  let node =
                    Cgraph.map2 node dist (fun sample dist ->
                        match dist.value with
                        | Kernel _ ->
                            (* A distribution can't dynamically switch from stateless to kernel
                               (TODO: lift this) *)
                            failwith
                              "Lmh_incremental_inference.handler: distribution \
                               switched from Stateless to Kernel"
                        | Stateless ({ ll; _ } as d) ->
                            let score = ll sample in
                            let uid = Counter.gen () in
                            let trace =
                              Trace.add_sample
                                (Trace.Sample { uid; dist = d; var; score })
                                dist.trace
                            in
                            { Traced.value = sample; trace })
                  in
                  k node ()
              | Kernel ({ start; sample; ll = _ } as d) ->
                  let pos = sample start rng_state in
                  let var = Cgraph.Var.create (start, pos) in
                  let node = Cgraph.var var in
                  let node =
                    Cgraph.map2 node dist (fun (prev, current) dist ->
                        match dist.value with
                        | Stateless _ ->
                            (* A distribution can't dynamically switch from kernel to stateless
                               (TODO: lift this) *)
                            failwith
                              "Lmh_incremental_inference.handler: distribution \
                               switched from Kernel to Stateless"
                        | Kernel { ll; _ } ->
                            let score = ll prev current in
                            let uid = Counter.gen () in
                            let trace =
                              Trace.add_sample
                                (Trace.Kernel_sample
                                   { uid; dist = d; var; score })
                                dist.trace
                            in
                            { Traced.value = current; trace })
                  in
                  k node ())
          | Score m ->
              k
                (Cgraph.map m (fun { Traced.value = (value, score); trace } ->
                     let uid = Counter.gen () in
                     { Traced.value;
                       trace =
                         Trace.add_score (Trace.Score { uid; score }) trace
                     }))
                ()
          | _unknown_effect ->
              invalid_arg "Lmh_incremental_inference: unknown effect")
    }

type processed_trace =
  { trace : Trace.t;
    card : int;
    samples : Trace.sample array Lazy.t;
    sampling_score : Log_space.t;
    score : Log_space.t
  }

let to_dot fname (model : 'a Syntax.t) =
  let oc = open_out fname in
  let handler = handler (RNG.make [| 0x1337; 0x533D |]) in
  Cgraph.Internal.set_debug true ;
  let model = (model ~handler).cont (fun x () () -> x) () () in
  let _ = Cgraph.get model in
  Cgraph.Internal.set_debug false ;
  Cgraph.Internal.(to_dot ~mode:Full (Cgraph.ex (Obj.magic model)) oc) ;
  close_out oc

let process : Trace.t -> processed_trace =
 fun trace ->
  let samples = lazy (Array.of_list trace.samples) in
  let card = Trace.cardinal trace in
  let (sampling_score, score) = Trace.total trace in
  { trace; card; samples; sampling_score; score }

let stream_samples (type a) (v : a Syntax.t) rng_state : a Seq.t =
  let handler = handler rng_state in
  let v = (v ~handler).cont (fun x () () -> x) () () in
  let select_resampling ({ samples; card; _ } : processed_trace) rng_state =
    if card = 0 then None
    else
      let samples = Lazy.force samples in
      Some samples.(RNG.int rng_state card)
    [@@inline]
  in
  let run (v : a Traced.t) = Cgraph.get v in
  let { Traced.value = first_value; trace = first_trace } = run v in
  let mcmc_move prev_value prev_trace _fwd_ll _bwd_ll undo =
    let { Traced.value = new_value; trace = new_trace } = Cgraph.get v in
    let new_trace = process new_trace in
    let intersection =
      Trace.intersect_samples prev_trace.trace.samples new_trace.trace.samples
    in
    (* Format.printf "old/new intersection: %a@." Trace.pp intersection ; *)
    let intersection_score = Trace.total_sample intersection in
    let forward_sampling_score =
      Log_space.mul
        Log_space.one
        (Log_space.div new_trace.sampling_score intersection_score)
    in
    let backward_sampling_score =
      Log_space.mul
        Log_space.one
        (Log_space.div prev_trace.sampling_score intersection_score)
    in
    let forward_flow =
      Log_space.(
        mul
          prev_trace.score
          (div forward_sampling_score (of_float (float_of_int prev_trace.card))))
    in
    let backward_flow =
      Log_space.(
        mul
          new_trace.score
          (div backward_sampling_score (of_float (float_of_int new_trace.card))))
    in
    let ratio = Log_space.div backward_flow forward_flow in
    let acceptance = Log_space.(min one ratio) in
    if Log_space.lt (Log_space.of_float (RNG.float rng_state 1.0)) acceptance
    then (new_value, new_trace)
    else
      let () = Cgraph.undo undo in
      (prev_value, prev_trace)
  in
  let sample_step (prev_value : a) (prev_trace : processed_trace) rng_state =
    match select_resampling prev_trace rng_state with
    | None -> (prev_value, prev_trace)
    | Some (Trace.Kernel_sample { uid = _; dist; var; score = _ }) ->
        let (_previous, current) = Cgraph.Var.peek var in
        let sample = dist.sample current rng_state in
        let fwd_ll = dist.ll current sample in
        let undo = Cgraph.Var.set_with_undo var (current, sample) in
        let bwd_ll = dist.ll sample current in
        mcmc_move prev_value prev_trace fwd_ll bwd_ll undo
    | Some (Trace.Sample { uid = _; dist; var; score = bwd_ll }) ->
        let (undo, fwd_ll) =
          let sample = dist.sample rng_state in
          let ll = dist.ll sample in
          let undo = Cgraph.Var.set_with_undo var sample in
          (undo, ll)
        in
        mcmc_move prev_value prev_trace fwd_ll bwd_ll undo
  in
  Seq.unfold
    (fun (prev_value, prev_trace) ->
      let next = sample_step prev_value prev_trace rng_state in
      Some (prev_value, next))
    (first_value, process first_trace)

include Syntax
module List_ops = Foldable.Make_list (Syntax)
module Seq_ops = Foldable.Make_seq (Syntax)
module Array_ops = Foldable.Make_array (Syntax)
