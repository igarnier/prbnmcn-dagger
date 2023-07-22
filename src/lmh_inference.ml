module Counter = struct
  let x = ref 0

  let gen () =
    let v = !x in
    incr x ;
    v
end

type 'ret sampling_point =
  | Sampling_point :
      { uid : int;
        ll : Log_space.t;
        pos : 'a;
        dist : 'a Dist.t;
        k : 'a -> unit -> 'ret trace -> 'ret
      }
      -> 'ret sampling_point

and score = int * Log_space.t

and 'ret trace =
  { scores : score list; samples : (Log_space.t * 'ret sampling_point) list }

let empty_trace = { scores = []; samples = [] }

module Trace = struct
  type 'a t = 'a trace
end

module Cps = Cps_monad.Make (Identity_monad) (Trace)

module Syntax = struct
  type 'a t = (unit, 'a) Cps.m

  include Cps
  include Lmh_generic.Make (Identity_monad) (Trace)
end

module Table = struct
  let push_sample (s : 'a trace) sp =
    match s.samples with
    | [] -> { s with samples = [(Log_space.one, sp)] }
    | (cumu, Sampling_point { ll; _ }) :: _ ->
        { s with samples = (Log_space.mul cumu ll, sp) :: s.samples }

  let push_score (s : 'a trace) sc = { s with scores = sc :: s.scores }

  let total_score { scores; samples } =
    let total_samples =
      match samples with
      | [] -> Log_space.one
      | (cumu, Sampling_point { ll; _ }) :: _ -> Log_space.mul cumu ll
    in
    let total_scores =
      List.fold_left
        (fun acc (_, ll) -> Log_space.mul acc ll)
        Log_space.one
        scores
    in
    (total_samples, Log_space.mul total_samples total_scores)

  let cardinal s = List.length s.samples

  let wipe_after s limit =
    { samples =
        List.filter
          (fun (_, Sampling_point { uid; _ }) -> uid < limit)
          s.samples;
      scores = List.filter (fun (uid, _) -> uid < limit) s.scores
    }

  let handler : RNG.t -> unit Syntax.handler =
    let open Syntax in
    fun rng_state ->
      { handler =
          (fun (type a) (dist : a Cps_monad.effect) k () trace ->
            match dist with
            | Dist (Stateless { sample; ll } as d) ->
                let pos = sample rng_state in
                let ll = ll pos in
                let uid = Counter.gen () in
                let trace =
                  push_sample
                    trace
                    (Sampling_point { uid; ll; dist = d; k; pos })
                in
                k pos () trace
            | Dist (Kernel { start; sample; ll } as d) ->
                let pos = sample start rng_state in
                let ll = ll start pos in
                let uid = Counter.gen () in
                let trace =
                  push_sample
                    trace
                    (Sampling_point { uid; ll; dist = d; k; pos })
                in
                k pos () trace
            | Score (m, ll) ->
                let uid = Counter.gen () in
                let trace = push_score trace (uid, ll) in
                k m () trace
            | _unknown_effect -> invalid_arg "Lmh_inference: unknown effect")
      }
end

(* TODO: functorize Cps over handler to allow inlining *)
let stream_samples (type a) (v : a Syntax.t) rng_state : a Seq.t =
  let select_resampling trace rng_state =
    let sample_points =
      (* TODO inefficient *)
      Array.of_list trace.samples
    in
    let length = Array.length sample_points in
    if length = 0 then None else Some sample_points.(RNG.int rng_state length)
  in
  (* In order to get back the trace out of the evaluation of the continuation,
     we use an exception. *)
  let exception Return of a * a trace in
  let first_run (v : a Syntax.t) rng_state =
    let handler = Table.handler rng_state in
    (v ~handler).cont (fun x () trace -> raise (Return (x, trace)))
  in
  let (first_value, first_trace) =
    match first_run v rng_state () empty_trace with
    | exception Return (x, trace) -> (x, trace)
    | _ -> assert false
  in
  let sample_step (prev_value : a) (prev_trace : a trace) rng_state =
    let prev_card = Table.cardinal prev_trace in
    let (prev_total_sampling_score, prev_total_score) =
      Table.total_score prev_trace
    in
    match select_resampling prev_trace rng_state with
    | None -> (prev_value, prev_trace)
    | Some
        ( prefix_score,
          Sampling_point ({ uid; ll = _prev_ll; dist; pos; k } as _payload) ) ->
        let propose rng_state =
          match dist with
          | Stateless d ->
              let sample = d.sample rng_state in
              let ll = d.ll sample in
              (sample, ll)
          | Kernel d ->
              let sample = d.sample pos rng_state in
              let fwd_ll = d.ll pos sample in
              (sample, fwd_ll)
        in
        let (proposed, fwd_ll) = propose rng_state in
        let backward_sampling_score =
          Log_space.div prev_total_sampling_score prefix_score
        in
        let trace_prefix =
          let trace = Table.wipe_after prev_trace uid in
          { trace with
            samples =
              ( prefix_score,
                Sampling_point { uid; ll = fwd_ll; dist; pos = proposed; k } )
              :: trace.samples
          }
        in
        let (new_value, new_trace) : a * a trace =
          match k proposed () trace_prefix with
          | exception Return (x, trace) -> (x, trace)
          | _ -> assert false
        in
        let (new_total_sampling_score, new_total_score) =
          Table.total_score new_trace
        in
        let new_card = Table.cardinal new_trace in
        let forward_sampling_score =
          Log_space.div new_total_sampling_score prefix_score
        in
        let forward_flow =
          Log_space.(
            mul
              prev_total_score
              (div forward_sampling_score (of_float (float_of_int prev_card))))
        in
        let backward_flow =
          Log_space.(
            mul
              new_total_score
              (div backward_sampling_score (of_float (float_of_int new_card))))
        in
        let ratio = Log_space.div backward_flow forward_flow in
        let acceptance = Log_space.(min one ratio) in
        if
          Log_space.lt (Log_space.of_float (RNG.float rng_state 1.0)) acceptance
        then (new_value, new_trace)
        else (prev_value, prev_trace)
  in
  Seq.unfold
    (fun (prev_value, prev_trace) ->
      let next = sample_step prev_value prev_trace rng_state in
      Some (prev_value, next))
    (first_value, first_trace)

include Syntax
module List_ops = Foldable.Make_list (Syntax)
module Seq_ops = Foldable.Make_seq (Syntax)
module Array_ops = Foldable.Make_array (Syntax)
