module Syntax = Cps_monad.Make (Identity_monad)

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
        mutable ll : Log_space.t;
        mutable pos : 'a;
        dist : 'a Dist.t;
        k : 'a -> 'ret
      }
      -> 'ret sampling_point

type score = int * Log_space.t

module Table (T : sig
  type t
end) =
struct
  let samples : (Log_space.t * T.t sampling_point) list ref = ref []

  let scores : score list ref = ref []

  let push_sample sp =
    let s = !samples in
    match s with
    | [] -> samples := [(Log_space.one, sp)]
    | (cumu, Sampling_point { ll; _ }) :: _ ->
        samples := (Log_space.mul cumu ll, sp) :: s

  let push_score sc = scores := sc :: !scores

  let total_score () =
    let total_samples =
      match !samples with
      | [] -> Log_space.one
      | (cumu, Sampling_point { ll; _ }) :: _ -> Log_space.mul cumu ll
    in
    let total_scores =
      List.fold_left
        (fun acc (_, ll) -> Log_space.mul acc ll)
        Log_space.one
        !scores
    in
    (total_samples, Log_space.mul total_samples total_scores)

  let cardinal () = List.length !samples

  let wipe_after limit =
    samples :=
      List.filter (fun (_, Sampling_point { uid; _ }) -> uid <= limit) !samples ;
    scores := List.filter (fun (uid, _) -> uid <= limit) !scores

  let save () = (!samples, !scores)

  let restore (spl, scr) =
    samples := spl ;
    scores := scr

  let handler : RNG.t -> Syntax.handler =
    let open Syntax in
    fun rng_state ->
      { handler =
          (fun (type a) (dist : a payload) ->
            { cont =
                (fun k ->
                  match dist with
                  | Dist (Stateless { sample; ll } as d) ->
                      let pos = sample rng_state in
                      let ll = ll pos in
                      let uid = Counter.gen () in
                      push_sample
                        (Sampling_point
                           { uid; ll; dist = d; k = Obj.magic k; pos }) ;
                      k pos
                  | Dist (Kernel { start; sample; ll } as d) ->
                      let pos = sample start rng_state in
                      let ll = ll start pos in
                      let uid = Counter.gen () in
                      push_sample
                        (Sampling_point
                           { uid; ll; dist = d; k = Obj.magic k; pos }) ;
                      k pos
                  | Score (m, ll) ->
                      let uid = Counter.gen () in
                      push_score (uid, ll) ;
                      k m)
            })
      }
end

(* TODO: functorize Cps over handler to allow inlining *)
let stream_samples (type a) (v : a Syntax.t) rng_state : a Seq.t =
  let module Table = Table (struct
    type t = a
  end) in
  let select_resampling rng_state =
    let sample_points =
      (* TODO inefficient *)
      Array.of_list !Table.samples
    in
    let length = Array.length sample_points in
    if length = 0 then None else Some sample_points.(RNG.int rng_state length)
  in
  let first_run (v : a Syntax.t) rng_state =
    let handler = Table.handler rng_state in
    (v ~handler).cont (fun x -> x)
  in
  let first_value = first_run v rng_state in
  let sample_step (prev_value : a) rng_state =
    let prev_card = Table.cardinal () in
    let (prev_total_sampling_score, prev_total_score) = Table.total_score () in
    match select_resampling rng_state with
    | None -> prev_value
    | Some
        ( prefix_score,
          Sampling_point ({ uid; ll = prev_ll; dist; pos; k } as payload) ) ->
        let propose rng_state =
          match dist with
          | Stateless d ->
              let sample = d.sample rng_state in
              let ll = d.ll sample in
              (sample, prev_ll, ll)
          | Kernel d ->
              let sample = d.sample pos rng_state in
              let fwd_ll = d.ll pos sample in
              let bwd_ll = d.ll sample pos in
              (sample, bwd_ll, fwd_ll)
        in
        let prev_pos = pos in
        let (proposed, bwd_ll, fwd_ll) = propose rng_state in
        let saved_trace = Table.save () in
        let backward_sampling_score =
          Log_space.div prev_total_sampling_score prefix_score
        in
        Table.wipe_after uid ;
        let () =
          (* this must be set before calling the continuation:
             - new samples are pushed during the continuation execution
             - the total score is updated incrementally from the field [ll]
             -> if we don't set it to its correct value, the [new_total_score]
                will be incorrect.
          *)
          payload.pos <- proposed ;
          payload.ll <- fwd_ll
        in
        let (new_value : a) = k proposed in
        let (new_total_sampling_score, new_total_score) =
          Table.total_score ()
        in
        let new_card = Table.cardinal () in
        let forward_sampling_score =
          Log_space.div new_total_sampling_score prefix_score
        in
        let forward_flow =
          Log_space.(
            mul
              prev_total_score
              (mul
                 (of_float (1. /. float_of_int prev_card))
                 forward_sampling_score))
        in
        let backward_flow =
          Log_space.(
            mul
              new_total_score
              (mul
                 (of_float (1. /. float_of_int new_card))
                 backward_sampling_score))
        in
        let ratio = Log_space.div backward_flow forward_flow in
        let acceptance = Log_space.(to_float (min one ratio)) in
        if RNG.float rng_state 1.0 < acceptance then new_value
        else (
          payload.pos <- prev_pos ;
          payload.ll <- bwd_ll ;
          Table.restore saved_trace ;
          prev_value)
  in
  Seq.unfold
    (fun prev_value ->
      let next_value = sample_step prev_value rng_state in
      Some (prev_value, next_value))
    first_value

include Syntax
module List_ops = Foldable.Make_list (Syntax)
module Seq_ops = Foldable.Make_seq (Syntax)
module Array_ops = Foldable.Make_array (Syntax)
