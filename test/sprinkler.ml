open Dagger

let () = if Helpers.produce_artifacts then Cgraph.Internal.set_debug true

let rng_state = RNG.make [| 0x1337; 0x533D |]

(* ------------------------------------------------------------------------- *)
(* Using incremental inference *)

module Traced_incremental = struct
  open Lmh_incremental_inference

  let bern = Stats_dist.bernouilli

  let model : bool t =
    let open Infix in
    let*! rain = sample (bern ~bias:0.2) in
    let*! sprinkler = sample (bern ~bias:0.1) in
    let prob_lawn_wet =
      let+ r = use rain and+ s = use sprinkler in
      match (r, s) with
      | (true, true) -> 0.99
      | (true, false) -> 0.7
      | (false, true) -> 0.9
      | (false, false) -> 0.01
    in
    let+ _ = map_score prob_lawn_wet Fun.id and+ r = use rain in
    r

  let () =
    if Helpers.produce_artifacts then
      Lmh_incremental_inference.to_dot "sprinkler.dot" model

  let nsamples = 100_000

  let test =
    QCheck.Test.make
      ~name:"biased coin, traced, incremental"
      ~count:1
      QCheck.unit
    @@ fun () ->
    let samples =
      Lmh_incremental_inference.stream_samples model rng_state
      |> Helpers.drop 1000 |> Helpers.take nsamples
    in
    let freq =
      (samples
      |> Seq.map (fun x -> if x then 1 else 0)
      |> List.of_seq |> List.fold_left ( + ) 0 |> float_of_int)
      /. float_of_int nsamples
    in
    Format.printf "probability that it rains: %f@." freq ;
    abs_float (freq -. 0.64) <. 0.01
end

(* ------------------------------------------------------------------------- *)
(* Using non-incremental inference *)

module Traced = struct
  open Lmh_inference

  let model : bool t =
    let open Infix in
    let* rain = sample (Stats_dist.bernouilli ~bias:0.2) in
    let* sprinkler = sample (Stats_dist.bernouilli ~bias:0.1) in
    let prob_lawn_wet =
      match (rain, sprinkler) with
      | (true, true) -> 0.99
      | (true, false) -> 0.7
      | (false, true) -> 0.9
      | (false, false) -> 0.01
    in
    let+ () = score prob_lawn_wet in
    rain

  let nsamples = 100_000

  let test =
    QCheck.Test.make ~name:"biased coin, traced" ~count:1 QCheck.unit
    @@ fun () ->
    let samples =
      Lmh_inference.stream_samples model rng_state
      |> Helpers.drop 1000 |> Helpers.take nsamples
    in
    let freq =
      (samples
      |> Seq.map (fun x -> if x then 1 else 0)
      |> List.of_seq |> List.fold_left ( + ) 0 |> float_of_int)
      /. float_of_int nsamples
    in
    Format.printf "probability that it rains: %f@." freq ;
    abs_float (freq -. 0.64) <. 0.01
end

let tests = [Traced_incremental.test; Traced.test]
