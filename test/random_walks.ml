open Dagger
open Lmh_incremental_inference

(** The purpose of this test is to check (visually) that a random walk converges
    to its equilibrium *)

let walk (low : int) (hi : int) =
  assert (low < hi) ;
  let mid = low + ((hi - low) / 2) in
  let card = hi - low + 1 in
  let last = card - 1 in
  let matrix = Array.make_matrix card card 0.0 in
  matrix.(0).(0) <- 0.5 ;
  matrix.(0).(1) <- 0.5 ;
  matrix.(last).(last) <- 0.5 ;
  matrix.(last).(last - 1) <- 0.5 ;
  for i = 1 to last - 1 do
    matrix.(i).(i) <- 0.5 ;
    matrix.(i).(i - 1) <- 0.25 ;
    matrix.(i).(i + 1) <- 0.25
  done ;
  let ll_matrix = Array.map (Array.map Log_space.of_float) matrix in
  Dist.kernel
    mid
    (fun pos rng_state ->
      let stay = Stats_dist.Gen.bool rng_state in
      if stay then pos
      else if pos = low then low + 1
      else if pos = hi then hi - 1
      else
        let up = Stats_dist.Gen.bool rng_state in
        if up then pos + 1 else pos - 1)
    (fun pos pos' -> ll_matrix.(pos - low).(pos' - low))

let plot data =
  if Helpers.produce_artifacts then
    Plot.(
      run
        ~target:(qt ())
        exec
        (plot2
           ~xaxis:"x"
           ~yaxis:"y"
           [ Histogram.hist
               ~binwidth:1.0
               ~points:(Data.of_list @@ List.map r1 data)
               () ]))

let run_simple () =
  let rng_state = RNG.make [| 0x1337; 0x533D |] in
  let model = sample (walk 10 20) in
  let samples =
    stream_samples model rng_state
    |> Helpers.take 100_000 |> Seq.map float_of_int |> List.of_seq
  in
  plot samples

let tests =
  [ ( QCheck.Test.make ~name:"random walk" ~count:1 QCheck.unit @@ fun () ->
      run_simple () ;
      true ) ]
