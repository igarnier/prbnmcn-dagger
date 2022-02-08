open Dagger
open Lmh_inference

let diffusion = sample @@ Dist.iid 2 @@ Stats_dist.brownian ~start:0.0 ~std:1.

let plot data =
  if Helpers.produce_artifacts then
    let plot =
      Plot.plot2
        ~xaxis:"x"
        ~yaxis:"y"
        (List.map
           (fun points ->
             Plot.Line.line
               ~points:(Plot.Data.of_list (List.map Plot.r1 points))
               ())
           data)
    in
    Plot.run ~target:Plot.(qt ()) ~plot Plot.exec

let run_traced () =
  let rng_state = RNG.make [| 0x1337; 0x533D |] in
  let samples = 2000 in
  let t0 = Unix.gettimeofday () in
  let res = stream_samples diffusion rng_state |> Helpers.take 2000 in
  let t1 = Unix.gettimeofday () in
  let res = List.of_seq res in
  let (x1, x2) = res |> List.map (fun a -> (a.(0), a.(1))) |> List.split in
  plot [x1; x2] ;
  Format.eprintf
    "performed %d samples in %f seconds (%f samples/sec)"
    samples
    (t1 -. t0)
    (float_of_int samples /. (t1 -. t0))

open Lmh_incremental_inference

let () = if Helpers.produce_artifacts then Cgraph.Internal.set_debug true

let diffusion = sample @@ Stats_dist.brownian ~start:0.0 ~std:1.0

let run_incremental () =
  let rng_state = RNG.make [| 0x1337; 0x533D |] in
  let samples = 2000 in
  let t0 = Unix.gettimeofday () in
  let res = stream_samples diffusion rng_state |> Helpers.take 2000 in
  let t1 = Unix.gettimeofday () in
  let res = List.of_seq res in
  plot [res] ;
  Format.eprintf
    "performed %d samples in %f seconds (%f samples/sec)"
    samples
    (t1 -. t0)
    (float_of_int samples /. (t1 -. t0))

let tests =
  [ ( QCheck.Test.make ~name:"diffusion" ~count:1 QCheck.unit @@ fun () ->
      run_traced () ;
      run_incremental () ;
      true ) ]
