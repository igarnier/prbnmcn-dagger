open Dagger

module Pure =
  Internal.Cps_monad.Make
    (Internal.Identity_monad)
    (struct
      type 'a t = unit
    end)

let run_pure () =
  let open Pure in
  let model =
    map_array [| return 0; return 1 |] @@ fun a -> a.(0) = 0 && a.(1) = 1
  in
  let handler = Pure.{ handler = (fun _eff _k _w -> assert false) } in
  (model ~handler).cont (fun x _ () -> x) () ()

let run_lmh () =
  let open Lmh_inference in
  let model =
    map_array [| return 0; return 1 |] @@ fun a -> a.(0) = 0 && a.(1) = 1
  in
  let rng_state = RNG.make [| 0x1337; 0x533D |] in
  let samples = stream_samples model rng_state in
  let samples = Helpers.take 1000 samples |> List.of_seq in
  List.for_all Fun.id samples

let run_lmh_incremental () =
  let open Lmh_incremental_inference in
  let model =
    map_array [| return 0; return 1 |] @@ fun a -> a.(0) = 0 && a.(1) = 1
  in
  let rng_state = RNG.make [| 0x1337; 0x533D |] in
  let samples = stream_samples model rng_state in
  let samples = Helpers.take 1000 samples |> List.of_seq in
  List.for_all Fun.id samples

let tests =
  [ ( QCheck.Test.make ~name:"map-array-pure" ~count:1 QCheck.unit @@ fun () ->
      run_pure () );
    ( QCheck.Test.make ~name:"map-array-lmh-incr" ~count:1 QCheck.unit
    @@ fun () -> run_lmh_incremental () );
    ( QCheck.Test.make ~name:"map-array-lmh" ~count:1 QCheck.unit @@ fun () ->
      run_lmh () ) ]
