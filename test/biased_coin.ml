open Dagger

let stats arr =
  let c = ref 0 in
  Array.iter (fun b -> if b then incr c) arr ;
  let p = float_of_int !c /. float_of_int (Array.length arr) in
  Format.printf "heads probability: %f@." p ;
  p

let stats_weighted total arr =
  let mass =
    Array.fold_left (fun mass (b, w) -> if b then mass +. w else mass) 0.0 arr
  in
  let p = mass /. total in
  Format.printf "heads probability: %f@." p ;
  p

let rng_state = RNG.make [| 0x1337; 0x533D |]

(* ------------------------------------------------------------------------- *)
(* Using non-incremental inference *)

open Lmh_inference

let fair : bool Dist.t = Stats_dist.bernoulli ~bias:0.5

let model =
  let open Infix in
  let* flip = sample fair in
  let* () = score @@ if flip then 2. /. 3. else 1. /. 3. in
  return flip

let eval nsamples : bool array =
  let rec loop n (stream : bool Seq.t) acc =
    if n = nsamples then Array.of_list acc
    else
      match stream () with
      | Nil -> Array.of_list acc
      | Cons (bool, rest) -> loop (Int.succ n) rest (bool :: acc)
  in
  let stream = Lmh_inference.stream_samples model rng_state in
  loop 0 stream []

let traced =
  QCheck.Test.make ~name:"biased coin, traced" ~count:1 QCheck.unit @@ fun () ->
  let t0 = Unix.gettimeofday () in
  let n = 1_000_000 in
  let res = eval n in
  let t1 = Unix.gettimeofday () in
  Format.printf "lmh: data generated in %f seconds@." (t1 -. t0) ;
  abs_float (stats res -. (2. /. 3.)) <. 0.01

(* ------------------------------------------------------------------------- *)
(* Using incremental inference, new version *)

open Lmh_incremental_inference

let model =
  map_score (sample fair) (fun flip -> if flip then 2. /. 3. else 1. /. 3.)

let eval nsamples : bool array =
  let rec loop n (stream : bool Seq.t) acc =
    if n = nsamples then Array.of_list acc
    else
      match stream () with
      | Nil -> Array.of_list acc
      | Cons (bool, rest) -> loop (Int.succ n) rest (bool :: acc)
  in
  let stream = Lmh_incremental_inference.stream_samples model rng_state in
  loop 0 stream []

let traced_incremental =
  QCheck.Test.make ~name:"biased coin, traced, incremental" ~count:1 QCheck.unit
  @@ fun () ->
  let t0 = Unix.gettimeofday () in
  let n = 1_000_000 in
  let res = eval n in
  let t1 = Unix.gettimeofday () in
  Format.printf "incr: data generated in %f seconds@." (t1 -. t0) ;
  abs_float (stats res -. (2. /. 3.)) <. 0.01

(* ------------------------------------------------------------------------- *)
(* Using basic SMC *)

open Smc_inference.Unit_smc

let model =
  let open Infix in
  let* flip = sample fair in
  let* () = score @@ if flip then 2. /. 3. else 1. /. 3. in
  let* () = yield () in
  return flip

let smc =
  QCheck.Test.make ~name:"biased coin, smc" ~count:1 QCheck.unit @@ fun () ->
  let rng_state = RNG.make [| 0x1337; 0x533D |] in
  let t0 = Unix.gettimeofday () in
  let res =
    run
      (Smc_inference.stratified_resampling ~ess_threshold:0.5)
      ()
      ~npart:10_000
      model
      rng_state
    |> Seq.filter_map (fun pop ->
           if Array.length pop.active = 0 then Some pop else None)
    |> List.of_seq |> List.hd
  in
  let t1 = Unix.gettimeofday () in
  Format.printf "smc: data generated in %f seconds@." (t1 -. t0) ;
  abs_float (stats_weighted res.total_mass res.terminated -. (2. /. 3.)) <. 0.01

(* ------------------------------------------------------------------------- *)
(* All tests *)

let tests = [traced; traced_incremental; smc]
