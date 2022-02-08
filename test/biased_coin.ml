open Dagger

let stats arr =
  let c = ref 0 in
  Array.iter (fun b -> if b then incr c) arr ;
  let p = float_of_int !c /. float_of_int (Array.length arr) in
  Format.printf "heads probability: %f@." p ;
  p

let rng_state = RNG.make [| 0x1337; 0x533D |]

(* ------------------------------------------------------------------------- *)
(* Using non-incremental inference *)

open Lmh_inference

let fair : bool Dist.t = Stats_dist.bernouilli ~bias:0.5

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
  Format.printf "data generated in %f seconds@." (t1 -. t0) ;
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
  Format.printf "data generated in %f seconds@." (t1 -. t0) ;
  abs_float (stats res -. (2. /. 3.)) <. 0.01

let tests = [traced; traced_incremental]
