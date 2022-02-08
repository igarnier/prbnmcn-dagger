open Basic_structures
module Log_space = Dagger.Log_space
module Gen = Stats.Gen.Make (PRNG.LXM.State)
module Pdfs = Stats.Pdfs
open Dagger.Dist

let float bound =
  dist1
    Gen.float
    (fun bound x ->
      Log_space.unsafe_cast @@ Pdfs.uniform_ln { min = 0.0; max = bound } x)
    bound

let int bound =
  dist1
    Gen.int
    (fun bound x ->
      if x < 0 || x >= bound then Log_space.zero
      else Log_space.of_float (1. /. float_of_int bound))
    bound

let bool =
  let ll = Log_space.of_float 0.5 in
  dist0 Gen.bool (fun _ -> ll)

let gaussian ~mean ~std =
  dist2
    (fun mean std rng_state -> Gen.gaussian ~mean ~std rng_state)
    (fun mean std x -> Log_space.unsafe_cast (Pdfs.gaussian_ln ~mean ~std x))
    mean
    std

let brownian ~start ~std =
  kernel1
    (fun std x rng_state -> Gen.gaussian ~mean:x ~std rng_state)
    (fun std x y -> Log_space.unsafe_cast (Pdfs.gaussian_ln ~mean:x ~std y))
    start
    std

let flat min max =
  let range = Stats.Stats_intf.{ min ; max } in
  dist0
    (fun rng_state -> Gen.range range rng_state)
    (fun x -> Log_space.unsafe_cast (Pdfs.uniform_ln range x))

let bernouilli ~bias =
  dist1
    Gen.bernouilli
    (fun bias x ->
      let llt = Log_space.of_float bias in
      let llf = Log_space.of_float (1. -. bias) in
      if x then llt else llf)
    bias

let geometric ~p =
  dist1
    Gen.geometric
    (fun p k -> Log_space.unsafe_cast (Pdfs.geometric_ln ~p k))
    p

let exponential ~rate =
  dist1
    (fun rate rng_state -> Gen.exponential ~rate rng_state)
    (fun rate x -> Log_space.unsafe_cast (Pdfs.exponential_ln ~rate x))
    rate

let uniform cases =
  let sampler = Gen.uniform cases in
  let len = Array.length cases in
  let log_w = Log_space.of_float (1. /. float_of_int len) in
  let log_pdf _elt = log_w in
  dist0 sampler log_pdf

let without_replacement cases n =
  let log_pdf n k =
    assert (n >= k) ;
    Log_space.unsafe_cast
    @@ Stats.Specfun.log_factorial k
       +. Stats.Specfun.log_factorial (n - k)
       -. Stats.Specfun.log_factorial n
  in
  let len = Array.length cases in
  let cases = Array.to_list cases in
  let sampler n rng_state = Gen.without_replacement n cases rng_state in
  let log_pdf k _elt = log_pdf len k in
  dist1 sampler log_pdf n

let categorical (type a) (module O : Basic_intf.Std with type t = a)
    (cases : (a * float) list) =
  let sampler = Gen.categorical cases in
  let module FV = Basic_impl.Free_module.Float_valued.Make_with_map (O) in
  let fin = Stats.Fin.Float.(normalize @@ measure (module FV) cases) in
  dist0 sampler (fun elt ->
      Log_space.of_float (Stats.Fin.Float.eval_prb fin elt))
