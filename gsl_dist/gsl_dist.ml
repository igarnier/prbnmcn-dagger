module Log_space = Dagger.Log_space
open Gsl

let gsl_rng = ref Rng.RANLUX

let rng_of_lxm (s : PRNG.LXM.State.t) =
  let rng = Rng.make !gsl_rng in
  let seed = PRNG.LXM.State.nativebits s in
  Rng.set rng seed ;
  rng

open Dagger.Dist

let dist0 sampler log_pdf =
  stateless (fun state -> sampler (rng_of_lxm state)) log_pdf
  [@@inline]

let dist1 sampler log_pdf arg =
  stateless
    (fun rng_state -> sampler arg (rng_of_lxm rng_state))
    (fun x -> log_pdf arg x)
  [@@inline]

let dist2 sampler log_pdf arg1 arg2 =
  stateless
    (fun rng_state -> sampler arg1 arg2 (rng_of_lxm rng_state))
    (fun x -> log_pdf arg1 arg2 x)
  [@@inline]

let kernel1 sampler log_pdf start arg =
  kernel
    start
    (fun x rng_state -> sampler arg x (rng_of_lxm rng_state))
    (fun x y -> log_pdf arg x y)
  [@@inline]

let float bound =
  dist1
    (fun b state -> Randist.flat state ~a:0.0 ~b)
    (fun b x -> Log_space.of_float (Randist.flat_pdf x ~a:0.0 ~b))
    bound

let int bound =
  dist1
    (fun b state -> Rng.uniform_int state b)
    (fun b x ->
      if x < 0 || x >= b then Log_space.zero
      else Log_space.of_float (1. /. float_of_int b))
    bound

let bool =
  let ll = Log_space.of_float 0.5 in
  dist0 (fun state -> Randist.bernoulli state ~p:0.5) (fun _ -> ll)

let gaussian ~mean ~std =
  dist2
    (fun mean sigma state -> mean +. Randist.gaussian state ~sigma)
    (fun mean sigma x ->
      Log_space.of_float (Randist.gaussian_pdf (mean +. x) ~sigma))
    mean
    std

let gaussian_tail ~a ~std =
  dist2
    (fun a sigma state -> Randist.gaussian_tail state ~a ~sigma)
    (fun a sigma x ->
      Log_space.of_float (Randist.gaussian_tail_pdf x ~a ~sigma))
    a
    std

let laplace ~a =
  dist1
    (fun a state -> Randist.laplace state ~a)
    (fun a x -> Log_space.of_float (Randist.laplace_pdf x ~a))
    a

let exppow ~a ~b =
  dist2
    (fun a b state -> Randist.exppow state ~a ~b)
    (fun a b x -> Log_space.of_float (Randist.exppow_pdf x ~a ~b))
    a
    b

let cauchy ~a =
  dist1
    (fun a state -> Randist.cauchy state ~a)
    (fun a x -> Log_space.of_float (Randist.cauchy_pdf x ~a))
    a

let rayleigh ~sigma =
  dist1
    (fun sigma state -> Randist.rayleigh state ~sigma)
    (fun sigma x -> Log_space.of_float (Randist.rayleigh_pdf x ~sigma))
    sigma

let rayleigh_tail ~a ~sigma =
  dist2
    (fun a sigma state -> Randist.rayleigh_tail state ~a ~sigma)
    (fun a sigma x ->
      Log_space.of_float (Randist.rayleigh_tail_pdf x ~a ~sigma))
    a
    sigma

let landau =
  dist0 Randist.landau (fun x -> Log_space.of_float (Randist.landau_pdf x))

let gamma ~a ~b =
  dist2
    (fun a b state -> Randist.gamma state ~a ~b)
    (fun a b x -> Log_space.of_float (Randist.gamma_pdf x ~a ~b))
    a
    b

let weibull ~a ~b =
  dist2
    (fun a b state -> Randist.weibull state ~a ~b)
    (fun a b x -> Log_space.of_float (Randist.weibull_pdf x ~a ~b))
    a
    b

let flat a b =
  dist2
    (fun a b state -> Randist.flat state ~a ~b)
    (fun a b x -> Log_space.of_float (Randist.flat_pdf x ~a ~b))
    a
    b

let bernoulli ~bias =
  dist1
    (fun p state -> Randist.bernoulli state ~p = 1)
    (fun p x ->
      if x then Log_space.of_float p else Log_space.of_float (1. -. p))
    bias

let binomial p n =
  dist2
    (fun p n state -> Randist.binomial state ~p ~n)
    (fun p n x -> Log_space.of_float (Randist.binomial_pdf x ~p ~n))
    p
    n

let geometric ~p =
  dist1
    (fun p state -> Randist.geometric state ~p)
    (fun p k -> Log_space.of_float (Randist.geometric_pdf k ~p))
    p

let exponential ~rate =
  dist1
    (fun mu state -> Randist.exponential state ~mu)
    (fun mu x -> Log_space.of_float (Randist.exponential_pdf x ~mu))
    rate

let poisson ~rate =
  dist1
    (fun mu state -> Randist.poisson state ~mu)
    (fun mu x -> Log_space.of_float (Randist.poisson_pdf x ~mu))
    rate

let categorical (type a) (module H : Hashtbl.S with type key = a)
    (cases : (a * float) array) =
  let xs = Array.map fst cases in
  let ps = Array.map snd cases in
  let contents = Array.mapi (fun i x -> (x, i)) xs in
  let table = H.of_seq (Array.to_seq contents) in
  let sampler = Randist.discrete_preproc ps in
  dist0
    (fun state ->
      let index = Randist.discrete state sampler in
      xs.(index))
    (fun elt ->
      match H.find_opt table elt with
      | None -> assert false
      | Some i -> Log_space.of_float (Randist.discrete_pdf i sampler))

let beta ~a ~b =
  dist2
    (fun a b state -> Randist.beta state ~a ~b)
    (fun a b x -> Log_space.of_float (Randist.beta_pdf ~a ~b x))
    a
    b

let dirichlet ~alpha =
  dist1
    (fun alpha state ->
      let theta = Array.make (Array.length alpha) 0.0 in
      Randist.dirichlet state ~alpha ~theta ;
      theta)
    (fun alpha theta ->
      Log_space.unsafe_cast (Randist.dirichlet_lnpdf ~alpha ~theta))
    alpha

let lognormal ~zeta ~sigma =
  dist2
    (fun zeta sigma state -> Randist.lognormal state ~zeta ~sigma)
    (fun zeta sigma x ->
      Log_space.of_float (Randist.lognormal_pdf x ~zeta ~sigma))
    zeta
    sigma

let chi_squared ~nu =
  dist1
    (fun nu state -> Randist.chisq state ~nu)
    (fun nu x -> Log_space.of_float (Randist.chisq_pdf x ~nu))
    nu

let mixture coeffs (dists : 'a t array) =
  let dists =
    Array.map
      (function Stateless d -> d | Kernel _ -> invalid_arg "mixture")
      dists
  in
  if Array.length coeffs <> Array.length dists then invalid_arg "mixture" ;
  if Array.length coeffs = 0 then invalid_arg "mixture" ;
  let log_coeffs = Array.map Log_space.of_float coeffs in
  let sampler = Randist.discrete_preproc coeffs in
  let log_pdf x =
    let open Log_space in
    let acc = ref one in
    for i = 0 to Array.length dists - 1 do
      acc := mul !acc (mul log_coeffs.(i) (dists.(i).ll x))
    done ;
    !acc
  in
  let sampler rng_state =
    let case = Randist.discrete (rng_of_lxm rng_state) sampler in
    dists.(case).sample rng_state
  in
  stateless sampler log_pdf
