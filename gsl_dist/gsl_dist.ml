module Log_space = Dagger.Log_space

module type GSL_SIG = sig
  module Rng : sig
    type rng_type

    type t

    val default : unit -> rng_type

    val make : rng_type -> t

    val set : t -> nativeint -> unit

    val uniform_int : t -> int -> int
  end

  module Randist : sig
    val flat : Rng.t -> a:float -> b:float -> float

    val flat_pdf : float -> a:float -> b:float -> float

    val bernoulli : Rng.t -> p:float -> int

    val bernoulli_pdf : int -> p:float -> float

    val gaussian : Rng.t -> sigma:float -> float

    val gaussian_pdf : float -> sigma:float -> float

    val gaussian_tail : Rng.t -> a:float -> sigma:float -> float

    val gaussian_tail_pdf : float -> a:float -> sigma:float -> float

    val laplace : Rng.t -> a:float -> float

    val laplace_pdf : float -> a:float -> float

    val exppow : Rng.t -> a:float -> b:float -> float

    val exppow_pdf : float -> a:float -> b:float -> float

    val cauchy : Rng.t -> a:float -> float

    val cauchy_pdf : float -> a:float -> float

    val rayleigh : Rng.t -> sigma:float -> float

    val rayleigh_pdf : float -> sigma:float -> float

    val rayleigh_tail : Rng.t -> a:float -> sigma:float -> float

    val rayleigh_tail_pdf : float -> a:float -> sigma:float -> float

    val landau : Rng.t -> float

    val landau_pdf : float -> float

    val gamma : Rng.t -> a:float -> b:float -> float

    val gamma_pdf : float -> a:float -> b:float -> float

    val weibull : Rng.t -> a:float -> b:float -> float

    val weibull_pdf : float -> a:float -> b:float -> float

    val binomial : Rng.t -> p:float -> n:int -> int

    val binomial_pdf : int -> p:float -> n:int -> float

    val geometric : Rng.t -> p:float -> int

    val geometric_pdf : int -> p:float -> float

    val exponential : Rng.t -> mu:float -> float

    val exponential_pdf : float -> mu:float -> float

    val poisson : Rng.t -> mu:float -> int

    val poisson_pdf : int -> mu:float -> float

    type discrete

    val discrete_preproc : float array -> discrete

    val discrete : Rng.t -> discrete -> int

    val discrete_pdf : int -> discrete -> float

    val beta : Rng.t -> a:float -> b:float -> float

    val beta_pdf : float -> a:float -> b:float -> float

    val dirichlet : Rng.t -> alpha:float array -> theta:float array -> unit

    val dirichlet_pdf : alpha:float array -> theta:float array -> float

    val dirichlet_lnpdf : alpha:float array -> theta:float array -> float

    val lognormal : Rng.t -> zeta:float -> sigma:float -> float

    val lognormal_pdf : float -> zeta:float -> sigma:float -> float

    val chisq : Rng.t -> nu:float -> float

    val chisq_pdf : float -> nu:float -> float
  end
end

module Make (Gsl : GSL_SIG) = struct
  open Gsl

  let gsl_rng = ref (Rng.default ())

  let rng (s : Random.State.t) =
    let rng = Rng.make !gsl_rng in
    let seed = Random.State.nativebits s in
    Rng.set rng seed ;
    rng

  open Dagger.Dist

  let dist0 sampler log_pdf =
    stateless (fun state -> sampler (rng state)) log_pdf
    [@@inline]

  let dist1 sampler log_pdf arg =
    stateless
      (fun rng_state -> sampler arg (rng rng_state))
      (fun x -> log_pdf arg x)
    [@@inline]

  let dist2 sampler log_pdf arg1 arg2 =
    stateless
      (fun rng_state -> sampler arg1 arg2 (rng rng_state))
      (fun x -> log_pdf arg1 arg2 x)
    [@@inline]

  let kernel1 sampler log_pdf start arg =
    kernel
      start
      (fun x rng_state -> sampler arg x (rng rng_state))
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
      let case = Randist.discrete (rng rng_state) sampler in
      dists.(case).sample rng_state
    in
    stateless sampler log_pdf
end
