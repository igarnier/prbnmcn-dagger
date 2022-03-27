(** Resampling functions.

    Resampling is used to improve the statistical quality of a population of weighted particles.

    Informally, resampling consists in sampling [N] new particles (each with weight [1/N])
    from a given population such that the resulting population is distributed identically
    to the target one (ie is unbiased).

    An straightforward to proceed would be to sample from a multinomial distribution
    with parameters [N] and the normalized weights of the given population. Other schemes
    are used in practice.

    We consider the following schemes:
    {ul {- systematic resampling}
        {- stratified resampling}}

    In the following, we consider an (ordered, even if arbitrarily)
    family of particles [(x_k, w_k)] for [k=1...K].

    The outcome of resampling is a map from each particle [x_k] to a
    replication count [r_k].

    {2 Systematic resampling}

    Let [U] be sampled uniformly in [[0,1/N]]. Consider the partition of the [\[0,1)] interval
    in [N] sub-intervals given by the points [U_i = U + (i-1)/N] for [i=1...N-1].

    Associate to each particle [x_k] its cumulative weight [W_k = w_1 + ... + w_k]
    and let us set [W_0 = 0].

    We set [r_k] to be the cardinal of the set of [U_i] included in [[W_{k-1}, W_k]].

    {2 Stratified resampling}

    For each [i] in [1...N], let [U_i] be sampled uniformly in [[0, 1/N]] and consider
    the family of intervals [(i-1)/N, (i-1)/N + U_i] for [i=1...N].
    We associate to each [i] the particle [x_k] with the lowest [k] such that [W_k]
    verifies [(i-1)/N + U_i < W_k].

    The replication count [r_k] is the number of times [x_k] was selected in this process.

    {2 References}

    A Tutorial on Particle Filtering and Smoothing: Fifteen years lated (Doucet & Johansen)
*)

(** A [Particle] is any abstract type from which a [weight] can be computed. *)
module type Particle = sig
  (** The type of particles. *)
  type t

  (** The type of weights (as the name indicates, we expect it admits the structure of a field). *)
  type field

  (** [weight p] is the weight of particle [p]. *)
  val weight : t -> field
end

(** The module type exposing resampling functions. *)
module type S = sig
  (** The type of weights. *)
  type field

  (** The type of particles. *)
  type particle

  type 'a t := 'a Stateful_sampling_monad.t

  (**/**)

  val resampling_generic_iterative :
    particle array -> (int -> field) -> int array

  val resampling_generic : particle array -> (int -> field t) -> int array t

  val resampling_generic_list :
    (int -> field t) ->
    (particle -> int -> 'a -> 'a) ->
    particle list ->
    'a ->
    'a t

  (**/**)

  (** [stratified_resampling ~target particles] computes an array of resampling
      counts, corresponding to a fresh population.
      [target] is the number of particles to resample and [particles]
      stores the input population.

      @raise Invalid_arg if [target < 2]
  *)
  val stratified_resampling : target:int -> particle array -> int array t

  (** [stratified_resampling_list ~target cons pop acc] does as [stratified_resampling] but
      proceeds on a list of particles instead of an array. The given [cons] and
      function is given for each particle the number of copies to be done
      (which can be 0).

      @raise Invalid_arg if [target < 2]
  *)
  val stratified_resampling_list :
    target:int -> (particle -> int -> 'a -> 'a) -> particle list -> 'a -> 'a t

  (** [systematic_resampling ~target particles] computes an array of resampling
      counts, corresponding to a fresh population.
      [target] is the number of particles to resample and [particles]
      stores the input population.

      @raise Invalid_arg if [target < 2]
  *)
  val systematic_resampling : target:int -> particle array -> int array t

  (** [systematic_resampling_list ~target cons pop acc] does as [systematic_resampling] but
      proceeds on a list of particles instead of an array. The given [cons] and
      function is given for each particle the number of copies to be done
      (which can be 0).

      @raise Invalid_arg if [target < 2]
  *)
  val systematic_resampling_list :
    target:int -> (particle -> int -> 'a -> 'a) -> particle list -> 'a -> 'a t
end

(** The [Make] functor is generic over a field [F] and a uniform
    sampler. Genericity over the underlying field is especially useful for testing,
    using arbitrary-precision rationals.  *)
module Make : functor
  (F : Intf.Field)
  (P : Particle with type field = F.t)
  (Sampler : sig
     val uniform : F.t -> F.t Stateful_sampling_monad.t
   end)
  -> S with type field = F.t and type particle = P.t

module Float_field : Intf.Field with type t = float

(** [Make_float] instantiates {!Make} over the field of floats. *)
module Make_float : functor (P : Particle with type field = float) ->
  S with type field = float and type particle = P.t
