(** Resampling functions

    Resampling consists in sampling {m N} new particles from a given population such
    that the resulting population is distributed identically to the initial one (ie is unbiased)
    and such that the statistical quality of the resulting population is improved.

    This module provides an API allowing users to specify their own resampling schemes. It also
    provides some well-known resampling strategies via the {!module-Resampling.module-Make_predefined} functor.

    {2 Example}

    A straightforward strategy is to sample from a multinomial distribution
    with parameters {m N} and the normalized scores of the given population.
    An implementation of corresponding algorithm using the API of this
    module is described {{!page-smc_inference_example}here}.
    Note that better-performing schemes are used in practice.

    {2 API}

    A resampling algorithm takes as input a "current" population of particles
    and must produce the "next" population. The only operations available to
    do so are accessed through a first class module of type {!Particles}.
*)

(** A [Particles] module exposes primitives available to resampling algorithms. *)
module type Particles = sig
  (** [p] is the type of particles *)
  type p

  (** [o] is the type of particle outputs. *)
  type o

  (** [r] is the type of the scores associated to each particle *)
  type r

  (** [iter] allows to iterate on the current population of particles. *)
  val iter : (p -> r -> unit) -> unit

  (** [fold] allows to fold on the current population of particles. *)
  val fold : ('acc -> p -> r -> 'acc) -> 'acc -> 'acc

  (** [get_output p] is the output of the particle at the last [yield] point.
      Returns [None] if the particle has terminated. See the documentation for
      [yield] in {!Smc_inference}. *)
  val get_output : p -> o option

  (** [get_score p] is the unnormalized score of the particle. *)
  val get_score : p -> r

  (** [append p r] appends particle [p] with score [r] to the next population. *)
  val append : p -> r -> unit

  (** [total ()] is the total mass of the current population. *)
  val total : unit -> r

  (** [size ()] is the cardinality of the current population. *)
  val size : unit -> int

  (** [ess ()] is the estimated sample size. Varies between [0] and the cardinality
      of the current population. Computed as {m \left(\sum_{i=1}^N w_i^2\right)^{-1}},
      where {m N} is equal to [size ()] and the {m w_i} are the normalized scores
      of each particle. *)
  val ess : unit -> r
end

(** Implementation of resampling must be generic in the implementation of particles,
    hence we abstract their types away.*)
type ('o, 'r) particles = (module Particles with type o = 'o and type r = 'r)

(** [strategy] is the type of resampling steps.

    - The first argument corresponds to the target size of the population.
    - The second argument represents the {{!particles}particles} on which resampling is to be performed.
    - The third argument is the previous resampling state.
    - The fourth argument is the random number generator state.

    Resampling strategies are called once every resampling step.
*)
type ('o, 'r, 'resampling_state) strategy =
  target_size:int ->
  ('o, 'r) particles ->
  'resampling_state ->
  RNG.t ->
  'resampling_state

(** [Make_predefined (F) (Sampler)] produces implementations of stratified and
    systematic resampling over a field [F] and given a uniform sampler [Sampler].

    {2 Algorithm description}

    A detailed treatment of the following algorithms is given in
    {{:https://www.stats.ox.ac.uk/~doucet/doucet_johansen_tutorialPF2011.pdf}A Tutorial on Particle Filtering and Smoothing: Fifteen years lated (Doucet & Johansen)}
    section 3.4.

    In the following, we consider a family of scored particles {m (x_k, w_k)} for {m k=1 \ldots K}
    with total score {m W=\sum_k w_k}.

    The outcome of the resampling strategies described below is a map from each particle {m x_k} to a
    replication count {m r_k}, such that {m \sum_k r_k = N}. The resulting population will consist in
    {m r_1} copies of {m x_1}, {m r_2} copies of {m x_2}, etc, all with equal
    score {m \frac{1}{W}}.

    {3 Systematic resampling}

    Let {m U} be sampled uniformly in {m [0,\frac{1}{N}]}. Consider the set of points {m U_i = U + \frac{i-1}{N}} for {m i=1 \ldots N-1}.
    Associate to each particle {m x_k} its cumulative score {m W_k = w_1 + ... + w_k}
    and us set {m W_0 = 0}.
    We set {m r_k} to be the cardinal of the set of {m W \cdot U_i} included in {m [W_{k-1}, W_k)}.

    {3 Stratified resampling}

    For each {m i} in {m 1 \ldots N}, let {m U_i} be sampled uniformly in {m [0, \frac{1}{N}]}.
    Associate to each {m i} the particle {m x_k} with the lowest {m k} such that {m \frac{i-1}{N} + U_i < W_k/W}.
    The replication count {m r_k} is the number of times {m x_k} was selected in this process.
 *)
module Make_predefined : functor
  (F : Intf.Field)
  (Sampler : sig
     val uniform : F.t -> RNG.t -> F.t
   end)
  -> sig
  (**/**)

  val resampling_generic_iterative :
    (int -> RNG.t -> F.t) -> ('o, F.t) particles -> RNG.t -> unit

  (**/**)

  (** [stratified_resampling ess_threshold] executes stratified resampling
      if the estimated sample size
      {{!module-Resampling.module-type-Particles.val-ess}P.ess} of the population is below
      [ess_threshold * P.size ()], where [P] is the
      {{!module-Resampling.type-particles}particles} module passed to
      the {{!type:strategy} strategy}.

      @raise Invalid_arg if [P.size < 2], if [target_size < 2] or if [ess_threshold] is not in the unit interval. *)
  val stratified_resampling : ess_threshold:F.t -> ('o, F.t, 's) strategy

  (** [systematic_resampling ess_threshold] executes systematic resampling
      if the estimated sample size
      {{!module-Resampling.module-type-Particles.val-ess}P.ess} of the population is below
      [ess_threshold * P.size ()] where [P] is the
      {{!module-Resampling.type-particles}particles} passed to
      the {{!type:strategy} strategy}.

      @raise Invalid_arg if [P.size < 2], if [target_size < 2] or if [ess_threshold] is not in the unit interval. *)
  val systematic_resampling : ess_threshold:F.t -> ('o, F.t, 's) strategy
end
