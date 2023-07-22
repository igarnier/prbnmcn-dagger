(** Sequential Monte Carlo

    Sequential Monte Carlo (SMC) represents a probability distribution by
    a family of particles weighted by their scores (the {e population} in SMC parlance).
    The population is evolved by alternating importance sampling steps
    and resampling steps. The goal of resampling steps is to increase the
    statistical quality of the population, e.g. by removing
    zero or low-score particles and "reproducing" high-score ones.

    At any point, one can obtain an approximation of the target distribution
    by normalizing the scores of the population.

    Resampling steps must be {e unbiased}: the resampled population must
    represent the same target distribution as the population before resampling.
    See {!module:Resampling} for the API related to resampling strategies.

    This module provides a convenient API for describing models to be run with
    SMC inference. It lets the user write the importance sampling part of the model
    and the resampling strategy modularly. Resampling points are marked
    using the {{!module-Smc_inference.module-type-S.val-yield}yield} primitive,
    which also acts as a message-passing bridge between each importance sampling step
    and the resampling step that follows it:
    {[
    val yield : particle_output -> resampling_state t
    ]}
    In a nutshell:
    {ul
    {- particles give their current value as argument to {{!module-Smc_inference.module-type-S.val-yield}yield},
       and the outcome of resampling (of type [resampling_state]) is broadcast
       to each particle as the value to which {{!module-Smc_inference.module-type-S.val-yield}yield} evaluates to.}
    {- {{!module-Resampling.type-strategy}Resampling steps} take the outcome of the previous resampling step as argument.}}

    Note: applications that do not require to maintain state across resampling steps can set [resampling_state = unit].
    Non-trivial resampling states are mostly useful for programming advanced SMC constructs.

    The flow of data is subsumed in the following diagram:

    {%html: <img src="http://stengah.free.fr/assets/dagger/smc-diagram.png" style="width:200mm;" /> %}

    From an implementation perspective, SMC inference proceeds by letting particles,
    implemented as lightweight threads, evolve independently until the next resampling
    event. Resampling acts as a synchronization barrier for all particles. The output
    of the inference is a sequence of the scored values carried by the particles just
    before each resampling.

    Have a look at the {{!page-smc_inference_example}examples}.
*)

(** {2 API of sequential Monte Carlo inference} *)

(** The sequential Monte Carlo DSL. *)
module type S = sig
  (** Core language API shared across all inference engines. *)
  include Intf.S

  (** [particle_output] is the type of values emitted by particles at {!yield} points. *)
  type particle_output

  (** [resampling_state] is the type of values produced by resampling at {!yield} points. *)
  type resampling_state

  (** [fork n] creates [n-1] particles. [fork 1] does not create any new particle.

      @raise [Invalid_arg] if [n < 1]. *)
  val fork : int -> unit t

  (** [get_score] returns the score of the current particle. *)
  val get_score : Log_space.t t

  (** [set_score] sets the score of the current particle. This drops the current score
      of the particle: use wisely. *)
  val set_score : Log_space.t -> unit t

  (** [yield o] signals that the particle produced output [o] and is ready for resampling.
      The output [o] is associated to the score of that particle at [yield] time (i.e. just
      before resampling) in the output of the SMC sampler (see type {!type:population}). *)
  val yield : particle_output -> resampling_state t

  (** ['a population] is the type of a population. *)
  type 'a population =
    { terminated : ('a * float) array;
          (** The terminated particles. These carry their return value. *)
      active : (particle_output * float) array;
          (** The active particles. These carry the value given in argument to {!yield}. *)
      total_mass : float
          (** The total mass of the population (including terminated particles). *)
    }

  exception Invalid_population

  type resampling_strategy :=
    (particle_output, float, resampling_state) Resampling.strategy

  (** [run resampling resampling_state ~npart model rng] returns a lazy sequence of populations. The initial
      population has size [npart]. [resampling] corresponds to the resampling strategy. [resampling_state]
      is the initial resampling state.

      The model is evaluated as follows:
      {ol
        {- all particles in the population have initially the same score;}
        {- each particle evolves until it terminates or until it calls [yield o], at which point it is suspended;}
        {- when all particles are either terminated or suspended, [resampling] is executed on all particles (including
           terminated ones), resulting in a freshened population;}
        {- execution resumes in step 2.}
      }

      The output of the algorithm is a sequence of {!type:population}. Each population is composed of {e yielded} particles,
      attached to some value of type [particle_output], and of {e terminated} particles.

      {b Very important note}: the output sequence is ephemeral.

      @raise Invalid_population if the total mass of the population becomes equal to {!Log_space.zero}
  *)
  val run :
    resampling_strategy ->
    resampling_state ->
    npart:int ->
    'a t ->
    RNG.t ->
    'a population Seq.t

  (** [run_custom resampling resampling_state ~npart model rng] returns a lazy sequence of populations.
      See the documentation of {!run} for more details on the meaning of each argument.

      The only difference with {!run} is that [run_custom] starts with a custom initial population,
      obtained by evaluating [model] on the integers from [0] to [npart-1].
  *)
  val run_custom :
    resampling_strategy ->
    resampling_state ->
    npart:int ->
    (int -> 'a t) ->
    RNG.t ->
    'a population Seq.t
end

(** The SMC implementation is parametric in the type of values produced by the
    particles at each {{!module-Smc_inference.module-type-S.val-yield}yield} and in the type of state of resampling. *)
module type Resampling_types_S = sig
  (** The type of values given to {{!module-Smc_inference.module-type-S.val-yield}yield}.  *)
  type particle_output

  (** The type of state on which {{!module-Resampling.type-strategy}resampling steps} operate. *)
  type resampling_state
end

(** [Make] produces an SMC inference module specialized to the types given
    in [Resampling_types]. *)
module Make (Resampling_types : Resampling_types_S) :
  S
    with type particle_output = Resampling_types.particle_output
     and type resampling_state = Resampling_types.resampling_state

(** {2 Predefined instances of [Make]} *)

module Unit_smc : module type of Make (struct
  type particle_output = unit

  type resampling_state = unit
end)

module Float_smc : module type of Make (struct
  type particle_output = float

  type resampling_state = unit
end)

(** {2 Predefined resampling strategies} *)

val systematic_resampling :
  ess_threshold:float -> ('o, float, 's) Resampling.strategy

val stratified_resampling :
  ess_threshold:float -> ('o, float, 's) Resampling.strategy
