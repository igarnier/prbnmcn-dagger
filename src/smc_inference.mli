(** Sequential Monte-Carlo *)

include Intf.S

(** [fork n] creates [n-1] particles. [fork 1] does not create any new particle.

    @raise [Invalid_arg] if [n < 1]. *)
val fork : int -> unit t

(** [get_score] returns the score of the current particle. *)
val get_score : Log_space.t t

(** [yield] signals the scheduler that the particle is ready for resampling. *)
val yield : unit t

(** [log_score_noyield] behaves as {!log_score} but doesn't yield. *)
val log_score_noyield : Log_space.t -> unit t

(** [score_noyield] behaves as {!score} but doesn't yield. *)
val score_noyield : float -> unit t

(** [map_log_score_noyield] behaves as {!map_log_score} but doesn't yield. *)
val map_log_score_noyield : 'a t -> ('a -> Log_space.t) -> 'a t

(** [map_score_noyield] behaves as {!map_score} but doesn't yield. *)
val map_score_noyield : 'a t -> ('a -> float) -> 'a t

(** [resampling] is used to improve the statistical quality of a population. See module {!Resampling}.
    By default, [systematic_resampling] is a reasonable choice. *)
type resampling_strategy

val systematic_resampling : resampling_strategy

val stratified_resampling : resampling_strategy

exception Invalid_population

(** The [Interruptible] module exposes an implementation of Sequential Monte-Carlo that
    gives back the hand to the user after each resampling. *)
module Interruptible : sig
  type 'a resumption =
    | Finished of ('a * Log_space.t) list
    | Suspended of (?resampling:resampling_strategy -> int -> 'a resumption)

  (** [run npart model rng_state] runs sequential Monte-Carlo with
      [npart] initial particles. [resampling] defaults to [systematic_resampling].

      @raise Invalid_population if the total mass of the population becomes equal to {!Log_space.zero} *)
  val run :
    ?resampling:resampling_strategy -> int -> 'a t -> RNG.t -> 'a resumption
end

(** The [Non_interruptible] module exposes an implementation of Sequential Monte-Carlo
    that terminates when {e all} particles terminate. Only use when the probabilistic program
    terminates almost surely! *)
module Non_interruptible : sig
  (** [run ?resampling npart model rng_state] runs sequential Monte-Carlo with
      [npart] initial particles. [resampling] defaults to [systematic_resampling].

      @raise Invalid_population if the total mass of the population becomes equal to {!Log_space.zero} *)
  val run :
    ?resampling:resampling_strategy ->
    int ->
    'a t ->
    RNG.t ->
    ('a * Log_space.t) list
end
