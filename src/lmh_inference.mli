(** Lightweight Metropolis-Hastings *)

include Intf.S with type 'a dist = 'a Dist.t

(** {2 Statistics on the LMH sampler} *)
module MCMC_stats : sig
  (** [reset ()] resets the number of accepted samples to 0. *)
  val reset : unit -> unit

  (** [set_window_size wsize] sets the size of the sliding window over which statistics are accumulated.

      @raise Invalid_argument if [wsize <= 0]. *)
  val set_window_size : int -> unit

  (** [acceptance_rate ()] returns the proportion of samples that were accepted over the sliding window. *)
  val acceptance_rate : unit -> float
end

(** Samples from the posterior described by a computation. Note that
    calling [stream_samples] when building a model is incorrect.
    Note also that each call to [stream_samples m rng_state] resets the
    statistics kept in [MCMC_stats]. *)
val stream_samples : 'a t -> RNG.t -> 'a Seq.t
