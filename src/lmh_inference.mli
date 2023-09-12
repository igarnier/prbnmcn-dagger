(** Lightweight Metropolis-Hastings *)

include Intf.S

(** Samples from the posterior described by a computation. Note that
    calling [stream_samples] when building a model is incorrect. *)
val stream_samples : 'a t -> RNG.t -> 'a Seq.t
