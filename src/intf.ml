(** Module type definitions *)

(** Abstract foldable containers *)
module type Foldable = sig
  type 'a t

  type 'a m

  val fold : ('a -> 'b -> 'a m) -> 'a -> 'b t -> 'a m

  val iter : ('a -> unit m) -> 'a t -> unit m
end

(** Infix operators *)
module type Infix = sig
  type 'a t

  (** Alias to [bind]. *)
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  (** Alias to [map]. *)
  val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t

  (** Alias to [bind]. *)
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  (** Alias to [map]. *)
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  (** Equivalent to [map2 m m' (fun x y -> (x, y))]. *)
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

  (** Convenience reexport of [return]. *)
  val return : 'a -> 'a t
end

module type Core = sig
  (** ['a t] is the type of computations of type ['a] *)
  type 'a t

  (** ['a res] is the outcome of running a computation of type ['a] *)
  type 'a res

  (** [return x] injects a value [x] as a computation *)
  val return : 'a -> 'a t

  (** Monadic bind *)
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  (** Functorial map *)
  val map : 'a t -> ('a -> 'b) -> 'b t

  (** Applicative structure *)
  val map2 : 'a t -> 'b t -> ('a -> 'b -> 'c) -> 'c t

  (** N-array applicative structure *)
  val map_array : 'a t array -> ('a array -> 'b) -> 'b t

  (** If-then-else, mostly useful for monads featuring incremental computation. Allows to
      efficiently bind on a boolean computation. *)
  val if_ : bool t -> (bool -> 'a t) -> 'a t

  (** Running a monadic computation *)
  val run : 'a t -> 'a res

  module Infix : Infix with type 'a t := 'a t
end

(** Module type describing the DSL, implemented by inference backends. *)
module type S = sig
  (** {2 Core constructs of the DSL }*)

  include Core

  (** [sample dist] builds a computation that samples from [dist].
      Note that [dist] must be a pure computation. *)
  val sample : 'a Dist.t -> 'a t

  (** [samplei dist] is similar to [sample] except that [dist] can
      be an impure computation (ie computing the distribution can
      involve sampling from other distributions). *)
  val samplei : 'a Dist.t t -> 'a t

  (** [map_score m f] behaves similarly to [m] except that the
      associated computation will be reweighted according to
      the result of evaluating [f] on the value of [m]. *)
  val map_score : 'a t -> ('a -> float) -> 'a t

  (** Same as [map_score] excepts that a log-space likelihood is expected. *)
  val map_log_score : 'a t -> ('a -> Log_space.t) -> 'a t

  (** [score s] reweights the computation by [s]. *)
  val score : float -> unit t

  (** [log_score] behaves as [score] except that a log-space weight
      is expected. *)
  val log_score : Log_space.t -> unit t

  (** Samples from the posterior described by a computation. Note that
      calling [stream_samples] when building a model is incorrect. *)
  val stream_samples : 'a t -> RNG.t -> 'a Seq.t

  module List_ops : Foldable with type 'a t = 'a list and type 'a m := 'a t

  module Array_ops : Foldable with type 'a t = 'a array and type 'a m := 'a t

  module Seq_ops : Foldable with type 'a t = 'a Seq.t and type 'a m := 'a t
end
