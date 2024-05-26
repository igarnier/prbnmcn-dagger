(** Log-space computation *)

type t = private float

val zero : t

val one : t

val mul : t -> t -> t

val div : t -> t -> t

val min : t -> t -> t

val max : t -> t -> t

val of_float : float -> t

val to_float : t -> float

val unsafe_cast : float -> t

val compare : t -> t -> int

val lt : t -> t -> bool

val equal : t -> t -> bool

val hash : 'a -> int

val pp : Format.formatter -> t -> unit
