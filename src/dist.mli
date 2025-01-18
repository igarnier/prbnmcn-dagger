(** Distributions

    A distribution must be sampleable and must be associated to a density w.r.t. some
    base measure (which is kept implicit). *)

(** ['a dist] is the type of distributions on the type ['a]. *)
type 'a dist = private { sample : RNG.t -> 'a; ll : 'a -> Log_space.t }

(** ['a kernel] is the type of kernels (ie random walks) on the type ['a]. *)
type 'a kernel = private
  { start : 'a; sample : 'a -> RNG.t -> 'a; ll : 'a -> 'a -> Log_space.t }

(** A "distribution" as understood by this library is either a kernel or a plain distribution. *)
type 'a t = private Stateless of 'a dist | Kernel of 'a kernel

(** Create a distribution from a sampler and a log-density. *)
val stateless : (RNG.t -> 'a) -> ('a -> Log_space.t) -> 'a t

(** Create a kernel from an initial point, a kernel and its associated log-density. *)
val kernel : 'a -> ('a -> RNG.t -> 'a) -> ('a -> 'a -> Log_space.t) -> 'a t

(** Helpers for parameterized distributions and kernels. *)

val dist0 : (RNG.t -> 'a) -> ('a -> Log_space.t) -> 'a t

val dist1 : ('a -> RNG.t -> 'b) -> ('a -> 'b -> Log_space.t) -> 'a -> 'b t

val dist2 :
  ('a -> 'b -> RNG.t -> 'c) ->
  ('a -> 'b -> 'c -> Log_space.t) ->
  'a ->
  'b ->
  'c t

val kernel1 :
  ('a -> 'b -> RNG.t -> 'b) ->
  ('a -> 'b -> 'b -> Log_space.t) ->
  'b ->
  'a ->
  'b t

(** Combinators on distributions and kernels. *)

(** [iid n dist] constructs the n-fold iid distribution where each component is distributed
    according to [dist]. *)
val iid : int -> 'a t -> 'a array t

(** [conv f g dist] transports a distribution along a pair of maps [f,g].
    [f,g] must both be total. They need not be bijections, though not respecting
    this additional constraint might cause unspecified behaviours for some
    inference algorithms.. *)
val conv : ('a -> 'b) -> ('b -> 'a) -> 'a t -> 'b t
