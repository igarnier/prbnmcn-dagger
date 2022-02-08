(** Incremental lightweight Metropolis-Hastings *)

include sig
  include Intf.S

  (** {2 Extra constructs of the incremental backend} *)

  (** The incremental backend can perform inference on models featuring explicit
      sharing of sub-computations. In practice, this allows to construct complex graphical
      models without having to use the (costlier) [bind] of the core language. *)

  (** The type of a shared sub-expression. *)
  type 'a shared

  (** [with_shared m f] allows to access a shareable representation of the computation [m]
      in the scope defined by [f]. *)
  val with_shared : 'a t -> ('a shared -> 'b t) -> 'b t

  (** [with_shared_list m f] is equivalent to folding [with_shared] on a list of computations. *)
  val with_shared_list : 'a t list -> ('a shared list -> 'b t) -> 'b t

  (** [with_shared_array m f] is equivalent to folding [with_shared] on an array of computations. *)
  val with_shared_array : 'a t array -> ('a shared array -> 'b t) -> 'b t

  (** [use shared] reifies a shareable representation of a computation as a bona fide computation of the same type. *)
  val use : 'a shared -> 'a t

  (** [to_dot file model] generates the graph of the [model] in graphviz format
      and saves it to [file]. *)
  val to_dot : string -> 'a t -> unit

  module Infix : sig
    include Intf.Infix with type 'a t := 'a t

    (** Alias to [sharing_bind] *)
    val ( let*! ) : 'a t -> ('a shared -> 'b t) -> 'b t

    (** Convenience re-export of [use]. *)
    val use : 'a shared -> 'a t
  end

  (** {2 Experimental features } *)

  (** [Make_shared] allows to construct [with] operators for any mappable type. *)
  module Make_shared : functor
    (C : sig
       type 'a t

       val map : 'a t -> ('a -> 'b) -> 'b t
     end)
    -> sig
    val with_shared : 'a t C.t -> ('a shared C.t -> 'b t) -> 'b t
  end
end
