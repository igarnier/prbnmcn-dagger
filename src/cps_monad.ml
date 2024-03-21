type 'a effect = ..

module Make
    (M : Intf.Core) (R : sig
      type 'a t
    end) =
struct
  type ('r, 'a) cont =
    { cont : 'b. ('a -> 'r -> 'b R.t -> 'b M.t) -> 'r -> 'b R.t -> 'b M.t }
  [@@unboxed]

  type ('r, 'a) m = handler:'r handler -> ('r, 'a M.t) cont

  and 'r handler =
    { handler :
        'a 'b.
        'a effect ->
        ('a M.t -> 'r -> 'b R.t -> 'b M.t) ->
        'r ->
        'b R.t ->
        'b M.t
    }
  [@@unboxed]

  let return : type a r. a -> (r, a) m =
   fun x ~handler:_ -> { cont = (fun k w s -> k (M.return x) w s) }

  let if_ cond body ~handler =
    { cont =
        (fun k w s ->
          (cond ~handler).cont
            (fun cond w s -> M.if_ cond (fun c -> (body c ~handler).cont k w s))
            w
            s)
    }

  let map (type a b r) (m : (r, a) m) (f : a -> b) : (r, b) m =
   fun ~handler ->
    { cont =
        (fun k w s -> (m ~handler).cont (fun m w s -> k (M.map m f) w s) w s)
    }

  let map2 (type a b c r) (m : (r, a) m) (m' : (r, b) m) (f : a -> b -> c) :
      (r, c) m =
   fun ~handler ->
    { cont =
        (fun k w s ->
          (m ~handler).cont
            (fun m w s ->
              (m' ~handler).cont (fun m' w s -> k (M.map2 m m' f) w s) w s)
            w
            s)
    }

  let map_array (type a b r) (m : (r, a) m array) (f : a array -> b) : (r, b) m
      =
   fun ~handler ->
    { cont =
        (fun k w s ->
          Array.fold_left
            (fun k (m : (r, a) m) (acc, w) ->
              (m ~handler).cont (fun m w -> k (m :: acc, w)) w)
            (fun (l, w) -> k (M.map_array (Array.of_list l) f) w)
            m
            ([], w)
            s)
    }

  let bind (m : ('r, 'a) m) (f : 'a -> ('r, 'b) m) : ('r, 'b) m =
   fun ~handler ->
    { cont =
        (fun k w s ->
          (m ~handler).cont
            (fun m w s -> M.bind m (fun m -> (f m ~handler).cont k w s))
            w
            s)
    }

  let handle effect ~handler = { cont = (fun k -> handler.handler effect k) }

  module Infix = struct
    let ( let* ) = bind

    let ( let+ ) = map

    let ( and+ ) m m' = map2 m m' (fun x y -> (x, y))

    let ( >|= ) = map

    let ( >>= ) = bind

    let return = return
  end
end
[@@inline]
