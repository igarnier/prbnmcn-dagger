module Incr = Cgraph

module M : Intf.Core with type 'a t = 'a Incr.t and type 'a res = 'a = struct
  type 'a t = 'a Incr.t

  type 'a res = 'a

  let return x = Incr.return x

  let if_ cond body = Incr.if_ cond (body true) (body false)

  let map = Incr.map

  let map2 = Incr.map2

  let map_array = Incr.map_array

  let bind = Incr.bind

  let run x = Incr.get x

  module Infix = struct
    let ( let* ) = bind

    let ( let+ ) = map

    let ( and+ ) m m' = map2 m m' (fun x y -> (x, y))

    let ( >>= ) = bind

    let ( >|= ) = map

    let return = return
  end
end

include M
