module M : Intf.Core with type 'a t = 'a and type 'a res = 'a = struct
  type 'a t = 'a

  type 'a res = 'a

  let return x = x

  let bind m f = f m

  let map m f = f m

  let map2 m1 m2 f = f m1 m2

  let map_array m f = f m

  let if_ c body = body c

  let run = Fun.id

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
