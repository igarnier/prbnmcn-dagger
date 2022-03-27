type 'a t = RNG.t -> 'a

let return x _rng_state = x

let bind m f rng_state =
  let x = m rng_state in
  f x rng_state

let map m f rng_state =
  let x = m rng_state in
  f x

let map2 m m' f rng_state =
  let x = m rng_state in
  let x' = m' rng_state in
  f x x'

let map_array arr f rng_state =
  let arr = Array.map (fun m -> m rng_state) arr in
  f arr

let if_ m body rng_state = bind m body rng_state

module Infix = struct
  let ( let* ) = bind

  let ( let+ ) = map

  let ( and+ ) m m' = map2 m m' (fun x y -> (x, y))

  let ( >>= ) = bind

  let ( >|= ) = map

  let return = return
end
