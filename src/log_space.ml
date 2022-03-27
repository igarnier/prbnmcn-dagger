type t = float

let zero = neg_infinity

let one = 0.0

let mul = ( +. )

let div = ( -. )

let of_float = log

let to_float = exp

let unsafe_cast = Fun.id

let min = Float.min

let lt (x : float) (y : float) = x < y

let compare = Float.compare

let equal = Float.equal

let hash = Hashtbl.hash

let pp fmtr l = Format.fprintf fmtr "%f [log=%f]" (to_float l) l
