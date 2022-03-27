module type Trace = sig
  type t

  val empty : t

  val union : t -> t -> t
end

module Make (M : Intf.Core) (T : Trace) = struct
  type 'a v = { value : 'a; trace : T.t }

  type 'a t = 'a v M.t

  let return x = M.return { value = x; trace = T.empty }

  let bind : type a b. a t -> (a -> b t) -> b t =
   fun m f ->
    M.bind m @@ fun { value = v; trace = t } ->
    M.map (f v) @@ fun { value = v'; trace = t' } ->
    { value = v'; trace = T.union t t' }

  let map : type a b. a t -> (a -> b) -> b t =
   fun m f -> M.map m (fun traced -> { traced with value = f traced.value })

  let map2 : type a b c. a t -> b t -> (a -> b -> c) -> c t =
   fun m m' f ->
    M.map2 m m' (fun t1 t2 ->
        { value = f t1.value t2.value; trace = T.union t1.trace t2.trace })

  let map_array : type a b. a t array -> (a array -> b) -> b t =
   fun m f ->
    M.map_array m (fun ts ->
        let a = Array.map (fun t -> t.value) ts in
        let trace =
          Array.fold_left (fun trace t -> T.union trace t.trace) T.empty ts
        in
        { value = f a; trace })

  let map_list : type a b. a t list -> (a -> b) -> b list t =
   fun list f ->
    let mapped = List.map (fun m -> map m f) list in
    let rec loop mapped =
      match mapped with
      | [] -> return []
      | hd :: tl -> map2 hd (loop tl) (fun h t -> h :: t)
    in
    loop mapped

  let if_ : type a. bool t -> (bool -> a t) -> a t =
   fun cond body ->
    let result : _ M.t =
      M.if_ (M.map cond @@ fun { value = condv; _ } -> condv) body
    in
    M.map2
      cond
      result
      (fun { trace = cond_trace; _ } { value; trace = result_trace } ->
        { value; trace = T.union cond_trace result_trace })

  module Infix = struct
    let ( let* ) = bind

    let ( let+ ) = map

    let ( and+ ) m m' = map2 m m' (fun x y -> (x, y))

    let ( >>= ) = bind

    let ( >|= ) = map

    let return = return
  end
end
[@@inline]
