module Make (M : Intf.Core) = struct
  type 'a t = ('a * Log_space.t) list M.t

  let return x = M.return [(x, Log_space.one)]

  let bind : type a b. a t -> (a -> b t) -> b t =
   fun m f ->
    let open M.Infix in
    let* xs = m in
    List.fold_left
      (fun acc (x, w) ->
        let+ acc = acc and+ ys = f x in
        let ys = List.map (fun (y, w') -> (y, Log_space.mul w w')) ys in
        List.rev_append ys acc)
      (return [])
      xs

  let map : type a b. a t -> (a -> b) -> b t =
   fun m f -> M.map m (fun xs -> List.map (fun (x, w) -> (f x, w)) xs)

  let fold_cartesian f l1 l2 acc =
    let rec loop f l1 l2 l2' acc =
      match l1 with
      | [] -> acc
      | x1 :: tl1 -> (
          match l2' with
          | [] -> loop f tl1 l2 l2 acc
          | x2 :: tl2 ->
              let acc = f x1 x2 acc in
              loop f l1 l2 tl2 acc)
    in
    loop f l1 l2 l2 acc

  let map2 : type a b c. a t -> b t -> (a -> b -> c) -> c t =
   fun m m' f ->
    M.map2 m m' (fun xs ys ->
        fold_cartesian
          (fun (x, w) (y, w') acc -> (f x y, Log_space.mul w w') :: acc)
          xs
          ys
          [])

  (* let fold_nary_cartesian f xss acc =
   *   let rec loop f xss word acc =
   *     match xss with
   *     | [] -> f (List.rev word) acc
   *     | xs :: tl ->
   *         List.fold_left (fun acc x -> loop f tl (x :: word) acc) acc xs
   *   in
   *   loop f xss [] acc *)

  (* let fold_nary_cartesian_arr f xss acc =
   *   let len = Array.length xss in
   *   let rec loop f i word acc =
   *     if i < 0 then f word acc
   *     else
   *       let xs = xss.(i) in
   *       List.fold_left (fun acc x -> loop f (i - 1) (x :: word) acc) acc xs
   *   in
   *   loop f (len - 1) [] acc *)

  let map_array : type a b. a t array -> (a array -> b) -> b t =
   fun m f ->
    let fold_nary_cartesian_arr (xss : (a * Log_space.t) list array) acc =
      let len = Array.length xss in
      let rec loop i word word_weight acc =
        if i < 0 then (f (Array.of_list word), word_weight) :: acc
        else
          let xs = xss.(i) in
          List.fold_left
            (fun acc (x, w) ->
              loop (i - 1) (x :: word) (Log_space.mul w word_weight) acc)
            acc
            xs
      in
      loop (len - 1) [] Log_space.one acc
    in
    M.map_array m (fun xsa -> fold_nary_cartesian_arr xsa [])

  let if_ : type a. bool t -> (bool -> a t) -> a t =
   fun cond body -> bind cond body

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
