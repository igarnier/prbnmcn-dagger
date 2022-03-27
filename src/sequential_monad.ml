module Make (M : Intf.Core) = struct
  type 'a resumption = Terminated of 'a | Suspended of (unit -> 'a t)

  and 'a t = 'a resumption M.t

  let return : type a. a -> a t = fun x -> M.return (Terminated x)

  let rec bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    let open M.Infix in
    let* m = m in
    match m with
    | Terminated x -> f x
    | Suspended s -> return (Suspended (fun () -> bind (s ()) f))

  let rec map (m : 'a t) (f : 'a -> 'b) : 'b t =
    let open M.Infix in
    let+ m = m in
    match m with
    | Terminated x -> Terminated (f x)
    | Suspended s -> Suspended (fun () -> map (s ()) f)

  let rec map2 : type a b c. a t -> b t -> (a -> b -> c) -> c t =
   fun m m' f ->
    let open M.Infix in
    let+ m = m and+ m' = m' in
    match (m, m') with
    | (Terminated x, Terminated y) -> Terminated (f x y)
    | (Terminated _, Suspended sy) ->
        Suspended (fun () -> map2 (return m) (sy ()) f)
    | (Suspended sx, Terminated _) ->
        Suspended (fun () -> map2 (sx ()) (return m') f)
    | (Suspended sx, Suspended sy) ->
        Suspended (fun () -> map2 (sx ()) (sy ()) f)

  let rec map_array : type a b. a t array -> (a array -> b) -> b t =
   fun ms f ->
    M.map_array ms @@ fun ms' ->
    if
      Array.for_all (function Terminated _ -> true | Suspended _ -> false) ms'
    then
      let xs =
        Array.map (function Terminated x -> x | _ -> assert false) ms'
      in
      Terminated (f xs)
    else
      let ms =
        Array.map2
          (fun m' m -> match m' with Terminated _ -> m | Suspended s -> s ())
          ms'
          ms
      in
      Suspended (fun () -> map_array ms f)

  let suspend (x : 'a t) : 'a t = M.return (Suspended (fun () -> x))

  let advance (m : 'a t) : 'a t =
    let open M.Infix in
    let* m' = m in
    match m' with Suspended f -> f () | Terminated _ -> m

  let rec finish (m : 'a t) : 'a M.t =
    let open M.Infix in
    let* m = m in
    match m with Terminated x -> M.return x | Suspended f -> finish (f ())

  let lift m =
    let open M.Infix in
    let* m = m in
    return (Terminated m)

  let if_ = bind

  module Infix = struct
    let ( let* ) = bind

    let ( let+ ) = map

    let ( and+ ) m m' = map2 m m' (fun x y -> (x, y))

    let ( >|= ) = map

    let ( >>= ) = bind

    let return = return
  end
end
