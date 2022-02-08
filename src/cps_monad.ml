module Make (M : Intf.Core) = struct
  type 'a cont = { cont : 'b. ('a M.t -> 'b M.t) -> 'b M.t } [@@unboxed]

  type 'a m = 'a M.t

  type 'a t = handler:handler -> 'a cont

  and handler = { handler : 'a. 'a payload -> 'a cont } [@@unboxed]

  and 'a payload = Dist of 'a Dist.t m | Score of 'a m * Log_space.t m

  type 'a res = handler:handler -> 'a M.res

  let return x : 'a t = fun ~handler:_ -> { cont = (fun k -> k (M.return x)) }

  let if_ cond body ~handler =
    { cont =
        (fun k ->
          (cond ~handler).cont @@ fun cond ->
          M.if_ cond (fun c -> (body c ~handler).cont k))
    }

  let map (type a b) (m : a t) (f : a -> b) : b t =
   fun ~handler ->
    { cont = (fun k -> (m ~handler).cont @@ fun m -> k (M.map m f)) }

  let map2 (type a b c) (m : a t) (m' : b t) (f : a -> b -> c) : c t =
   fun ~handler ->
    { cont =
        (fun k ->
          (m ~handler).cont @@ fun m ->
          (m' ~handler).cont @@ fun m' -> k @@ M.map2 m m' f)
    }

  let map_array (type a b) (m : a t array) (f : a array -> b) : b t =
   fun ~handler ->
    { cont =
        (fun k ->
          Array.fold_left
            (fun k m acc -> (m ~handler).cont @@ fun m -> k (m :: acc))
            (fun l -> k @@ M.map_array (Array.of_list (List.rev l)) f)
            m
            [])
    }

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
   fun ~handler ->
    { cont =
        (fun k ->
          (m ~handler).cont @@ fun m ->
          M.bind m @@ fun m -> (f m ~handler).cont k)
    }

  let log_score : Log_space.t -> unit t =
   fun s ~handler ->
    { cont =
        (fun k -> (handler.handler (Score (M.return (), M.return s))).cont k)
    }

  let score s = log_score (Log_space.of_float s)

  let map_log_score : type a. a t -> (a -> Log_space.t) -> a t =
   fun m f ~handler ->
    { cont =
        (fun k ->
          (m ~handler).cont @@ fun m ->
          let s = M.map m f in
          (handler.handler (Score (m, s))).cont k)
    }

  let map_score m f = map_log_score m (fun x -> Log_space.of_float (f x))

  let samplei (d : 'a Dist.t t) ~handler =
    { cont =
        (fun k ->
          (d ~handler).cont @@ fun d -> (handler.handler (Dist d)).cont k)
    }

  let sample (d : 'a Dist.t) ~handler =
    { cont = (fun k -> (handler.handler (Dist (M.return d))).cont k) }

  let run (m : 'a t) : 'a res =
   fun ~handler -> M.run @@ (m ~handler).cont Fun.id

  module Infix = struct
    let ( let* ) = bind

    let ( let+ ) = map

    let ( and+ ) m m' = map2 m m' (fun x y -> (x, y))

    let ( >|= ) = map

    let ( >>= ) = bind

    let ( >>=$ ) = score

    let return = return
  end
end
[@@inline]
