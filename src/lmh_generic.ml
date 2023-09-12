module Make
    (M : Intf.Core) (R : sig
      type 'a t
    end) : sig
  type ('r, 'a) t := ('r, 'a) Cps_monad.Make(M)(R).m

  type 'a Cps_monad.effect +=
    | Dist of 'a Dist.t M.t
    | Score of ('a * Log_space.t) M.t

  val log_score : Log_space.t -> ('r, unit) t

  val score : float -> ('r, unit) t

  val map_log_score : ('r, 'a) t -> ('a -> Log_space.t) -> ('r, 'a) t

  val map_score : ('r, 'a) t -> ('a -> float) -> ('r, 'a) t

  val samplei : ('r, 'a Dist.t) t -> ('r, 'a) t

  val sample : 'a Dist.t -> ('r, 'a) t
end = struct
  type ('r, 'a) t = ('r, 'a) Cps_monad.Make(M)(R).m

  type 'a Cps_monad.effect +=
    | Dist of 'a Dist.t M.t
    | Score of ('a * Log_space.t) M.t

  let log_score : Log_space.t -> ('r, unit) t =
   fun s ~handler ->
    { cont = (fun k -> handler.handler (Score (M.return ((), s))) k) }

  let score s = log_score (Log_space.of_float s)

  let map_log_score : type a r. (r, a) t -> (a -> Log_space.t) -> (r, a) t =
   fun m f ~handler ->
    { cont =
        (fun k ->
          (m ~handler).cont @@ fun m ->
          let s = M.map m (fun x -> (x, f x)) in
          handler.handler (Score s) k)
    }

  let map_score m f = map_log_score m (fun x -> Log_space.of_float (f x))

  let samplei (d : ('r, 'a Dist.t) t) : ('r, _) t =
   fun ~handler ->
    { cont = (fun k -> (d ~handler).cont @@ fun d -> handler.handler (Dist d) k)
    }

  let sample (d : 'a Dist.t) : ('r, _) t =
   fun ~handler -> { cont = (fun k -> handler.handler (Dist (M.return d)) k) }
end
