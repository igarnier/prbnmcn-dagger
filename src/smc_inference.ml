module Cps = Cps_monad.Make (Identity_monad)

type callback =
  | Particle_cont :
      { value : 'b; score : Log_space.t; cont : 'b -> Log_space.t -> 'a }
      -> callback

type state =
  { mutable suspended : callback list;
        (** The list of suspended particle callbacks, waiting to be resampled *)
    mutable active : callback list
        (** The list of particle callbakcs to be executed before the next resampling *)
  }

module Resampling = Resampling.Make_float (struct
  type field = float

  type t = callback

  let weight (Particle_cont { score; _ }) = Log_space.to_float score
end)

type resampling_strategy =
  target:int ->
  (callback -> int -> callback list -> callback list) ->
  callback list ->
  callback list ->
  callback list Stateful_sampling_monad.t

let systematic_resampling = Resampling.systematic_resampling_list

let stratified_resampling = Resampling.stratified_resampling_list

module Syntax = struct
  type 'a t = state -> (Log_space.t, 'a) Cps.m

  type 'a Cps_monad.effect +=
    | Dist of 'a Dist.t
    | Score of 'a * Log_space.t
    | Yield of state * 'a
    | Fork of state * 'a * int
    | Get_score : Log_space.t Cps_monad.effect

  let return : type a. a -> a t = fun x _state -> Cps.return x

  let bind : type a b. a t -> (a -> b t) -> b t =
   fun m f state -> Cps.bind (m state) (fun x -> f x state)

  let map : type a b. a t -> (a -> b) -> b t =
   fun m f state -> Cps.map (m state) f

  let map2 : type a b c. a t -> b t -> (a -> b -> c) -> c t =
   fun m1 m2 f state -> Cps.map2 (m1 state) (m2 state) f

  let map_array : type a b. a t array -> (a array -> b) -> b t =
   fun ms f state -> Cps.map_array (Array.map (fun m -> m state) ms) f

  let if_ = bind

  let log_score_noyield : Log_space.t -> unit t =
   fun s _state ~handler ->
    let effect = Score ((), s) in
    { cont = (fun k w -> handler.handler effect k w) }

  let score_noyield s =
    if s < 0.0 then invalid_arg "score_noyield: negative input given" ;
    let ll = Log_space.of_float s in
    log_score_noyield ll

  let map_log_score_noyield : type a. a t -> (a -> Log_space.t) -> a t =
   fun m f state ~handler ->
    { cont =
        (fun k w ->
          Fun.flip (m state ~handler).cont w @@ fun m w ->
          let effect = Score (m, f m) in
          handler.handler effect k w)
    }

  let map_score_noyield m f =
    map_log_score_noyield m (fun x -> Log_space.of_float (f x))

  let yield : unit t =
   fun state ~handler ->
    { cont = (fun k w -> handler.handler (Yield (state, ())) k w) }

  let samplei : type a. a Dist.t t -> a t =
   fun d state ~handler ->
    { cont =
        (fun k w ->
          Fun.flip (d state ~handler).cont w @@ fun d w ->
          handler.handler (Dist d) k w)
    }

  let sample (d : 'a Dist.t) : _ t =
   fun _state ~handler -> { cont = (fun k w -> handler.handler (Dist d) k w) }

  let fork n : unit t =
   fun state ->
    if n < 1 then invalid_arg "fork" ;
    Cps.handle (Fork (state, (), n))

  let get_score : Log_space.t t = fun _state -> Cps.handle Get_score

  module Infix = struct
    let ( let* ) = bind

    let ( let+ ) = map

    let ( and+ ) m m' = map2 m m' (fun x y -> (x, y))

    let ( >|= ) = map

    let ( >>= ) = bind

    let return = return
  end

  let log_score ll =
    let open Infix in
    let* () = log_score_noyield ll in
    yield

  let score s =
    let open Infix in
    let* () = score_noyield s in
    yield

  let map_log_score m f =
    let open Infix in
    let* res = map_log_score_noyield m f in
    let* () = yield in
    return res

  let map_score m f =
    let open Infix in
    let* res = map_score_noyield m f in
    let* () = yield in
    return res
end

exception Terminated

exception Invalid_population

let cons_copy x n acc =
  let rec loop x n acc = if n = 0 then acc else loop x (n - 1) (x :: acc) in
  if n < 0 then invalid_arg "copy" ;
  loop x n acc

module Non_interruptible = struct
  let rec handler :
      resampling_strategy -> int -> RNG.t -> Log_space.t Cps.handler =
    let open Syntax in
    fun resampling pop_target rng_state ->
      { handler =
          (fun (type a) (eff : a Cps_monad.effect) k w ->
            match eff with
            | Dist dist -> (
                match dist with
                | Dist.Stateless dist ->
                    let sample = dist.sample rng_state in
                    k sample w
                | Dist.Kernel kern ->
                    let sample = kern.sample kern.start rng_state in
                    k sample w)
            | Score (m, w') -> k m (Log_space.mul w w')
            | Yield (state, m) ->
                let cont = Particle_cont { value = m; score = w; cont = k } in
                state.suspended <- cont :: state.suspended ;
                jump_to_next_active resampling pop_target rng_state state
            | Fork (state, m, n) ->
                let cont =
                  Particle_cont { value = m; score = Log_space.one; cont = k }
                in
                state.active <- cons_copy cont n state.active ;
                jump_to_next_active resampling pop_target rng_state state
            | Get_score -> k w w
            | _ -> assert false)
      }

  and jump_to_next_active :
      type a. resampling_strategy -> int -> RNG.t -> state -> a =
   fun resampling pop_target rng_state state ->
    match state.active with
    | [] -> perform_resampling resampling pop_target rng_state state
    | Particle_cont { value; score; cont } :: rest ->
        state.active <- rest ;
        (* safe because k never returns *)
        Obj.magic (cont value score)

  and perform_resampling :
      type a. resampling_strategy -> int -> RNG.t -> state -> a =
   fun resampling pop_target rng_state state ->
    match state.suspended with
    | [] -> raise Terminated
    | _ ->
        assert (state.active = []) ;
        let cons_copy (x : callback) n acc =
          let (Particle_cont { value; score = _; cont }) = x in
          cons_copy (Particle_cont { value; score = Log_space.one; cont }) n acc
          [@@inline]
        in
        let resampled =
          resampling ~target:pop_target cons_copy state.suspended [] rng_state
        in
        if resampled = [] then raise Invalid_population else () ;
        state.active <- resampled ;
        state.suspended <- [] ;
        jump_to_next_active resampling pop_target rng_state state

  let run :
      ?resampling:resampling_strategy ->
      int ->
      'a Syntax.t ->
      RNG.t ->
      ('a * Log_space.t) list =
    let open Syntax in
    fun ?resampling target_pop m rng_state ->
      let resampling = Option.value resampling ~default:systematic_resampling in
      let state = { suspended = []; active = [] } in
      let results = ref [] in
      let m =
        let open Infix in
        let* () = fork target_pop in
        m
      in
      let initial () w =
        (m state ~handler:(handler resampling target_pop rng_state)).cont
          (fun x w ->
            results := (x, w) :: !results ;
            jump_to_next_active resampling target_pop rng_state state)
          w
      in
      state.active <-
        [Particle_cont { value = (); score = Log_space.one; cont = initial }] ;
      try jump_to_next_active resampling target_pop rng_state state
      with Terminated -> !results
end

module Interruptible = struct
  exception Interrupt

  type 'a resumption =
    | Finished of ('a * Log_space.t) list
    | Suspended of (?resampling:resampling_strategy -> int -> 'a resumption)

  let rec handler :
      resampling_strategy -> int -> RNG.t -> Log_space.t Cps.handler =
    let open Syntax in
    fun resampling pop_target rng_state ->
      { handler =
          (fun (type a) (eff : a Cps_monad.effect) k w ->
            match eff with
            | Dist dist -> (
                match dist with
                | Dist.Stateless dist ->
                    let sample = dist.sample rng_state in
                    k sample w
                | Dist.Kernel kern ->
                    let sample = kern.sample kern.start rng_state in
                    k sample w)
            | Score (m, w') -> k m (Log_space.mul w w')
            | Yield (state, m) ->
                let cont = Particle_cont { value = m; score = w; cont = k } in
                state.suspended <- cont :: state.suspended ;
                jump_to_next_active resampling pop_target rng_state state
            | Fork (state, m, n) ->
                let cont =
                  Particle_cont { value = m; score = Log_space.one; cont = k }
                in
                state.active <- cons_copy cont n state.active ;
                jump_to_next_active resampling pop_target rng_state state
            | Get_score -> k w w
            | _ -> assert false)
      }

  and jump_to_next_active :
      type a. resampling_strategy -> int -> RNG.t -> state -> a =
   fun resampling pop_target rng_state state ->
    match state.active with
    | [] -> perform_resampling resampling pop_target rng_state state
    | Particle_cont { value; score; cont } :: rest ->
        state.active <- rest ;
        (* safe because k never returns *)
        Obj.magic (cont value score)

  and perform_resampling :
      type a. resampling_strategy -> int -> RNG.t -> state -> a =
   fun resampling pop_target rng_state state ->
    match state.suspended with
    | [] -> raise Terminated
    | _ ->
        assert (state.active = []) ;
        let cons_copy (x : callback) n acc =
          let (Particle_cont { value; score = _; cont }) = x in
          cons_copy (Particle_cont { value; score = Log_space.one; cont }) n acc
          [@@inline]
        in
        let resampled =
          resampling ~target:pop_target cons_copy state.suspended [] rng_state
        in
        if resampled = [] then raise Invalid_population else () ;
        state.active <- resampled ;
        state.suspended <- [] ;
        raise Interrupt

  let run :
      ?resampling:resampling_strategy ->
      int ->
      'a Syntax.t ->
      RNG.t ->
      'a resumption =
    let open Syntax in
    fun ?resampling target_pop m rng_state ->
      let resampling = Option.value resampling ~default:systematic_resampling in
      let state = { suspended = []; active = [] } in
      let results = ref [] in
      let m =
        let open Infix in
        let* () = fork target_pop in
        m
      in
      let initial () w =
        (m state ~handler:(handler resampling target_pop rng_state)).cont
          (fun x w ->
            results := (x, w) :: !results ;
            jump_to_next_active resampling target_pop rng_state state)
          w
      in
      state.active <-
        [Particle_cont { value = (); score = Log_space.one; cont = initial }] ;
      let rec loop ?resampling target_pop =
        let resampling =
          Option.value resampling ~default:systematic_resampling
        in
        try jump_to_next_active resampling target_pop rng_state state with
        | Terminated -> Finished !results
        | Interrupt -> Suspended loop
      in
      loop target_pop
end

include Syntax
module List_ops = Foldable.Make_list (Syntax)
module Seq_ops = Foldable.Make_seq (Syntax)
module Array_ops = Foldable.Make_array (Syntax)
