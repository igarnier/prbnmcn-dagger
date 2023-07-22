module type S = sig
  include Intf.S

  type particle_output

  type resampling_state

  val fork : int -> unit t

  val get_score : Log_space.t t

  val set_score : Log_space.t -> unit t

  val yield : particle_output -> resampling_state t

  type 'a population =
    { terminated : ('a * float) array;
      active : (particle_output * float) array;
      total_mass : float
    }

  exception Invalid_population

  type resampling_strategy :=
    (particle_output, float, resampling_state) Resampling.strategy

  val run :
    resampling_strategy ->
    resampling_state ->
    npart:int ->
    'a t ->
    RNG.t ->
    'a population Seq.t

  val run_custom :
    resampling_strategy ->
    resampling_state ->
    npart:int ->
    (int -> 'a t) ->
    RNG.t ->
    'a population Seq.t
end

module Float_field : Intf.Field with type t = float = struct
  type t = float

  let add = ( +. )

  let sub = ( -. )

  let mul = ( *. )

  let div = ( /. )

  let zero = 0.0

  let one = 1.0

  let of_int = float_of_int

  let ( = ) (x : float) (y : float) = x = y [@@inline]

  let ( < ) (x : float) (y : float) = x < y [@@inline]

  let ( <= ) (x : float) (y : float) = x <= y [@@inline]
end

module type Resampling_types_S = sig
  type particle_output

  type resampling_state
end

module Make (Resampling_types : Resampling_types_S) = struct
  type particle_output = Resampling_types.particle_output

  type resampling_state = Resampling_types.resampling_state

  type resampling_strategy =
    (particle_output, float, resampling_state) Resampling.strategy

  type 'a population =
    { terminated : ('a * float) array;
      active : (particle_output * float) array;
      total_mass : float
    }

  exception Invalid_population

  module Vector : sig
    type 'a t

    val empty : unit -> 'a t

    val push : 'a t -> 'a -> unit

    val pop : 'a t -> 'a option

    val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

    val iter : ('a -> unit) -> 'a t -> unit

    val clear : 'a t -> unit

    val is_empty : 'a t -> bool

    val length : 'a t -> int

    val map_to_array : 'a t -> ('a -> 'b) -> 'b array
  end = struct
    module Vec = Vector

    type 'a t = 'a Vec.vector

    let empty () = Vec.create () [@@ocaml.inline]

    let push vec elt = Vec.push vec elt [@@ocaml.inline]

    let pop vec = Vec.pop vec [@@ocaml.inline]

    let fold = Vec.fold

    let iter = Vec.iter

    let clear vec = Vec.clear vec [@@ocaml.inline]

    let is_empty vec = Vec.is_empty vec

    let length vec = Vec.length vec

    (* This variant makes use of unsafe features to not allocate the intermediate array.
       Not a bottleneck, not worth it at the time of writing. Kept for reference. *)

    (* let map_to_array vec f = *)
    (*   let length = Vec.length vec in *)
    (*   let unsafe_past_length = Vec.unsafe_get_array vec in *)
    (*   Array.init length (fun i -> f (Array.unsafe_get unsafe_past_length i)) *)

    let map_to_array vec f = Vec.to_array vec |> Array.map f
  end

  module Population_state : sig
    type 'a active_particle = private
      | Resampled_active_particle :
          { score : Log_space.t;
            cont : resampling_state -> Log_space.t -> 'a t -> 'a
          }
          -> 'a active_particle
      | Forked_particle :
          { score : Log_space.t; cont : unit -> Log_space.t -> 'a t -> 'a }
          -> 'a active_particle

    and 'a t

    val empty :
      target:int ->
      on_new_population:('a population -> unit) ->
      resampling_state:resampling_state ->
      'a t

    (** [next_active pop] returns the next particle in the active queue. *)
    val next_active : 'a t -> 'a active_particle option

    (** [last_resampling_state pop] returns the last resampling output, if any.
        Before the first [yield] synchronization barrier, this returns None. *)
    val last_resampling_state : 'a t -> resampling_state

    (** Appending particles to the state. *)

    val yielded :
      'b t ->
      particle_output ->
      Log_space.t ->
      (resampling_state -> Log_space.t -> 'b t -> 'b) ->
      unit

    val forked :
      'a t -> int -> Log_space.t -> (unit -> Log_space.t -> 'a t -> 'a) -> unit

    val terminated : 'a t -> 'a -> Log_space.t -> unit

    (** Resampling *)

    val resample :
      'a t -> resampling_strategy -> RNG.t -> [ `continue | `terminate ]
  end = struct
    type 'a active_particle =
      | Resampled_active_particle :
          { score : Log_space.t;
            cont : resampling_state -> Log_space.t -> 'a t -> 'a
          }
          -> 'a active_particle
      | Forked_particle :
          { score : Log_space.t; cont : unit -> Log_space.t -> 'a t -> 'a }
          -> 'a active_particle

    and 'a suspended_particle =
      | Yielded_particle :
          { output : particle_output;
            mutable score : Log_space.t;
            cont : resampling_state -> Log_space.t -> 'a t -> 'a
          }
          -> 'a suspended_particle

    and 'a terminated_particle =
      | Terminated_particle :
          { value : 'a; mutable score : Log_space.t }
          -> 'a terminated_particle

    and 'a particle_buffer =
      { mutable suspended : 'a suspended_particle Vector.t;
            (** The suspended particle particles, waiting to be resampled. *)
        mutable terminated : 'a terminated_particle Vector.t;
            (** The terminated particles, waiting to be resampled *)
        mutable active : 'a active_particle Vector.t;
            (** The active particles *)
        mutable max_score : Log_space.t
            (** The maximum score of all particles pushed in [suspended] or [terminated]. *)
      }

    and 'a t =
      { mutable front : 'a particle_buffer;
        mutable back : 'a particle_buffer;
        mutable resampling_state : resampling_state;
            (** The last output of the resampling step. *)
        on_new_population : 'a population -> unit;
            (** User callback, called on each resampling. *)
        target : int
      }

    module Buffer = struct
      let empty () =
        { suspended = Vector.empty ();
          active = Vector.empty ();
          terminated = Vector.empty ();
          max_score = Log_space.zero
        }

      let clear buf =
        Vector.clear buf.suspended ;
        Vector.clear buf.active ;
        Vector.clear buf.terminated ;
        buf.max_score <- Log_space.zero

      let push_yielded buf output score cont =
        buf.max_score <- Log_space.max buf.max_score score ;
        Vector.push buf.suspended (Yielded_particle { output; score; cont })
        [@@ocaml.inline]

      let push_forked buf n score cont =
        assert (n >= 1) ;
        (* NB: since we push to active, we don't update max_score. *)
        (* Preserve total mass, divide the score by [n] *)
        let score = Log_space.div score (Log_space.of_float (float_of_int n)) in
        let cont = Forked_particle { score; cont } in
        for _i = 1 to n do
          Vector.push buf.active cont
        done
        [@@ocaml.inline]

      let push_resampled_active buf score cont =
        (* NB: since we push to active, we don't update max_score. *)
        Vector.push buf.active (Resampled_active_particle { score; cont })
        [@@ocaml.inline]

      let push_terminated buf value score =
        buf.max_score <- Log_space.max buf.max_score score ;
        Vector.push buf.terminated (Terminated_particle { value; score })
        [@@ocaml.inline]
    end

    let empty ~target ~on_new_population ~resampling_state =
      if target < 2 then failwith "Population_state.empty" ;
      { front = Buffer.empty ();
        back = Buffer.empty ();
        on_new_population;
        resampling_state;
        target
      }

    let last_resampling_state pop = pop.resampling_state

    let yielded pop output score cont =
      Buffer.push_yielded pop.front output score cont
      [@@ocaml.inline]

    let forked pop n score cont = Buffer.push_forked pop.front n score cont
      [@@ocaml.inline]

    let terminated pop value score =
      Buffer.push_terminated pop.front value score
      [@@ocaml.inline]

    let rescale buf =
      assert (Vector.is_empty buf.active) ;
      let max = buf.max_score in
      (* We divide by the maximum in log-space to bring back the weights closer to log(1)=0
         The goal is to avoid using [exp] on numbers with big absolute value.

         We use an ugly hack: since [Log_space.t] = float and we don't want to waste this
         precious space, we store non-log-space values in the [score] field.
         This is used during the execution of the [Particles].
      *)
      let sum = ref 0.0 in
      let sum_of_squares = ref 0.0 in
      Vector.iter
        (fun (Yielded_particle p) ->
          let score = Log_space.div p.score max |> Log_space.to_float in
          p.score <- Log_space.unsafe_cast score ;
          sum_of_squares := !sum_of_squares +. (score *. score) ;
          sum := !sum +. score)
        buf.suspended ;
      Vector.iter
        (fun (Terminated_particle p) ->
          let score = Log_space.div p.score max |> Log_space.to_float in
          p.score <- Log_space.unsafe_cast score ;
          sum_of_squares := !sum_of_squares +. (score *. score) ;
          sum := !sum +. score)
        buf.terminated ;
      (!sum, !sum_of_squares)

    let next_active pop = Vector.pop pop.front.active [@@ocaml.inline]

    let needs_resampling pop = not (Vector.is_empty pop.suspended)
      [@@ocaml.inline]

    let particles (type a) total_mass ess (pop : a t) :
        (particle_output, float) Resampling.particles =
      (module struct
        type p =
          | Suspended of a suspended_particle
          | Terminated of a terminated_particle

        type o = particle_output

        type r = float

        let get_output (p : p) =
          match p with
          | Suspended (Yielded_particle { output; _ }) -> Some output
          | Terminated _ -> None

        let get_score (p : p) =
          match p with
          | Suspended (Yielded_particle { score; _ }) -> (score :> float)
          | Terminated (Terminated_particle { score; _ }) -> (score :> float)

        let iter f =
          (* Note that the [score] field is of type {Log_space.t} but holds
             weights that were converted out of {Log_space.t} in
             the {rescale} function.

             TODO: find a better way to handle the scores. e.g.:
             - have a dedicated vector for storing the scores of the suspended/terminated particles
             - such particles would only need to store an index into that field
          *)
          Vector.iter
            (fun p ->
              match p with
              | Yielded_particle { score; _ } ->
                  f (Suspended p) (score :> float))
            pop.front.suspended ;
          Vector.iter
            (fun p ->
              match p with
              | Terminated_particle { score; _ } ->
                  f (Terminated p) (score :> float))
            pop.front.terminated

        let fold f acc =
          (* Note that the [score] field is of type {Log_space.t} but holds
             weights that were converted out of {Log_space.t} in
             the {rescale} function.

             TODO: find a better way to handle the scores. e.g.:
             - have a dedicated vector for storing the scores of the suspended/terminated particles
             - such particles would only need to store an index into that field
          *)
          let acc =
            Vector.fold
              (fun acc p ->
                match p with
                | Yielded_particle { score; _ } ->
                    f acc (Suspended p) (score :> float))
              acc
              pop.front.suspended
          in
          Vector.fold
            (fun acc p ->
              match p with
              | Terminated_particle { score; _ } ->
                  f acc (Terminated p) (score :> float))
            acc
            pop.front.terminated

        let append (p : p) score =
          let score = Log_space.of_float score in
          match p with
          | Suspended (Yielded_particle { output = _; score = _; cont }) ->
              Buffer.push_resampled_active pop.back score cont
          | Terminated (Terminated_particle { score = _; value }) ->
              (* Note that terminated particle stay in [terminated]. *)
              Buffer.push_terminated pop.back value score

        let total () = total_mass

        let size () = Vector.length pop.front.suspended

        let ess () = ess
      end)

    let wakeup_suspended buf =
      buf.max_score <- Log_space.zero ;
      Vector.iter
        (fun p ->
          match p with
          | Yielded_particle { score; cont; output = _ } ->
              let score = Log_space.of_float (score :> float) in
              Buffer.push_resampled_active buf score cont)
        buf.suspended ;
      Vector.clear buf.suspended ;
      (* Terminated particles don't need to be moved!
         Our hack of storing non-log-space scores in the [score] field shows its limits:
         here if we had the original value, we wouldn't need to do anything.
      *)
      Vector.iter
        (fun p ->
          match p with
          | Terminated_particle payload ->
              let score = Log_space.of_float (payload.score :> float) in
              buf.max_score <- Log_space.max buf.max_score score ;
              payload.score <- score)
        buf.terminated

    let resample pop resampling rng_state =
      assert (Vector.is_empty pop.front.active) ;

      let (total_mass, sum_of_squares) = rescale pop.front in

      if total_mass <= 0.0 then raise Invalid_population ;

      if needs_resampling pop.front then (
        (* 1. Produce population. *)
        let active =
          Vector.map_to_array pop.front.suspended (function
              | Yielded_particle { output; score; _ } ->
              (output, (score :> float)))
        in
        let terminated =
          Vector.map_to_array pop.front.terminated (function
              | Terminated_particle { value; score } ->
              (value, (score :> float)))
        in
        let outcome = { active; terminated; total_mass } in

        (* 2. Give the hand back to the scheduler *)
        pop.on_new_population outcome ;

        (* 3. Perform resampling *)
        Buffer.clear pop.back ;

        let ess = total_mass *. total_mass /. sum_of_squares in

        let p = particles total_mass ess pop in

        let resampling_state =
          resampling ~target_size:pop.target p pop.resampling_state rng_state
        in
        pop.resampling_state <- resampling_state ;

        let resampling_happened =
          (* [true] iff user called [append], i.e. nontrivial resampling *)
          Vector.length pop.back.active <> 0
          || Vector.length pop.back.terminated <> 0
        in

        if resampling_happened then (
          (* Invariant: [resampling] never pushes [Forked_particles] in the active queue. *)
          let tmp = pop.back in
          pop.back <- pop.front ;
          pop.front <- tmp ;
          if Vector.is_empty pop.front.active then raise Invalid_population
          else `continue)
        else (
          (* No resampling. Transfer all suspended particles back to active. *)
          wakeup_suspended pop.front ;
          `continue))
      else
        let terminated =
          Vector.map_to_array pop.front.terminated (function
              | Terminated_particle { value; score } ->
              (value, (score :> float)))
        in
        pop.on_new_population { active = [||]; terminated; total_mass } ;
        `terminate
  end

  module Cps = Cps_monad.Make (Identity_monad) (Population_state)

  module Syntax = struct
    type 'a t = (Log_space.t, 'a) Cps.m

    type 'a Cps_monad.effect +=
      | Dist of 'a Dist.t
      | Score of 'a * Log_space.t
      | Yield : particle_output -> resampling_state Cps_monad.effect
      | Fork : int -> unit Cps_monad.effect
      | Get_score : Log_space.t Cps_monad.effect
      | Set_score : Log_space.t -> unit Cps_monad.effect

    let return = Cps.return

    let bind = Cps.bind

    let map = Cps.map

    let map2 = Cps.map2

    let map_array = Cps.map_array

    let if_ = Cps.if_

    let log_score : Log_space.t -> unit t = fun s -> Cps.handle (Score ((), s))

    let score s =
      if s < 0.0 then invalid_arg "score: negative input given" ;
      let ll = Log_space.of_float s in
      log_score ll

    let map_log_score : type a. a t -> (a -> Log_space.t) -> a t =
     fun m f ~handler ->
      { cont =
          (fun k w pop ->
            (m ~handler).cont
              (fun m w pop ->
                let effect = Score (m, f m) in
                handler.handler effect k w pop)
              w
              pop)
      }

    let map_score m f = map_log_score m (fun x -> Log_space.of_float (f x))

    let yield o : resampling_state t = Cps.handle (Yield o)

    let samplei : type a. a Dist.t t -> a t =
     fun d ~handler ->
      { cont =
          (fun k w pop ->
            (d ~handler).cont
              (fun d w pop -> handler.handler (Dist d) k w pop)
              w
              pop)
      }

    let sample (d : 'a Dist.t) : _ t = Cps.handle (Dist d)

    let fork n : unit t =
      if n < 1 then invalid_arg "fork" ;
      Cps.handle (Fork n)

    let get_score : Log_space.t t = Cps.handle Get_score

    let set_score s : unit t = Cps.handle (Set_score s)

    module Infix = struct
      let ( let* ) = bind

      let ( let+ ) = map

      let ( and+ ) m m' = map2 m m' (fun x y -> (x, y))

      let ( >|= ) = map

      let ( >>= ) = bind

      let return = return
    end
  end

  module Runner = struct
    exception Terminated

    let rec handler : resampling_strategy -> RNG.t -> Log_space.t Cps.handler =
      let open Syntax in
      fun resampling rng_state ->
        { handler =
            (fun (type a b)
                 (eff : a Cps_monad.effect)
                 k
                 w
                 (pop : b Population_state.t) ->
              match eff with
              | Dist dist -> (
                  match dist with
                  | Dist.Stateless dist ->
                      let sample = dist.sample rng_state in
                      k sample w pop
                  | Dist.Kernel kern ->
                      let sample = kern.sample kern.start rng_state in
                      k sample w pop)
              | Score (m, w') -> k m (Log_space.mul w w') pop
              | Yield o ->
                  Population_state.yielded pop o w k ;
                  jump_to_next_active resampling rng_state pop
              | Fork n ->
                  Population_state.forked pop n w k ;
                  jump_to_next_active resampling rng_state pop
              | Get_score -> k w w pop
              | Set_score s -> k () s pop
              | _ -> assert false)
        }

    and jump_to_next_active :
        type a. resampling_strategy -> RNG.t -> a Population_state.t -> a =
     fun resampling rng_state pop ->
      match Population_state.next_active pop with
      | None -> perform_resampling resampling rng_state pop
      | Some (Resampled_active_particle { score; cont }) ->
          cont (Population_state.last_resampling_state pop) score pop
      | Some (Forked_particle { score; cont }) -> cont () score pop

    and perform_resampling :
        type a. resampling_strategy -> RNG.t -> a Population_state.t -> a =
     fun resampling rng_state pop ->
      match Population_state.resample pop resampling rng_state with
      | `continue -> jump_to_next_active resampling rng_state pop
      | `terminate -> raise Terminated

    (* Slightly modified version of the yield example in
       https://v2.ocaml.org/manual/effects.html *)
    let invert (type a) ~(iter : (a -> unit) -> unit) : a Seq.t =
      let open Effect in
      let open Deep in
      let module M = struct
        type _ Effect.t += Yield : a -> unit t
      end in
      let yield v = perform (M.Yield v) in
      fun () ->
        match_with
          iter
          yield
          { retc = (fun _ -> Seq.Nil);
            exnc = (function Terminated -> Seq.Nil | e -> raise e);
            effc =
              (fun (type b) (eff : b Effect.t) ->
                match eff with
                | M.Yield v ->
                    Some
                      (fun (k : (b, _) continuation) ->
                        Seq.Cons (v, continue k))
                | _ -> None)
          }

    let run :
        type a.
        resampling_strategy ->
        resampling_state ->
        npart:int ->
        a Syntax.t ->
        RNG.t ->
        a population Seq.t =
     fun resampling resampling_state ~npart:target_pop m rng_state ->
      let iter f =
        let state : a Population_state.t =
          Population_state.empty
            ~target:target_pop
            ~on_new_population:f
            ~resampling_state
        in
        let initial _ w state =
          (m ~handler:(handler resampling rng_state)).cont
            (fun value score state ->
              Population_state.terminated state value score ;
              jump_to_next_active resampling rng_state state)
            w
            state
        in
        Population_state.forked state target_pop Log_space.one initial ;
        ignore (jump_to_next_active resampling rng_state state)
      in
      invert ~iter

    let run_custom :
        type a.
        resampling_strategy ->
        resampling_state ->
        npart:int ->
        (int -> a Syntax.t) ->
        RNG.t ->
        a population Seq.t =
     fun resampling resampling_state ~npart:target_pop m rng_state ->
      let iter f =
        let state : a Population_state.t =
          Population_state.empty
            ~target:target_pop
            ~on_new_population:f
            ~resampling_state
        in
        let initial (m : a Syntax.t) _ w state =
          (m ~handler:(handler resampling rng_state)).cont
            (fun value score state ->
              Population_state.terminated state value score ;
              jump_to_next_active resampling rng_state state)
            w
            state
        in
        for i = 0 to target_pop - 1 do
          let m = m i in
          Population_state.forked state target_pop Log_space.one (initial m)
        done ;
        ignore (jump_to_next_active resampling rng_state state)
      in
      invert ~iter
  end

  let run = Runner.run

  let run_custom = Runner.run_custom

  include Syntax
  module List_ops = Foldable.Make_list (Syntax)
  module Seq_ops = Foldable.Make_seq (Syntax)
  module Array_ops = Foldable.Make_array (Syntax)
end

module Predefined_resamplings =
  Resampling.Make_predefined
    (Float_field)
    (struct
      let uniform x rng_state = RNG.float rng_state x
    end)

let systematic_resampling = Predefined_resamplings.systematic_resampling

let stratified_resampling = Predefined_resamplings.stratified_resampling

module Unit_smc = Make (struct
  type particle_output = unit

  type resampling_state = unit
end)

module Float_smc = Make (struct
  type particle_output = float

  type resampling_state = unit
end)
