(** Test resampling (used in SMC) *)

open Dagger

(** {2 Helpers} *)

let initial_state =
  [| 0x1337533D;
     71287309;
     666932349;
     719132214;
     461480042;
     387006837;
     443018964;
     450865457;
     901711679;
     833353016;
     397060904;
     811875353
  |]

let state = RNG.make (Array.copy initial_state)

let pp_arr pp fmtr arr =
  let open Format in
  let xs = Array.to_list arr in
  pp_print_list ~pp_sep:(fun fmtr () -> fprintf fmtr ",") pp fmtr xs

module Rat = struct
  include Basic_structures.Basic_impl.Reals.Rational

  let pp fmtr q = Format.fprintf fmtr "%f" (Q.to_float q)
end

(** Instantiate resampling over the field of (arbitrary precision) rationals *)
let uniform x =
  let f = Q.to_float x in
  fun state -> Q.of_float (RNG.float state f)

(** Define a Q-valued measure generator for QCheck *)
module Dist = struct
  type 'a t =
    { mutable active : ('a * Q.t) list; mutable suspended : ('a * Q.t) list }

  let pp pp_elt fmtr { active; suspended } =
    let open Format in
    let pp_mes fmtr list =
      pp_print_list
        ~pp_sep:(fun fmtr () -> fprintf fmtr ",")
        (fun fmtr (x, q) -> fprintf fmtr "(%a,%a)" pp_elt x Rat.pp q)
        fmtr
        list
    in
    fprintf fmtr "active=%a, suspended=%a" pp_mes active pp_mes suspended

  (* [simpl] removes duplicates in the support of the measure and
     canonicalizes it.

     We use [Stdlib.compare] which is not pretty. *)
  let simpl : ('a * Q.t) list -> ('a * Q.t) list =
   fun l ->
    let rec loop l =
      match l with
      | [] -> []
      | [_] -> l
      | ((x1, w1) as hd) :: ((x2, w2) :: tl as rest) ->
          if Stdlib.( = ) x1 x2 then loop ((x1, Q.add w1 w2) :: tl)
          else hd :: loop rest
    in
    let l = List.sort (fun (x, _) (y, _) -> Stdlib.compare x y) l in
    loop l

  let gen_list : 'a QCheck.Gen.t -> ('a * Q.t) list QCheck.Gen.t =
   fun gen ->
    let open QCheck.Gen in
    let nonzero = small_nat >|= ( + ) 2 in
    nonzero >>= fun length ->
    list_size (return length) gen >>= fun list ->
    list_size (return length) nonzero >>= fun wlist ->
    let wlist = List.map Q.of_int wlist in
    let total = List.fold_left Q.add Q.zero wlist in
    let plist = List.map (fun w -> Q.(w / total)) wlist in
    return (List.combine list plist)

  let gen : 'a QCheck.Gen.t -> 'a t QCheck.Gen.t =
   fun gen ->
    let open QCheck.Gen in
    gen_list gen >|= fun suspended -> { active = []; suspended }

  let flip { active; suspended } = { suspended = active; active = suspended }

  let copy { active; suspended } = { active; suspended }
end

(* ------------------------------------------------------------------------- *)
(* Check that the measure generator generates normalized measures. *)

let is_generator_normalized =
  QCheck.Test.make
    ~count:1000
    ~name:"is_generator_normalized"
    (QCheck.make
       QCheck.Gen.(Dist.gen small_nat >|= fun x -> List.map snd x.suspended))
    (fun proba ->
      let total = List.fold_left Q.add Q.zero proba in
      if Q.(total <> one) then
        QCheck.Test.fail_reportf
          "expected normalized measure, got %f (measure = %a)"
          (Q.to_float total)
          (Format.pp_print_list
             ~pp_sep:(fun fmtr () -> Format.fprintf fmtr ",")
             (fun fmtr q -> Format.fprintf fmtr "%f" (Q.to_float q)))
          proba ;
      true)

(* ---------------------------------------------------------------- *)

module R =
  Resampling.Make_predefined
    (Rat)
    (struct
      let uniform = uniform
    end)

let environment (type a) (pop : a Dist.t) : (a, Q.t) Resampling.particles =
  (module struct
    type p = a * Q.t

    type o = a

    type r = Q.t

    let get_output _ = None

    let get_score (_, s) = s

    let iter f = List.iter (fun ((_, score) as p) -> f p score) pop.suspended

    let fold f acc =
      List.fold_left
        (fun acc ((_, score) as p) -> f acc p score)
        acc
        pop.suspended

    let append ((a, _) : p) score = pop.active <- (a, score) :: pop.active

    let total () =
      List.fold_left
        (fun acc (_, score) -> Q.add acc score)
        Q.zero
        pop.suspended

    let size () = List.length pop.suspended

    let ess () = Q.zero
  end)

exception Invalid_population

let list_empty = function [] -> true | _ -> false

let resample mu
    (resampling : ('a, Q.t) Resampling.particles -> unit -> RNG.t -> unit)
    rng_state =
  resampling (environment mu) () rng_state

let total_mass (type a) (mu : a Dist.t) =
  let (module E) = environment mu in
  E.total ()

let cardinal (type a) (mu : a Dist.t) =
  let (module E) = environment mu in
  E.size ()

(* ------------------------------------------------------------------------- *)
(* Test stratified resampling. *)

let isum a = Array.fold_left ( + ) 0 a

let iterative_resampling_generic ?(state = state) mu f =
  resample mu (fun env () rng -> R.resampling_generic_iterative f env rng) state

(* iterative implementation *)
let iterative_stratified_resampling ?(state = state) mu =
  resample
    mu
    (R.stratified_resampling
       ~ess_threshold:Q.one
       ~target_size:(List.length mu.suspended))
    state

let iter ?(pp = fun _ _ -> ()) ?(msg = "") f mu0 =
  let total = total_mass mu0 in
  let card = cardinal mu0 in
  let zero_mass_elements =
    List.filter_map
      (fun (x, q) -> if Q.equal q Q.zero then Some x else None)
      (Dist.simpl mu0.suspended)
  in
  let mu1 = Dist.copy mu0 in
  Format.printf "before %a@." (Dist.pp pp) mu1 ;
  f mu1 ;
  Format.printf "after %a@." (Dist.pp pp) mu1 ;
  let mu1 = Dist.flip mu1 in
  let total' = total_mass mu1 in
  let card' = cardinal mu1 in
  if total <> total' then
    QCheck.Test.fail_reportf
      "%s total mass not preserved (%f vs %f)"
      msg
      (Q.to_float total)
      (Q.to_float total')
  else if card <> card' then
    QCheck.Test.fail_reportf
      "%s cardinality not preserved (%a vs %a)"
      msg
      (Dist.pp pp)
      mu0
      (Dist.pp pp)
      mu1
  else
    let has_zero_elements =
      List.exists (fun (x, _) -> List.mem x zero_mass_elements) mu1.suspended
    in
    if has_zero_elements then
      QCheck.Test.fail_reportf
        "%s has zero mass elements (%a)"
        msg
        (Dist.pp pp)
        mu0
        (Dist.pp pp)
        mu1

let test_stratified_on_handcrafted =
  let rand = Q.of_float 0.12 in
  let f i _rng_state = Q.add rand (Q.div (Q.of_int i) (Q.of_int 7)) in
  let resampling mu = iterative_resampling_generic mu f in
  QCheck.Test.make
    ~name:"test_stratified_on_handcrafted"
    ~count:1
    (QCheck.make
       QCheck.Gen.(
         return
           [ (0, Q.zero);
             (1, Q.zero);
             (2, Q.zero);
             (3, Q.zero);
             (4, Q.zero);
             (5, Q.(1 // 3));
             (6, Q.(2 // 3)) ]))
    (fun proba ->
      let measure = { Dist.suspended = proba; active = [] } in
      iter
        ~pp:Format.pp_print_int
        ~msg:"test_stratified_on_handcrafted"
        resampling
        measure ;
      let support = measure.active |> List.map fst in
      let correct = List.for_all (fun i -> List.mem i [5; 6]) support in
      if not correct then
        QCheck.Test.fail_reportf
          "invalid support (measure = %a)"
          (Dist.pp Format.pp_print_int)
          measure ;
      true)

let test_iterative_stratified =
  QCheck.Test.make
    ~count:1000
    ~name:"test_iterative_stratified"
    (QCheck.make QCheck.Gen.(Dist.gen small_nat))
    (fun measure ->
      assert (List.length measure.Dist.suspended >= 2) ;
      iter
        ~pp:Format.pp_print_int
        ~msg:"test_iterative_stratified"
        iterative_stratified_resampling
        measure ;
      true)

(* ------------------------------------------------------------------------- *)

let tests =
  [ is_generator_normalized;
    test_stratified_on_handcrafted;
    test_iterative_stratified ]
