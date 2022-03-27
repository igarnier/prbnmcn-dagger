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

module R =
  Resampling.Make
    (Rat)
    (struct
      type field = Q.t

      type t = Q.t

      let weight = Fun.id
    end)
    (struct
      let uniform = uniform
    end)

(** Define a Q-valued measure generator for QCheck *)
module Dist = struct
  type 'a t = ('a * Q.t) list

  (* [simpl] removes duplicates in the support of the measure and
     canonicalizes it.

     We use [Stdlib.compare] which is not pretty. *)
  let simpl : 'a t -> 'a t =
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

  (* The dirac natural transformation *)
  let return : 'a. 'a -> 'a t = fun x -> [(x, Q.one)]

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    List.concat_map
      (fun (x, w) ->
        let l = f x in
        List.map (fun (y, w') -> (y, Q.mul w w')) l)
      m
    |> simpl

  let ( let* ) = bind

  let gen : 'a QCheck.Gen.t -> 'a t QCheck.Gen.t =
   fun gen ->
    let open QCheck.Gen in
    let nonzero = small_nat >|= ( + ) 1 in
    nonzero >>= fun length ->
    list_size (return length) gen >>= fun list ->
    list_size (return length) nonzero >>= fun wlist ->
    let wlist = List.map Q.of_int wlist in
    let total = List.fold_left Q.add Q.zero wlist in
    let plist = List.map (fun w -> Q.(w / total)) wlist in
    return (simpl (List.combine list plist))
end

(* ------------------------------------------------------------------------- *)
(* Check that the measure generator generates normalized measures. *)

let is_generator_normalized =
  QCheck.Test.make
    ~count:1000
    ~name:"is_generator_normalized"
    (QCheck.make QCheck.Gen.(Dist.gen small_nat >|= List.map snd))
    (fun proba ->
      let total = List.fold_left Q.add Q.zero proba in
      if Q.(total <> one) then
        QCheck.Test.fail_reportf
          "expected noramlized measure, got %f (measure = %a)"
          (Q.to_float total)
          (Format.pp_print_list
             ~pp_sep:(fun fmtr () -> Format.fprintf fmtr ",")
             (fun fmtr q -> Format.fprintf fmtr "%f" (Q.to_float q)))
          proba ;
      true)

(* ------------------------------------------------------------------------- *)
(* Test stratified resampling. *)

let isum a = Array.fold_left ( + ) 0 a

(* iterative implementation *)
let iterative_stratified_resampling ?(state = state) ?(target = 5) mu =
  assert (target > 0) ;
  let inv = 1. /. float_of_int target in
  R.resampling_generic_iterative mu (fun i ->
      let rand = RNG.float state inv in
      Q.add Q.(i // target) (Q.of_float rand))

let test_stratified_on_handcrafted =
  let rand = Q.of_float 0.18 in
  let f i = Q.add rand (Q.div (Q.of_int i) (Q.of_int 5)) in
  let resampling mu = R.resampling_generic_iterative mu f in
  QCheck.Test.make
    ~name:"test_stratified_on_handcrafted"
    ~count:1
    (QCheck.make
       QCheck.Gen.(
         return [Q.zero; Q.zero; Q.zero; Q.zero; Q.zero; Q.(1 // 3); Q.(2 // 3)]))
    (fun proba ->
      let result = resampling (Array.of_list proba) in
      let count = isum result in
      if count <> 5 then
        QCheck.Test.fail_reportf
          "count = %d, expected %d (measure = %a)"
          count
          5
          (Format.pp_print_list
             ~pp_sep:(fun fmtr () -> Format.fprintf fmtr ",")
             (fun fmtr q -> Format.fprintf fmtr "%f" (Q.to_float q)))
          proba ;
      true)

let test_iterative_stratified =
  QCheck.Test.make
    ~count:1000
    ~name:"test_iterative_stratified"
    (QCheck.make
       QCheck.Gen.(pair (Dist.gen small_nat >|= List.map snd) (int_range 5 50)))
    (fun (proba, target) ->
      let result =
        iterative_stratified_resampling ~target (Array.of_list proba)
      in
      let count = isum result in
      if count <> target then
        QCheck.Test.fail_reportf
          "count = %d, expected %d (measure = %a)"
          count
          target
          (Format.pp_print_list
             ~pp_sep:(fun fmtr () -> Format.fprintf fmtr ",")
             (fun fmtr q -> Format.fprintf fmtr "%f" (Q.to_float q)))
          proba ;
      true)

let test_monadic_vs_iterative_stratified =
  QCheck.Test.make
    ~count:1000
    ~name:"test_monadic_vs_iterative_stratified"
    (QCheck.make
       QCheck.Gen.(pair (Dist.gen small_nat >|= List.map snd) (int_range 5 50)))
    (fun (proba, target) ->
      let state = RNG.make (Array.copy initial_state) in
      let result1 =
        iterative_stratified_resampling ~state ~target (Array.of_list proba)
      in
      let state = RNG.make (Array.copy initial_state) in
      let result2 =
        R.stratified_resampling ~target (Array.of_list proba) state
      in
      if result1 <> result2 then QCheck.Test.fail_reportf "result1 <> result2" ;
      true)

(* ------------------------------------------------------------------------- *)
(* Test systematic resampling. *)

(* iterative implementation *)
let iterative_systematic_resampling ?(state = state) ?(target = 5) mu =
  assert (target > 0) ;
  let inv = 1. /. float_of_int target in
  let rand = RNG.float state inv in
  R.resampling_generic_iterative mu (fun i ->
      Q.add Q.(i // target) (Q.of_float rand))

let test_iterative_systematic =
  QCheck.Test.make
    ~count:1000
    ~name:"test_iterative_systematic"
    (QCheck.make
       QCheck.Gen.(pair (Dist.gen small_nat >|= List.map snd) (int_range 5 50)))
    (fun (proba, target) ->
      let result =
        iterative_systematic_resampling ~target (Array.of_list proba)
      in
      let count = isum result in
      if count <> target then
        QCheck.Test.fail_reportf
          "count = %d, expected %d (measure = %a)"
          count
          target
          (Format.pp_print_list
             ~pp_sep:(fun fmtr () -> Format.fprintf fmtr ",")
             (fun fmtr q -> Format.fprintf fmtr "%f" (Q.to_float q)))
          proba ;
      true)

let test_monadic_vs_iterative_systematic =
  QCheck.Test.make
    ~count:1000
    ~name:"test_monadic_vs_iterative_systematic"
    (QCheck.make
       QCheck.Gen.(pair (Dist.gen small_nat >|= List.map snd) (int_range 5 50)))
    (fun (proba, target) ->
      let state = RNG.make (Array.copy initial_state) in
      let result1 =
        iterative_systematic_resampling ~state ~target (Array.of_list proba)
      in
      let state = RNG.make (Array.copy initial_state) in
      let result2 =
        R.systematic_resampling ~target (Array.of_list proba) state
      in
      if result1 <> result2 then QCheck.Test.fail_reportf "result1 <> result2" ;
      true)

(* ------------------------------------------------------------------------- *)
(* Test consistency of list vs array resampling algorithms *)

let test_list_vs_array_stratified =
  QCheck.Test.make
    ~count:1000
    ~name:"test_list_vs_array_stratified"
    (QCheck.make
       QCheck.Gen.(pair (Dist.gen small_nat >|= List.map snd) (int_range 5 50)))
    (fun (proba, target) ->
      let state = RNG.make (Array.copy initial_state) in
      let result1 =
        R.stratified_resampling ~target (Array.of_list proba) state
      in
      let state = RNG.make (Array.copy initial_state) in
      let result2 =
        R.stratified_resampling_list
          ~target
          (fun _q n acc -> n :: acc)
          proba
          []
          state
        |> List.rev |> Array.of_list
      in
      if result1 <> result2 then
        QCheck.Test.fail_reportf
          "result1 = %a@.result2 = %a"
          (pp_arr Format.pp_print_int)
          result1
          (pp_arr Format.pp_print_int)
          result2 ;
      true)

let test_list_vs_array_systematic =
  QCheck.Test.make
    ~count:1000
    ~name:"test_list_vs_array_systematic"
    (QCheck.make
       QCheck.Gen.(pair (Dist.gen small_nat >|= List.map snd) (int_range 5 50)))
    (fun (proba, target) ->
      let state = RNG.make (Array.copy initial_state) in
      let result1 =
        R.systematic_resampling ~target (Array.of_list proba) state
      in
      let state = RNG.make (Array.copy initial_state) in
      let result2 =
        R.systematic_resampling_list
          ~target
          (fun _q n acc -> n :: acc)
          proba
          []
          state
        |> List.rev |> Array.of_list
      in
      if result1 <> result2 then
        QCheck.Test.fail_reportf
          "result1 = %a@.result2 = %a"
          (pp_arr Format.pp_print_int)
          result1
          (pp_arr Format.pp_print_int)
          result2 ;
      true)

(* ------------------------------------------------------------------------- *)
(* Test resampling on small arrays. *)

let test_systematic_on_small =
  QCheck.Test.make
    ~name:"test_systematic_on_small"
    ~count:100
    (QCheck.make QCheck.Gen.(pair small_nat (return [Q.(1 // 3); Q.(2 // 3)])))
    (fun (target, proba) ->
      (* 0, 1 are invalid target numbers *)
      let target = target + 2 in
      let state = RNG.make (Array.copy initial_state) in
      let result =
        R.systematic_resampling ~target (Array.of_list proba) state
      in
      let count = isum result in
      if count <> target then
        QCheck.Test.fail_reportf
          "count = %d, expected %d (measure = %a)"
          count
          target
          (Format.pp_print_list
             ~pp_sep:(fun fmtr () -> Format.fprintf fmtr ",")
             (fun fmtr q -> Format.fprintf fmtr "%f" (Q.to_float q)))
          proba ;
      true)

let test_stratified_on_small =
  QCheck.Test.make
    ~name:"test_stratified_on_small"
    ~count:100
    (QCheck.make QCheck.Gen.(pair small_nat (return [Q.(1 // 3); Q.(2 // 3)])))
    (fun (target, proba) ->
      (* 0, 1 are invalid target numbers *)
      let target = target + 2 in
      let state = RNG.make (Array.copy initial_state) in
      let result =
        R.stratified_resampling ~target (Array.of_list proba) state
      in
      let count = isum result in
      if count <> target then
        QCheck.Test.fail_reportf
          "count = %d, expected %d (measure = %a)"
          count
          target
          (Format.pp_print_list
             ~pp_sep:(fun fmtr () -> Format.fprintf fmtr ",")
             (fun fmtr q -> Format.fprintf fmtr "%f" (Q.to_float q)))
          proba ;
      true)

(* ------------------------------------------------------------------------- *)

let tests =
  [ is_generator_normalized;
    test_stratified_on_handcrafted;
    test_iterative_stratified;
    test_monadic_vs_iterative_stratified;
    test_iterative_systematic;
    test_monadic_vs_iterative_systematic;
    test_list_vs_array_stratified;
    test_list_vs_array_systematic;
    test_systematic_on_small;
    test_stratified_on_small ]
