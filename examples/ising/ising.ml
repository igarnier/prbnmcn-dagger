(* ------------------------------------------------------------------------- *)
(* The functions in this section come from the OCaml Stdlib for
   version >= 4.14. Pasted here for convenience.
   Licensed under the Gnu LGPL 2.1 license. Author: Simon Cruanes.
   Copyright 2017 Institut National de Recherche en Informatique et
   en Automatique. *)
module Seq = CCSeq

let rec take_one_every_k ~n ~k (xs : 'a Seq.t) : 'a Seq.t =
 fun () ->
  if n = 0 then Seq.Nil
  else
    match Seq.drop k xs () with
    | Seq.Nil -> invalid_arg "take_one_every_k"
    | Seq.Cons (elt, rest) -> Seq.Cons (elt, take_one_every_k ~n:(n - 1) ~k rest)

(* ------------------------------------------------------------------------- *)
(* Integrating vector-valued functions against empirical measures presented
   as 'a Seq.t *)

module type Vector_space = sig
  type t

  val zero : t

  val add : t -> t -> t

  val smul : float -> t -> t
end

module Float_array (Dim : sig
  val dim : int
end) : Vector_space with type t = float array = struct
  type t = float array

  let zero = Array.make Dim.dim 0.0

  let add x y = Array.map2 ( +. ) x y

  let smul s x = Array.map (fun xi -> s *. xi) x
end

let integrate (type t) (module V : Vector_space with type t = t) nsamples
    (f : 'a -> t) (xs : 'a Seq.t) =
  let total =
    xs |> Seq.take nsamples |> Seq.map f |> Seq.fold_left V.add V.zero
  in
  V.smul (1. /. float_of_int nsamples) total

(* ------------------------------------------------------------------------- *)
(* Matrix printing *)

module Matrix = struct
  type 'a t = { cols : int; rows : int; mat : col:int -> row:int -> 'a }

  let pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
      =
   fun pp_elt fmtr m ->
    Format.pp_open_tbox fmtr () ;
    (* print header *)
    Format.pp_set_tab fmtr () ;
    Format.fprintf fmtr "   " ;
    for c = 0 to m.cols - 1 do
      Format.pp_set_tab fmtr () ;
      Format.fprintf fmtr "%d  " c
    done ;
    Format.pp_print_tab fmtr () ;
    Format.fprintf fmtr "   " ;
    for _c = 0 to m.cols - 1 do
      Format.pp_print_tab fmtr () ;
      Format.fprintf fmtr "#"
    done ;
    for r = 0 to m.rows - 1 do
      Format.pp_print_tab fmtr () ;
      Format.fprintf fmtr "%d" r ;
      for c = 0 to m.cols - 1 do
        let elt = m.mat ~col:c ~row:r in
        Format.pp_print_tab fmtr () ;
        Format.fprintf fmtr "%a" pp_elt elt
      done
    done ;
    Format.pp_close_tbox fmtr ()
end

(* ------------------------------------------------------------------------- *)
(* Ising model *)

open Dagger
open Lmh_incremental_inference

(* 2d ising model

   - We neglect the potential energy associated to the external magnetic field
     and only consider the pairwise interactions. Let [p] denote the pairwise interaction energy (typically denoted J in descriptions
     of Ising models).
   - We let [m, n] range over site locations. Neighbouring locations [m,n] are denoted [m~n].
   - We let [s_m] denote the value of a site at location [m].
   - We let [\beta = 1/(k_b x T)] with [k_b] the Boltzmann constant and [T] the temperature in Kelvins.

   The Hamiltonian writes: H({s_m}_m) = - \sum_{m~n} p s_m s_n
   At equilibrium and at temperature [t], the states obey the Bolzmann distribution
   P({s_m}_m) = 1/Z exp(- \beta H({s_m}_m))

   Hence the log-likelihood of a state is, up to normalization,
   log(P{s_m}_m) = -\beta H({s_m}_m) - log(Z)

   Ignoring the term due to the partition function, we obtain that each spin pair [s_m, s_n] for [m~n]
   must be associated the score [\beta p s_m s_n] *)

(* We work on a torus *)
let neighbours lattice_size c r =
  let s = (c, r) in
  [(s, ((c + 1) mod lattice_size, r)); (s, (c, (r + 1) mod lattice_size))]

let all_bonds lattice_size =
  let acc = ref [] in
  for i = 0 to lattice_size - 1 do
    for j = 0 to lattice_size - 1 do
      acc := List.rev_append (neighbours lattice_size i j) !acc
    done
  done ;
  List.sort_uniq Stdlib.compare !acc

let all_positions lattice_size =
  let indices = List.init lattice_size Fun.id in
  List.concat_map (fun i -> List.map (fun j -> (i, j)) indices) indices
  |> Array.of_list

let spin =
  Dist.conv
    (fun x -> if x then 1 else -1)
    (fun x -> x > 0)
    (Stats_dist.bernouilli ~bias:0.5)

let sample_spin = sample spin

let boltzmann = 1.380649e-23

let energy per_bond_energy bonds (matrix : int Matrix.t) =
  List.fold_left
    (fun acc ((c1, r1), (c2, r2)) ->
      let site1 = matrix.mat ~col:c1 ~row:r1
      and site2 = matrix.mat ~col:c2 ~row:r2 in
      acc +. (float (site1 * site2) *. per_bond_energy))
    0.0
    bonds

let ising_model ~kelvins ~p ~n =
  let open Infix in
  let beta = 1. /. (boltzmann *. kelvins) in
  let per_bond_energy = beta *. p in
  let bonds = all_bonds n in
  let positions = all_positions n in
  with_shared_array (Array.map (fun _ -> sample_spin) positions)
  @@ fun lattice ->
  let to_index (col, row) = col + (row * n) in
  let matrix =
    let+ bits =
      let positions =
        positions |> Array.map (fun p -> use lattice.(to_index p))
      in
      map_array positions Fun.id
    in
    { Matrix.cols = n;
      rows = n;
      mat = (fun ~col ~row -> bits.(to_index (col, row)))
    }
  in
  map_log_score matrix (fun x ->
      Log_space.unsafe_cast (energy per_bond_energy bonds x))

(* ------------------------------------------------------------------------- *)
(* Simulations *)

let lattice_size = 16

let bond_energy = boltzmann /. sqrt (float_of_int lattice_size)

(* Prediction of the critical temperature for 2d isotropic ising models *)
let tc = 2.2691853 *. bond_energy /. boltzmann

let () = Format.printf "tc = %f@." tc

let generate_samples ~kelvins ~nsamples =
  stream_samples
    (ising_model ~kelvins ~p:bond_energy ~n:lattice_size)
    (RNG.make_self_init ())
  |> Seq.drop (lattice_size * lattice_size) (* burn-in *)
  |> take_one_every_k ~n:nsamples ~k:(lattice_size * lattice_size)

let two_point_product (c1, r1) (c2, r2) matrix =
  let open Matrix in
  let i1 = matrix.mat ~col:c1 ~row:r1 in
  let i2 = matrix.mat ~col:c2 ~row:r2 in
  float_of_int (i1 * i2)

let points lattice_size =
  let half_size = lattice_size / 2 in
  Array.init (half_size - 1) (fun col -> (0, col + 1))

let two_point_correlations points matrix =
  Array.map (fun p -> two_point_product (0, 0) p matrix) points

let estimate_two_point_correlations ~kelvins ~nsamples =
  let points = points lattice_size in
  let dim = Array.length points in
  let module V = Float_array (struct
    let dim = dim
  end) in
  let evaluate_two_point_correlations matrix =
    Array.map (fun p -> two_point_product (0, 0) p matrix) points
  in
  let samples = generate_samples ~kelvins ~nsamples in
  let result =
    integrate (module V) nsamples evaluate_two_point_correlations samples
  in
  Array.mapi
    (fun i correlation ->
      let dist_to_origin = float_of_int (i + 1) in
      (dist_to_origin, correlation))
    result

let estimates =
  Array.init 16 (fun i -> float_of_int (i + 1) *. tc /. 8.)
  |> Array.map (fun kelvins ->
         (kelvins, estimate_two_point_correlations ~kelvins ~nsamples:1_000))

let () =
  let oc = open_out "raw.dat" in
  let fmtr = Format.formatter_of_out_channel oc in
  Array.iter
    (fun (kelvins, correlations) ->
      Array.iter
        (fun (length, corr) ->
          Format.fprintf fmtr "%f %f %f@." kelvins length corr)
        correlations ;
      Format.fprintf fmtr "@.")
    estimates ;
  close_out oc
