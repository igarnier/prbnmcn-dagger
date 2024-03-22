(* simulate configurations of a 2D polymer chain, under the influence of a
 * harmonic potential constraining its end point to full extension along the x
 * axis.
 *
 * Because the end point depends strongly on the initial direction, the latter
 * is only very rarely updated. This is a generic limitation in LMH sampling.
 * *)

open Dagger

module U = struct
  let rotate theta z = Complex.(polar 1. theta |> mul z)
  let gauss_ln_2d z ~mean ~std =
    let open Complex in
    let x = Stats.Pdfs.gaussian_ln ~mean:mean.re ~std z.re in
    let y = Stats.Pdfs.gaussian_ln ~mean:mean.im ~std z.im in
    Log_space.unsafe_cast (x +. y)
end

module P = struct
  (* this simulation generates a number *)
  let samples = 20_000
  (* of sample configuration of a polymer chain in thermal equilibrium, with *)
  let n_links = 15
  (* rigid links, in the (complex number) plane in 2D. the first link starts
     from the origin at an angle distributed according to *)
  let prior_initial_angle =
    let center = 0. in
    let width = Float.pi *. 2. in
    Stats_dist.flat (center -. width /. 2.) (center +. width /. 2.)
  (* further links are added at random kink angles distributed with *)
  let prior_kink_angle =
    let width = Float.pi *. 0.1 in
    Stats_dist.flat (-. width /. 2.) (+. width /. 2.)
  (* the chain end point x + I y in the complex plane is constrained by a
   * harmonic potential that tries to stretch the chain to the right. *)
  let log_likelihood =
    let mean = {Complex.re = float n_links *. 1.; im = 0.} in
    let std = 0.1 in
    U.gauss_ln_2d ~mean ~std
end

module Lmh = Lmh_inference

module Model = struct
  let compute =
    let open Lmh in let open Lmh.Infix in
    let* init_dir =
      let* init_angle = sample P.prior_initial_angle in
      U.rotate init_angle Complex.one |> return in
    let rec chain i links =
      if i >= P.n_links then return (List.rev links) else
        let* angle = sample P.prior_kink_angle in
        let last_dir = List.hd links in
        let dir = U.rotate angle last_dir in
        chain (i+1) (dir::links) in
    let* links = chain 1 [init_dir] in
    let whole_chain = List.fold_left (fun growing link ->
        match growing with
        | [] -> [link]
        | h :: t -> Complex.add h link :: h :: t)
        [] links in
    let final_point = List.hd whole_chain in
    let* () = log_score (P.log_likelihood final_point) in
    return (init_dir, List.rev whole_chain)
end

let main () =
  let rng_state = RNG.make [| 0x1337; 0x533D; |] in
  let run () =
    let id, ch =
      Lmh.stream_samples Model.compute rng_state
      |> Seq.take P.samples
      |> Seq.split in
    let different_initial_angles_sampled =
      Seq.(zip id (repeat ())) |> Hashtbl.of_seq |> Hashtbl.to_seq_keys
      |> Seq.length in
    Format.printf
      ("%d total polymer chain samples yielded" ^^
       " %d different initial angle samples@.")
      P.samples different_initial_angles_sampled;
    let init_dirs, chains =
      let cplx_to_str {Complex.re; im} = Printf.sprintf "%f+%fI" re im in
      id |> Seq.map cplx_to_str |> Seq.map (fun x -> [x]) |> List.of_seq,
      ch |> Seq.map (List.map cplx_to_str) |> List.of_seq in
    Csv.save "test_initial_dirs.csv" init_dirs;
    Csv.save "test_chains.csv" chains in
  run ()

let _ = main ()
