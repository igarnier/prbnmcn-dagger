(** Testing various backends on bayesian linear regression *)

open Dagger

(** {2 Setup and helper definitions } *)

(** {3 Definition of the non-linearity to infer } *)

let nonlinearity x = x *. x

let intercept = 0.03

let coeff = 12.2

(** {3 Sampling of a synthetic data set } *)

let rng_state = RNG.make [| 0x1337; 0x533D |]

let synthetic_data_set =
  List.init 100 (fun x ->
      let x = float_of_int x /. 10. in
      let y =
        intercept
        +. (coeff *. nonlinearity x)
        +. Stats_dist.Gen.gaussian ~mean:0.0 ~std:1.0 rng_state
      in
      (x, y))

(** {3 Plotting helpers } *)

let from_parameters ~intercept ~coeff =
  List.init 100 (fun x ->
      let x = float_of_int x /. 10. in
      let y = intercept +. (coeff *. nonlinearity x) in
      Plot.r2 x y)

let plot title samples =
  if Helpers.produce_artifacts then
    let truth = from_parameters ~intercept ~coeff in
    let few_samples =
      Array.to_list samples |> List.sort_uniq compare
      |> List.map (fun (intercept, coeff) ->
             Plot.Line.line_2d
               ~style:Plot.Style.(default |> set_circle ~radius:1.)
               ~points:Plot.Data.(of_list (from_parameters ~intercept ~coeff))
               ())
    in
    Plot.(
      run
        ~target:(qt ())
        exec
        (plot2
           ~xaxis:"x"
           ~yaxis:"freq"
           ~title
           ([ Line.line_2d
                ~style:Style.default
                ~points:(Data.of_list truth)
                ~legend:"truth"
                ();
              Scatter.points_2d
                ~points:
                  (Data.of_seq
                  @@ Seq.map (fun (x, y) -> r2 x y)
                  @@ List.to_seq synthetic_data_set)
                ~style:Style.default
                ~legend:"data"
                () ]
           @ few_samples)))

(** {2 Regression using Lmh_inference } *)

(* linear regression: infer A, b such that (\sum_{i} |A x_i + b - y_i|^2)
   is minimized, for a given dataset x_i, y_i *)
module Traced = struct
  open Lmh_inference

  let regression data (nonlinearity : float -> float) =
    let open Infix in
    let* intercept = sample (Stats_dist.gaussian ~mean:0.0 ~std:1.0) in
    let* coeff = sample (Stats_dist.gaussian ~mean:0.0 ~std:15.) in
    let+ () =
      List.fold_left
        (fun acc (x, y) ->
          let predicted = intercept +. (coeff *. nonlinearity x) in
          let s = Stats.Pdfs.gaussian ~mean:predicted ~std:1.0 y in
          let* () = score s in
          acc)
        (return ())
        data
    in
    (intercept, coeff)

  let model = regression synthetic_data_set nonlinearity

  let eval nsamples : (float * float) array =
    Lmh_inference.stream_samples model rng_state
    |> Helpers.drop 1000 |> Helpers.take nsamples |> Array.of_seq

  let test =
    QCheck.Test.make ~name:"linear regression, traced" ~count:1 QCheck.unit
    @@ fun () ->
    let t0 = Unix.gettimeofday () in
    (* remove the first 1000 samples as it corresponds to the burn-in of the chain *)
    let res = eval 2000 in
    let t1 = Unix.gettimeofday () in
    Format.printf "data generated in %f seconds@." (t1 -. t0) ;
    let () = Format.printf "samples: %d@." (Array.length res) in
    let () = plot "regression, traced" res in
    Array.for_all
      (fun (a, b) ->
        (* intercept is more sensitive to noise *)
        abs_float (a -. intercept) <. 3.0 && abs_float (b -. coeff) <. 0.5)
      res
end

(** {2 Regression using Lmh_incremental_inference } *)

module Traced_incremental = struct
  open Lmh_incremental_inference

  let regression data (nonlinearity : float -> float) =
    let open Infix in
    let* intercept = sample (Stats_dist.gaussian ~mean:0.0 ~std:1.0) in
    let* coeff = sample (Stats_dist.gaussian ~mean:0.0 ~std:15.) in
    let+ () =
      List.fold_left
        (fun acc (x, y) ->
          let predicted = intercept +. (coeff *. nonlinearity x) in
          let s = Stats.Pdfs.gaussian ~mean:predicted ~std:1.0 y in
          let* () = score s in
          acc)
        (return ())
        data
    in
    (intercept, coeff)

  let model = regression synthetic_data_set nonlinearity

  let eval nsamples : (float * float) array =
    Lmh_incremental_inference.stream_samples model rng_state
    |> Helpers.drop 1000 |> Helpers.take nsamples |> Array.of_seq

  let test =
    QCheck.Test.make
      ~name:"linear regression, traced, incremental"
      ~count:1
      QCheck.unit
    @@ fun () ->
    let t0 = Unix.gettimeofday () in
    (* remove the first 1000 samples as it corresponds to the burn-in of the chain *)
    let res = eval 2000 in
    let t1 = Unix.gettimeofday () in
    Format.printf "data generated in %f seconds@." (t1 -. t0) ;
    let () = Format.printf "samples: %d@." (Array.length res) in
    let () = plot "regression, traced (incremental)" res in
    Array.for_all
      (fun (a, b) ->
        (* intercept is more sensitive to noise *)
        abs_float (a -. intercept) <. 3.0 && abs_float (b -. coeff) <. 0.5)
      res
end

(** {2 Regression using Smc_inference } *)

module Smc = struct
  open Smc_inference

  let regression data (nonlinearity : float -> float) =
    let open Infix in
    let* intercept = sample (Stats_dist.gaussian ~mean:0.0 ~std:1.0) in
    let* coeff = sample (Stats_dist.gaussian ~mean:0.0 ~std:15.) in
    let+ () =
      List.fold_left
        (fun acc (x, y) ->
          let predicted = intercept +. (coeff *. nonlinearity x) in
          let s = Stats.Pdfs.gaussian ~mean:predicted ~std:1.0 y in
          let* () = score s in
          acc)
        (return ())
        data
    in
    (intercept, coeff)

  let model = regression synthetic_data_set nonlinearity

  module R2 :
    Basic_structures.Basic_intf.Module_std
      with type R.t = float
       and type t = float * float = struct
    module R = Basic_structures.Basic_impl.Reals.Float

    type t = float * float

    let zero = (0., 0.)

    let add (x, y) (x', y') = (x +. x', y +. y')

    let neg (x, y) = (~-.x, ~-.y)

    let smul s (x, y) = (s *. x, s *. y)

    let pp fmtr (x, y) = Format.fprintf fmtr "(%f, %f)" x y

    let hash = Hashtbl.hash

    let compare = Stdlib.compare

    let equal (x, y) (x', y') = x =. x' && y =. y'
  end

  module FM =
    Basic_structures.Basic_impl.Free_module.Float_valued.Make_with_map (R2)

  module Table = Hashtbl.Make (R2)

  let eval nsamples = Non_interruptible.run nsamples model rng_state

  let average pop =
    let pop =
      pop |> List.to_seq
      |> Seq.map (fun (p, w) -> (p, Log_space.to_float w))
      |> Array.of_seq
      |> Stats.Fin.Float.of_assoc (module Table)
    in
    Stats.Fin.Float.measure pop
    |> Stats.Fin.Float.normalize |> Stats.Fin.Float.as_measure
    |> Stats.Fin.Float.mean_generic (module R2)

  let test =
    QCheck.Test.make ~name:"linear regression, smc" ~count:1 QCheck.unit
    @@ fun () ->
    let t0 = Unix.gettimeofday () in
    let pop = eval 2000 in
    let t1 = Unix.gettimeofday () in
    Format.printf "data generated in %f seconds@." (t1 -. t0) ;
    let res = average pop in
    let () =
      plot
        (Printf.sprintf "regression, smc (y0=%f, coeff=%f)" (fst res) (snd res))
        [| res |]
    in
    Array.for_all
      (fun (a, b) ->
        (* intercept is more sensitive to noise *)
        abs_float (a -. intercept) <. 3.0 && abs_float (b -. coeff) <. 0.5)
      [| res |]
end

let tests = [Traced.test; Traced_incremental.test; Smc.test]
