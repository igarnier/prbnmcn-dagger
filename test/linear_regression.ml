open Dagger

let nonlinearity x = x *. x

let intercept = 0.03

let coeff = 12.2

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
    Plot.run
      ~target:(Plot.qt ())
      ~plot:
        (Plot.plot2
           ~xaxis:"x"
           ~yaxis:"freq"
           ~title
           ([ Plot.Line.line_2d
                ~style:Plot.Style.default
                ~points:(Plot.Data.of_list truth)
                ~legend:"truth"
                ();
              Plot.Scatter.points_2d
                ~points:
                  (Plot.Data.of_seq
                  @@ Seq.map (fun (x, y) -> Plot.r2 x y)
                  @@ List.to_seq synthetic_data_set)
                ~style:Plot.Style.default
                ~legend:"data"
                () ]
           @ few_samples))
      Plot.exec

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

(* linear regression: infer A, b such that (\sum_{i} |A x_i + b - y_i|^2)
   is minimized, for a given dataset x_i, y_i *)
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
    let () = plot "regression, traced" res in
    Array.for_all
      (fun (a, b) ->
        (* intercept is more sensitive to noise *)
        abs_float (a -. intercept) <. 3.0 && abs_float (b -. coeff) <. 0.5)
      res
end

let tests = [Traced.test; Traced_incremental.test]
