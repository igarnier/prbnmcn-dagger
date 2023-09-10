open Dagger.Smc_inference

module Poly = struct
  type t = float array

  let zero () = [| 0.0 |]

  let degree p = Array.length p - 1

  let get p i = if i >= Array.length p then 0.0 else p.(i)

  let add p1 p2 =
    let len = Int.max (Array.length p1) (Array.length p2) in
    Array.init len (fun i -> get p1 i +. get p2 i)

  let smul s a = Array.map (fun x -> x *. s) a

  let eval p x =
    let c = ref 0.0 in
    for i = 0 to Array.length p - 1 do
      c := !c +. (p.(i) *. (x ** float i))
    done ;
    !c

  let init deg f = Array.init (deg + 1) f

  let truncate deg p = Array.init (deg + 1) (fun i -> get p i)

  let pp fmtr (p : t) =
    let first = ref true in
    for i = 0 to Array.length p - 1 do
      let coeff = p.(i) in
      if Float.equal coeff 0.0 then ()
      else
        let sep =
          if !first then (
            first := false ;
            "")
          else " + "
        in
        if i = 0 then Format.fprintf fmtr "%f" p.(i)
        else if i = 1 then Format.fprintf fmtr "%s%f x" sep p.(i)
        else Format.fprintf fmtr "%s%f x^%d" sep p.(i) i
    done
end

module Smc_types = struct
  type particle_output = Poly.t

  type resampling_state = unit
end

module Smc = Make (Smc_types)

(* A random walk on polynomials *)
let mutate (p : Poly.t) =
  let open Smc in
  let open Infix in
  let current_degree = Poly.degree p in
  let* degree =
    sample
      (Stats_dist.uniform
         [| Int.max 0 (current_degree - 1);
            current_degree;
            current_degree + 1
         |])
  in
  let* noise =
    map_array
      (Poly.init degree (fun _ ->
           sample (Stats_dist.gaussian ~mean:0.0 ~std:1.0)))
      Fun.id
  in
  return (Poly.add noise p |> Poly.truncate degree)

let model observations =
  let open Smc in
  let open Infix in
  let rec loop observed acc prev_coeffs =
    match observed with
    | [] -> return ()
    | next :: ys ->
        let* coeffs = mutate prev_coeffs in
        let acc = next :: acc in
        (* Score the quality of the fit *)
        let* () =
          List_ops.iter
            (fun (x, y) ->
              let estimate = Poly.eval coeffs x in
              log_score @@ Stats_dist.Pdfs.gaussian_ln ~mean:y ~std:1.0 estimate)
            acc
        in
        (* Penalize high-degree polynomials *)
        let* () =
          log_score
          @@ Stats_dist.Pdfs.exponential_ln
               ~rate:0.5
               (float (Poly.degree coeffs))
        in
        let* () = yield coeffs in
        loop ys acc coeffs
  in
  loop observations [] (Poly.zero ())

let run_model observations rng_state =
  Smc.run
    (systematic_resampling ~ess_threshold:0.5)
    ()
    ~npart:10_000
    (model observations)
    rng_state
  |> Seq.filter_map (fun pop ->
         if Array.length pop.Smc.active = 0 then None
         else
           let itotal = 1. /. pop.total_mass in
           Array.fold_left
             (fun acc (coeff, w) -> Poly.(add acc (smul (w *. itotal) coeff)))
             (Poly.zero ())
             pop.active
           |> Option.some)
  |> Seq.memoize

let coeffs = [| 3.0; 25.0; -8.; 0.5 |]

let noisy_observations rng_state =
  List.init 150 (fun i ->
      let x = 0.1 *. float i in
      (x, Stats.Gen.gaussian ~mean:(Poly.eval coeffs x) ~std:10.0 rng_state))

let rng_state = Random.State.make [| 149572; 3891981; 48478478190758 |]

let observations = noisy_observations rng_state

let plot obs coeffs =
  let open Plot in
  if not Helpers.produce_artifacts then ()
  else
    let line2d points = Line.line_2d ~points:(points |> Data.of_list) () in
    let reference =
      Line.line_2d ~legend:"obs" ~points:(obs |> Data.of_list) ()
    in
    let plots = reference :: List.map line2d coeffs in
    run
      ~target:(png ~pixel_size:(1280, 1024) ~png_file:"smc_poly_obs.png" ())
      exec
      (plot2
         ~xaxis:"x"
         ~yaxis:"y"
         ~xrange:(Range.make ~min:0.0 ~max:15.0 ())
         ~yrange:(Range.make ~min:~-.70. ~max:70. ())
         plots)

let run () =
  let plot_predicted coeffs =
    List.map (fun (x, _) -> (x, Poly.eval coeffs x)) observations
  in
  let coeffs = run_model (noisy_observations rng_state) rng_state in
  let predicted =
    coeffs
    |> Seq.mapi (fun i elt -> (i, elt))
    |> Seq.filter (fun (i, _) -> i mod 10 = 0)
    |> Seq.map snd |> Seq.map plot_predicted |> List.of_seq
  in
  plot observations predicted ;
  Seq.iteri (fun i coeff -> Format.printf "%d, %a@." i Poly.pp coeff) coeffs

let tests =
  [ ( QCheck.Test.make ~name:"smc-poly-fit" ~count:1 QCheck.unit @@ fun () ->
      run () ;
      true ) ]
