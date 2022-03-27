open Dagger
module Gen = Gen.Make (RNG)

let rng_state = RNG.make [| 0x1337; 0x533D |]

module Make (N : sig
  val name : string
end)
(Lang : Intf.S) (Runner : sig
  val stream_samples : float Lang.t -> RNG.t -> float Seq.t
end) =
struct
  open Lang
  open Runner

  let name s = Printf.sprintf "%s: %s" N.name s

  let eval ~burn_in nsamples (model : float t) =
    let samples = stream_samples model (RNG.copy rng_state) in
    let after_burn_in = Helpers.drop burn_in samples in
    Helpers.take nsamples after_burn_in

  let dist_with_oracle ?plot ?oracle_pdf ~burn_in nsamples model oracle =
    let model = eval ~burn_in nsamples model in
    let model_arr = Array.of_seq model in
    let (model, oracle_pdf_opt) = Helpers.to_fin_mes ?oracle_pdf model_arr in
    let oracle_arr = Emp.of_generative ~nsamples oracle rng_state in
    let (oracle, _) = Helpers.to_fin_mes oracle_arr in
    (match (Helpers.produce_artifacts, plot) with
    | (false, _) -> ()
    | (true, None) -> ()
    | (true, Some s) ->
        Helpers.plot_binned
          s
          ([(model, "model"); (oracle, "oracle")]
          @ match oracle_pdf_opt with None -> [] | Some m -> [(m, "pdf")])) ;
    assert (Array.length oracle_arr = Array.length model_arr) ;
    Fin.Float.Dist.lp int_table ~p:1. model oracle

  (* ------------------------------------------------------------------------- *)
  (* 1d gaussian *)

  let gaussian_1d = sample @@ Stats_dist.gaussian ~mean:0.0 ~std:1.0

  let test_gaussian_1d =
    QCheck.Test.make ~name:(name "1d gaussian") ~count:1 QCheck.unit
    @@ fun () ->
    let dist =
      dist_with_oracle
        ~plot:(Printf.sprintf "%s_gaussian_1d.png" N.name)
        ~oracle_pdf:(Pdfs.gaussian ~mean:0.0 ~std:1.0)
        ~burn_in:1_000
        150_000
        gaussian_1d
        (Gen.gaussian ~mean:0.0 ~std:1.0)
    in
    let bound = 0.03 in
    if not (dist <. bound) then
      QCheck.Test.fail_reportf "dist %f >= %f" dist bound
    else true

  (* ------------------------------------------------------------------------- *)
  (* 1d gamma *)

  let gamma_1d = sample @@ Gsl_dist.gamma ~a:3. ~b:(1. /. 3.)

  let test_gamma_1d =
    QCheck.Test.make ~name:(name "1d gamma") ~count:1 QCheck.unit @@ fun () ->
    let gen rng_state =
      Gsl.Randist.gamma ~a:3.0 ~b:(1. /. 3.) (Gsl_dist.rng_of_lxm rng_state)
    in
    let dist =
      dist_with_oracle
        ~plot:(Printf.sprintf "%s_gamma_1d.png" N.name)
        ~oracle_pdf:(Gsl.Randist.gamma_pdf ~a:3.0 ~b:(1. /. 3.))
        ~burn_in:1_000
        100_000
        gamma_1d
        gen
    in
    let bound = 0.028 in
    if not (dist <. bound) then
      QCheck.Test.fail_reportf "dist %f >= %f" dist bound
    else true

  (* ------------------------------------------------------------------------- *)
  (* sum of independent gaussians *)

  let gaussian_sum =
    let n = Stats_dist.gaussian ~mean:0.0 ~std:1.0 in
    map2 (sample n) (sample n) ( +. )

  let gaussian_sum_oracle =
    let open Gen.Infix in
    let* x = Gen.gaussian ~mean:0.0 ~std:1.0 in
    let* y = Gen.gaussian ~mean:0.0 ~std:1.0 in
    return (x +. y)

  let test_gaussian_sum =
    QCheck.Test.make ~name:(name "gaussian sum") ~count:1 QCheck.unit
    @@ fun () ->
    let dist =
      dist_with_oracle
        ~plot:(Printf.sprintf "%s_gaussian_sum.png" N.name)
        ~oracle_pdf:(fun x -> Pdfs.gaussian ~mean:0.0 ~std:(sqrt 2.0) x)
        ~burn_in:1_000
        100_000
        gaussian_sum
        gaussian_sum_oracle
    in
    let bound = 0.05 in
    if not (dist <. bound) then
      QCheck.Test.fail_reportf "dist %f >= %f" dist bound
    else true

  (* ------------------------------------------------------------------------- *)
  (* sum of independent gaussians, using iid *)

  let iid_gaussian_sum =
    let open Infix in
    let n = Stats_dist.gaussian ~mean:0. ~std:1. in
    let+ arr = sample (Dist.iid 10 n) in
    Array.fold_left ( +. ) 0.0 arr

  let iid_gaussian_sum_oracle =
    let open Gen.Infix in
    let rec loop n acc =
      if n = 0 then return acc
      else
        let* x = Gen.gaussian ~mean:0.0 ~std:1.0 in
        loop (n - 1) (acc +. x)
    in
    loop 10 0.

  let test_iid_gaussian_sum =
    QCheck.Test.make ~name:(name "iid gaussian sum") ~count:1 QCheck.unit
    @@ fun () ->
    let dist =
      dist_with_oracle
        ~plot:(Printf.sprintf "%s_iid_gaussian_sum.png" N.name)
        ~oracle_pdf:(fun x -> Pdfs.gaussian ~mean:0.0 ~std:(sqrt 10.0) x)
        ~burn_in:10_000
        150_000
        iid_gaussian_sum
        iid_gaussian_sum_oracle
    in
    let bound = 0.05 in
    if not (dist <. bound) then
      QCheck.Test.fail_reportf "dist %f >= %f" dist bound
    else true

  (* ------------------------------------------------------------------------- *)
  (* dependent gaussian chain *)

  let gaussian_chain =
    let open Infix in
    let n = Stats_dist.gaussian ~mean:0.0 ~std:1.0 in
    let* x = sample n in
    sample @@ Stats_dist.gaussian ~mean:x ~std:1.0

  let gaussian_chain_oracle =
    let open Gen.Infix in
    let* x = Gen.gaussian ~mean:0.0 ~std:1.0 in
    let* y = Gen.gaussian ~mean:x ~std:1.0 in
    return y

  let test_gaussian_chain =
    QCheck.Test.make ~name:(name "gaussian chain") ~count:1 QCheck.unit
    @@ fun () ->
    let dist =
      dist_with_oracle
        ~plot:(Printf.sprintf "%s_gaussian_chain.png" N.name)
        ~oracle_pdf:(Pdfs.gaussian ~mean:0.0 ~std:(sqrt 2.0))
        ~burn_in:10_000
        100_000
        gaussian_chain
        gaussian_chain_oracle
    in
    let bound = 0.09 in
    if not (dist <. bound) then
      QCheck.Test.fail_reportf "dist %f >= %f" dist bound
    else true

  (* ------------------------------------------------------------------------- *)
  (* map2 is equivalent to bind; bind  *)

  let gaussian_sum_map =
    let open Infix in
    let n = Stats_dist.gaussian ~mean:0.0 ~std:1.0 in
    let+ x =
      let* x = sample n in
      let* () = score (abs_float x) in
      return x
    and+ y =
      let* y = sample n in
      let* () = score (abs_float y) in
      return y
    in
    x +. y

  let gaussian_sum_bind =
    let open Infix in
    let n = Stats_dist.gaussian ~mean:0.0 ~std:1.0 in
    let* x =
      let* x = sample n in
      let* () = score (abs_float x) in
      return x
    in
    let* y =
      let* y = sample n in
      let* () = score (abs_float y) in
      return y
    in
    return (x +. y)

  let test_map2_bind =
    QCheck.Test.make ~name:(name "bind; bind vs map2") ~count:1 QCheck.unit
    @@ fun () ->
    let bind_based =
      eval ~burn_in:1_000 100_000 gaussian_sum_map |> Array.of_seq
    in
    let map_based =
      eval ~burn_in:1_000 100_000 gaussian_sum_bind |> Array.of_seq
    in
    Array.iter (fun f -> Format.printf "%f@." f) bind_based ;
    Array.iter (fun f -> Format.printf "%f@." f) map_based ;
    let m1 = Helpers.histogram map_based in
    let m2 = Helpers.histogram bind_based in
    let () =
      if Helpers.produce_artifacts then
        Helpers.plot_binned
          (Format.asprintf "bind_vs_map2_%s.png" N.name)
          [(m1, "map"); (m2, "bind")]
      else ()
    in
    let dist = Fin.Float.Dist.lp int_table ~p:1.0 m1 m2 in
    let bound = 0.09 in
    if not (dist <. bound) then
      QCheck.Test.fail_reportf "dist %f >= %f" dist bound
    else true

  (* ------------------------------------------------------------------------- *)
  (* Mixtures *)

  let mixture1 =
    let open Infix in
    let* cond = sample @@ Stats_dist.bernoulli ~bias:0.5 in
    if cond then sample @@ Stats_dist.gaussian ~mean:10. ~std:2.
    else sample @@ Stats_dist.gaussian ~mean:1. ~std:2.

  let oracle_pdf x =
    (0.5 *. Pdfs.gaussian ~mean:10. ~std:2. x)
    +. (0.5 *. Pdfs.gaussian ~mean:1. ~std:2. x)

  let mixture_oracle =
    let open Gen.Infix in
    let* x = Gen.gaussian ~mean:0.0 ~std:1.0 in
    if x >. 0.0 then Gen.gaussian ~mean:10.0 ~std:2.
    else Gen.gaussian ~mean:1.0 ~std:2.

  let test_mixture1 =
    QCheck.Test.make ~name:(name "mixture 1") ~count:1 QCheck.unit @@ fun () ->
    let dist =
      dist_with_oracle
        ~plot:(Printf.sprintf "%s_mixture1.png" N.name)
        ~oracle_pdf
        ~burn_in:10_000
        100_000
        mixture1
        mixture_oracle
    in
    let bound = 0.1 in
    if not (dist <. bound) then
      QCheck.Test.fail_reportf "dist = %f >= %f" dist bound
    else true

  let mixture2 =
    let cond = sample @@ Stats_dist.bernoulli ~bias:0.5 in
    let left = sample @@ Stats_dist.gaussian ~mean:10. ~std:2. in
    let right = sample @@ Stats_dist.gaussian ~mean:1. ~std:2. in
    if_ cond (fun x -> if x then left else right)

  let test_mixture2 =
    QCheck.Test.make ~name:(name "mixture 2") ~count:1 QCheck.unit @@ fun () ->
    let dist =
      dist_with_oracle
        ~plot:(Printf.sprintf "%s_mixture2.png" N.name)
        ~oracle_pdf
        ~burn_in:10_000
        100_000
        mixture2
        mixture_oracle
    in
    let bound = 0.1 in
    if not (dist <. bound) then
      QCheck.Test.fail_reportf "dist = %f >= %f" dist bound
    else true

  let mixture3 =
    (* Example taken from "A Provably correct sampler for probabilistic programs"
       Chunk-Kil Hur et al. *)
    let cond = sample @@ Gsl_dist.bernoulli ~bias:0.5 in
    let left = sample @@ Gsl_dist.gaussian ~mean:10. ~std:2. in
    let right = sample @@ Gsl_dist.gamma ~a:3. ~b:(1. /. 3.) in
    if_ cond (fun x -> if x then left else right)

  let mixture3_oracle =
    let open Gen.Infix in
    let gen rng_state =
      Gsl.Randist.gamma ~a:3.0 ~b:(1. /. 3.) (Gsl_dist.rng_of_lxm rng_state)
    in
    let* x = Gen.gaussian ~mean:0.0 ~std:1.0 in
    if x >. 0.0 then Gen.gaussian ~mean:10.0 ~std:2. else gen

  let oracle_pdf x =
    (0.5 *. Pdfs.gaussian ~mean:10. ~std:2. x)
    +. (0.5 *. Gsl.Randist.gamma_pdf ~a:3. ~b:(1. /. 3.) x)

  let test_mixture3 =
    QCheck.Test.make ~name:(name "mixture 3") ~count:1 QCheck.unit @@ fun () ->
    let dist =
      dist_with_oracle
        ~plot:(Printf.sprintf "%s_mixture3.png" N.name)
        ~oracle_pdf
        ~burn_in:10_000
        100_000
        mixture3
        mixture3_oracle
    in
    let bound = 0.1 in
    if not (dist <. bound) then
      QCheck.Test.fail_reportf "dist = %f >= %f" dist bound
    else true

  (* let mixture =
   *   let open Infix in
   *   let* cond =
   *     sample @@ Stats_dist.bernoulli ~bias:0.5
   *     (\* let* x = sample @@ Stats_dist.gaussian ~mean:0.0 ~std:1.0 in
   *      * return (x >. 0.0) *\)
   *   in
   *   let* l = sample @@ Stats_dist.gaussian ~mean:10. ~std:2. in
   *   let* r = sample @@ Stats_dist.gaussian ~mean:1. ~std:2. in
   *   if__ cond (function
   *       | true ->
   *           return l (\* sample @@ Stats_dist.gaussian ~mean:10. ~std:2. *\)
   *       | false -> return r) *)

  (* sample @@ Gsl_dist.gamma ~a:3. ~b:(1. /. 3.) *)
  (* sample @@ Stats_dist.gaussian ~mean:1. ~std:2.) *)

  (* let mixture =
   *   let open Infix in
   *   let* x = sample @@ Stats_dist.gaussian ~mean:0.0 ~std:1.0 in
   *   if x >. 0.0 then sample @@ Stats_dist.gaussian ~mean:10. ~std:2.
   *   else sample @@ Gsl_dist.gamma ~a:3. ~b:(1. /. 3.) *)

  (* let mixture =
   *   let open Infix in
   *   let* x =
   *     sample @@ Stats_dist.bernoulli ~bias:0.5 (\* ~mean:0.0 ~std:1.0 *\)
   *   in
   *   if x then sample @@ Stats_dist.gaussian ~mean:10. ~std:2.
   *   else sample @@ Stats_dist.gaussian ~mean:1.0 ~std:2. *)

  (* ------------------------------------------------------------------------- *)
  (* Sample from Gaussian using importance sampling *)

  type range = { min : float; max : float }

  let uniform_in_interval ~range:{ min; max } state =
    if max -. min >=. 0. then min +. Random.State.float state (max -. min)
    else invalid_arg "uniform_in_interval"

  let uniform_dist = Stats_dist.flat ~-.5. 5.

  let importance_gaussian =
    let open Infix in
    let* x = sample uniform_dist in
    let* () = score (Stats.Pdfs.gaussian ~mean:0.0 ~std:1.0 x) in
    return x

  let test_importance =
    QCheck.Test.make ~name:(name "importance_gaussian") ~count:1 QCheck.unit
    @@ fun () ->
    let dist =
      dist_with_oracle
        ~plot:(Printf.sprintf "%s_importance.png" N.name)
        ~burn_in:10_000
        ~oracle_pdf:(fun x -> Stats.Pdfs.gaussian ~mean:0.0 ~std:1.0 x)
        100_000
        importance_gaussian
        (Gen.gaussian ~mean:0.0 ~std:1.0)
    in
    let bound = 0.08 in
    if not (dist <. bound) then
      QCheck.Test.fail_reportf "dist = %f >= %f" dist bound
    else true

  (* ------------------------------------------------------------------------- *)

  let tests =
    [ test_gaussian_1d;
      test_gamma_1d;
      test_gaussian_sum;
      test_iid_gaussian_sum;
      test_gaussian_chain;
      test_mixture1;
      test_mixture2;
      test_mixture3;
      test_importance;
      test_map2_bind ]
end

module Lmh =
  Make
    (struct
      let name = "lmh_inference"
    end)
    (Lmh_inference)
    (Lmh_inference)

module Lmh_incremental =
  Make
    (struct
      let name = "lmh_incremental_inference"
    end)
    (Lmh_incremental_inference)
    (Lmh_incremental_inference)

module Smc_systematic =
  Make
    (struct
      let name = "smc-systematic"
    end)
    (Smc_inference)
    (struct
      let sampler_of_population (pop : (float * Log_space.t) list) =
        let pop = List.rev_map (fun (x, w) -> (x, Log_space.to_float w)) pop in
        Stats_dist.Gen.categorical (Array.of_list pop)

      let stream_samples model rng_state =
        let categorical =
          Smc_inference.Non_interruptible.run
            ~resampling:Smc_inference.systematic_resampling
            300_000
            model
            rng_state
          |> sampler_of_population
        in
        Seq.unfold (fun () -> Some (categorical rng_state, ())) ()
    end)

module Smc_stratified =
  Make
    (struct
      let name = "smc-stratified"
    end)
    (Smc_inference)
    (struct
      let sampler_of_population (pop : (float * Log_space.t) list) =
        let pop = List.rev_map (fun (x, w) -> (x, Log_space.to_float w)) pop in
        Stats_dist.Gen.categorical (Array.of_list pop)

      let stream_samples model rng_state =
        let categorical =
          Smc_inference.Non_interruptible.run
            ~resampling:Smc_inference.stratified_resampling
            300_000
            model
            rng_state
          |> sampler_of_population
        in
        Seq.unfold (fun () -> Some (categorical rng_state, ())) ()
    end)
