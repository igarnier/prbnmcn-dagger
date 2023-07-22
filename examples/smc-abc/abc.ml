open Dagger

(* Implementation of the inference method in
   "Adaptive approximate Bayesian computation", Beaumont et al. *)

module Smc_types = struct
  type particle_output = { theta : float }

  type resampling_state =
    | Initial
    | Steady of { tau_sq : float; next_score : float -> float }
end

module Smc = Smc_inference.Make (Smc_types)

let rev_array_of_iter iter =
  let elts = ref [] in
  iter (fun elt w -> elts := (elt, w) :: !elts) ;
  Array.of_list !elts

module Vec = Containers.Vector

type streaming_variance_state =
  { mutable w_sum : float;
    mutable w_sum2 : float;
    mutable mean : float;
    mutable s : float
  }

let streaming_variance_init () =
  { w_sum = 0.0; w_sum2 = 0.0; mean = 0.0; s = 0.0 }

let next state ~w ~theta =
  state.w_sum <- state.w_sum +. w ;
  state.w_sum2 <- state.w_sum2 +. (w *. w) ;
  let mean_old = state.mean in
  state.mean <- mean_old +. (w /. state.w_sum *. (theta -. mean_old)) ;
  state.s <- state.s +. (w *. (theta -. mean_old) *. (theta -. state.mean))

let variance state = 2. *. state.s /. state.w_sum

let multinomial_resampling : (_, float, unit) Resampling.strategy =
  fun (type o)
      ~target_size
      ((module P) : (o, float) Resampling.particles)
      ()
      rng_state ->
   let elts = rev_array_of_iter P.iter in
   let sampler = Stats.Gen.categorical elts in
   let w = 1. /. float_of_int target_size in
   for _i = 0 to target_size - 1 do
     let p = sampler rng_state in
     P.append p w
   done

let abc_resampling prior_density :
    ( Smc_types.particle_output,
      float,
      Smc_types.resampling_state )
    Resampling.strategy =
 fun ~target_size (module P) resampling_state rng_state ->
  let elts = rev_array_of_iter P.iter in
  let sampler = Stats.Gen.categorical elts in

  let streaming = streaming_variance_init () in

  let vec_theta = Vec.create () in
  let vec_w = Vec.create () in
  for _i = 0 to target_size - 1 do
    let p = sampler rng_state in
    let { Smc_types.theta } = P.get_output p |> Option.get in
    let w = P.get_score p in
    P.append p w ;
    next streaming ~w ~theta ;
    Vec.push vec_theta theta ;
    Vec.push vec_w w
  done ;
  let tau_sq = variance streaming in
  assert (tau_sq >= 0.0) ;
  let tau_sq' =
    match resampling_state with
    | Initial ->
        let streaming = streaming_variance_init () in
        P.iter (fun p w ->
            let { Smc_types.theta } = P.get_output p |> Option.get in
            next streaming ~w ~theta) ;
        variance streaming
    | Steady { tau_sq; _ } -> tau_sq
  in
  let next_score theta =
    let acc = ref 0.0 in
    for j = 0 to target_size - 1 do
      let w_j = Vec.get vec_w j in
      let theta_j = Vec.get vec_theta j in
      let x = (theta -. theta_j) /. sqrt tau_sq' in
      acc := !acc +. (w_j *. Stats.Pdfs.gaussian ~mean:0.0 ~std:1.0 x)
    done ;
    prior_density theta /. !acc
  in
  Smc_types.Steady { tau_sq; next_score }

let rec loop_until_eps dist prior likelihood y eps =
  let open Smc.Infix in
  let* theta = prior in
  let* x = likelihood theta in
  if dist x y < eps then return theta
  else loop_until_eps dist prior likelihood y eps

let rec abc_loop dist likelihood y epss theta tau_sq next_score =
  let open Smc in
  let open Smc.Infix in
  match epss with
  | [] -> return theta
  | eps :: epss ->
      let prior = sample (Stats_dist.gaussian ~mean:theta ~std:tau_sq) in
      let* theta = loop_until_eps dist prior likelihood y eps in
      let* () = set_score (Log_space.of_float (next_score theta)) in
      let* (Smc_types.Steady { tau_sq; next_score }) =
        yield { Smc_types.theta }
      in
      abc_loop dist likelihood y epss theta tau_sq next_score
  [@@ocaml.warning "-8"]

let abc dist prior likelihood y epss =
  let open Smc in
  let open Smc.Infix in
  match epss with
  | [] -> assert false
  | eps :: epss ->
      let* theta = loop_until_eps dist prior likelihood y eps in
      let* (Steady { tau_sq; next_score }) = yield { Smc_types.theta } in
      abc_loop dist likelihood y epss theta tau_sq next_score
  [@@ocaml.warning "-8"]

(* We reproduce the toy mixture example given in the paper *)
let prior = Smc.sample (Stats_dist.flat (-10.) 10.)

let prior_density = Stats.Pdfs.uniform { min = -10.; max = 10. }

let likelihood theta =
  let open Smc in
  sample
  @@ Stats_dist.mixture
       [| 0.5; 0.5 |]
       [| Stats_dist.gaussian ~mean:theta ~std:1.0;
          Stats_dist.gaussian ~mean:theta ~std:0.01
       |]

let dist x y = abs_float (x -. y)

let y = 0.

let model =
  abc
    dist
    prior
    likelihood
    y
    (List.init 20 (fun i -> 2. /. float_of_int (i + 1)))

let rng_state = RNG.make [| 0x1337; 0xD3AD; 0xB33F |]

let plot_hist : int -> float Smc.population -> unit =
 fun index pop ->
  let open Plot in
  let data =
    pop.Smc.active
    |> Array.map (fun ({ Smc_types.theta }, w) -> (theta, w))
    |> Stats.Gen.categorical |> Stats.Gen.iid
  in
  let data = data rng_state |> Seq.map r1 |> Seq.take 4_000 in
  [Histogram.hist ~bins:100 ~points:(Data.of_seq data) ()]
  |> plot2 ~xaxis:"theta" ~yaxis:"w"
  |> run ~target:(png ~png_file:(Printf.sprintf "plot_%.2d.png" index) ()) exec

let result =
  Smc.run
    (abc_resampling prior_density)
    Smc_types.Initial
    ~npart:5_000
    model
    rng_state
  |> Seq.iteri plot_hist
