(* We write a Kalman filter to predict the trajectory of
   a unit point mass submitted to the gravity and a random
   wind force *)
type r2 = { x : float; y : float }

type point = r2

type vec = r2

type mass = { pos : point; vel : vec }

module R2 = struct
  module R = Basic_structures.Std.Float

  type t = r2

  let zero = { x = 0.0; y = 0.0 }

  let add a b = { x = a.x +. b.x; y = a.y +. b.y }

  let neg a = { x = -.a.x; y = -.a.y }

  let smul s a = { x = s *. a.x; y = s *. a.y }

  let l2 a =
    let x = a.x in
    let y = a.y in
    sqrt ((x *. x) +. (y *. y))
end

let pp_r2 fmtr { x; y } = Format.fprintf fmtr "(%.2f, %.2f)" x y

let pp_mass fmtr { pos; vel } =
  Format.fprintf fmtr "{ pos = %a; vel = %a }" pp_r2 pos pp_r2 vel

let initial vel = { pos = { x = 0.0; y = 0.0 }; vel }

module Dynamics = struct
  let gravity = -9.81

  let wind_std = 10.

  (* Simple forward Euler step *)
  let step : m:mass -> dt:float -> mass Stats.Gen.t =
   fun ~m ~dt rng_state ->
    let (xwind, ywind) =
      Stats.Gen.box_muller ~mean:0.0 ~std:wind_std rng_state
    in
    let xvel = m.vel.x +. (xwind *. dt) in
    let yvel = m.vel.y +. ((ywind +. gravity) *. dt) in
    let xpos = m.pos.x +. xvel in
    let ypos = m.pos.y +. yvel in
    { pos = { x = xpos; y = ypos }; vel = { x = xvel; y = yvel } }

  let init_vel = { x = 10.; y = 100. }

  let init = initial init_vel

  let simulate :
      duration:float ->
      dt:float ->
      init:mass ->
      Random.State.t ->
      (mass * float) Seq.t =
   fun ~duration ~dt ~init rng_state ->
    Seq.unfold
      (fun (m, t) ->
        if t > duration || m.pos.y < 0.0 then None
        else
          let next = step ~m ~dt rng_state in
          Some ((next, t), (next, t +. dt)))
      (init, 0.0)
    |> Seq.memoize

  let state = Random.State.make [| 0xDEAD; 0xBEEF |]

  (* Simulate a trajectory *)
  let trajectory = simulate ~duration:30. ~dt:0.01 ~init state

  let observation_noise_std = 5.

  let noisfy_r2 { x; y } =
    { x = Stats.Gen.gaussian ~mean:x ~std:observation_noise_std state;
      y = Stats.Gen.gaussian ~mean:y ~std:observation_noise_std state
    }

  let noisify m = { pos = noisfy_r2 m.pos; vel = noisfy_r2 m.vel }

  (* We only observe a noisy version of the trajectory *)
  let noisy_trajectory =
    Seq.map
      (fun (m, t) ->
        let noisy = noisify m in
        (noisy, t))
      trajectory
    |> Array.of_seq
end

let nparticles = 5000

module Kalman_cps = struct
  open Dagger

  module Smc =
    Smc_inference.Make
      (struct
        type particle_output = point

        type resampling_state = unit
      end)
      ()

  open Smc
  open Infix
  module Dist = Stats.Gen

  let step_model : m:mass -> dt:float -> mass t =
   fun ~m ~dt ->
    let* xwind = sample (Dist.gaussian ~mean:0.0 ~std:Dynamics.wind_std) in
    let* ywind = sample (Dist.gaussian ~mean:0.0 ~std:Dynamics.wind_std) in
    let xvel = m.vel.x +. (xwind *. dt) in
    let yvel = m.vel.y +. ((ywind +. Dynamics.gravity) *. dt) in
    let xpos = m.pos.x +. xvel in
    let ypos = m.pos.y +. yvel in
    return { pos = { x = xpos; y = ypos }; vel = { x = xvel; y = yvel } }

  let score_estimate ~(observed : mass) ~(estimate : mass) =
    let score estimate observed =
      (Stats.Pdfs.gaussian_ln
         ~mean:estimate
         ~std:Dynamics.observation_noise_std
         observed
        :> float)
      |> Log_space.unsafe_cast
        [@@ocaml.inline]
    in
    let px = score estimate.pos.x observed.pos.x in
    let py = score estimate.pos.y observed.pos.y in
    let vx = score estimate.vel.x observed.vel.x in
    let vy = score estimate.vel.y observed.vel.y in
    let score = Log_space.mul (Log_space.mul px py) (Log_space.mul vx vy) in
    let* () = log_score score in
    (* Resampling will occur here *)
    yield estimate.pos

  let rec model i estimate t traj =
    match Seq.uncons traj with
    | None -> return ()
    | Some ((m, t'), rest) ->
        (*
          predict
         *)
        let* next_estimate = step_model ~m:estimate ~dt:(t' -. t) in
        (*
          score current estimate wrt observation, perform resampling
         *)
        let* () = score_estimate ~observed:m ~estimate in
        (*
          Recurse
         *)
        model (i + 1) next_estimate t' rest

  let filtered =
    let rng_state = Random.State.make [| 0xDEAD; 0xBEEF |] in
    let t0 = Unix.gettimeofday () in
    let pop =
      let traj = Dynamics.noisy_trajectory |> Array.to_seq in
      run_custom
        ~nthreads:1
        (Smc_inference.systematic_resampling ~ess_threshold:0.5)
        ()
        ~npart:nparticles
        (Fun.const (model 0 Dynamics.init 0.0 traj))
        rng_state
      |> Seq.filter_map (fun pop ->
             if Array.length pop.active = 0 then None else Option.some pop)
      |> Array.of_seq
    in
    let t1 = Unix.gettimeofday () in
    Format.printf "inference time %f@." (t1 -. t0) ;
    Array.to_seq pop
end

let plot_traj ?legend ?error_bars traj =
  Plot.Line.line_2d
    ~points:
      (traj |> Seq.map (fun pos -> Plot.r2 pos.x pos.y) |> Plot.Data.of_seq)
    ?legend
    ?error_bars
    ()

let plot title (filtered : unit Kalman_cps.Smc.population Seq.t) =
  let open Plot in
  let real_trajectory =
    Dynamics.trajectory
    |> Seq.map (fun (m, _t) -> m.pos)
    |> plot_traj ~legend:"real"
  in
  let noisy_trajectory =
    Dynamics.noisy_trajectory |> Array.to_seq
    |> Seq.map (fun (m, _t) -> m.pos)
    |> plot_traj ~legend:"observed"
  in
  let avg total (pop : (point * float) array) : point =
    let itotal = 1. /. total in
    Array.fold_left
      (fun acc (p, w) -> R2.add acc (R2.smul (w *. itotal) p))
      R2.zero
      pop
  in
  let mean_trajectory =
    Seq.map
      (fun { Kalman_cps.Smc.active; total_mass; terminated = _ } ->
        let pos = Array.map fst active in
        let mean = avg total_mass active in
        (pos, mean))
      filtered
  in
  let spread =
    mean_trajectory
    |> Seq.map (fun (pos, mean) ->
           let spreads = Array.map (fun p -> R2.(l2 (add mean (neg p)))) pos in
           let median_spread = Stats.Emp.quantile (module Float) spreads 0.5 in
           (median_spread, median_spread))
    |> Data.of_seq
  in
  let predicted_trajectory =
    mean_trajectory |> Seq.map snd
    |> plot_traj ~legend:"mean" ~error_bars:spread
  in
  plot2
    ~xaxis:"x"
    ~yaxis:"y"
    ~title
    [real_trajectory; noisy_trajectory; predicted_trajectory]

let () = plot "cps" Kalman_cps.filtered |> Plot.(run ~target:(qt ()) exec)
