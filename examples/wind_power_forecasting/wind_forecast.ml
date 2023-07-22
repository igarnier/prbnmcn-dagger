(** {1 Forecasting the power output of a wind turbine}

    We use a dataset made available {{: https://www.kaggle.com/theforcecoder/wind-power-forecasting }here},
    which corresponds to the power output of a 1750 kW wind turbine. *)

module Seq = CCSeq

let sf = Printf.sprintf

(* ------------------------------------------------------------------------- *)

(** {2 Helpers} *)

(** Naming typical durations *)

let minute = 60.

let hour = minute *. 60.

let day = hour *. 24.

let year = day *. 365.

let month = year /. 12.

(** Compute the (approximate) time derivative of the series *)
let derivative series =
  Seq.map2
    (fun (t, x) (t', x') -> (t, (x' -. x) /. (t' -. t)))
    series
    (Seq.tail_exn series)

(** Restrict a signal to a given time interval *)
let restrict low hi =
  assert (low < hi) ;
  Seq.filter (fun (t, _) -> t > low && t <= hi)

(** If [seq = (t1, x1), (t2, x2), (t3, x3) ...] then [cumulative seq] is equal to
    [(t1, x1), (t2, x1 + x2), (t3, x1 + x2 + x3), ...]. *)
let cumulative ts =
  Seq.unfold
    (fun (prev, s) ->
      match s () with
      | Seq.Nil -> None
      | Seq.Cons ((t, x), s') ->
          let tot = prev +. x in
          Some ((t, tot), (tot, s')))
    (0.0, ts)

(** [fold_unfold f state seq] folds [f] over [seq] and constructs lazily
   a sequence out of each outcome. *)
let rec fold_unfold f state seq =
  match seq () with
  | Seq.Nil -> Seq.nil
  | Seq.Cons (elt, tail) ->
      let (state, elt) = f state elt in
      fun () -> Seq.Cons (elt, fold_unfold f state tail)

(** Average a [signal] piecewise according to [kernel]. *)
let average (signal : Float.Array.t) (kernel : Float.Array.t) =
  let m = Float.Array.length signal in
  let n = Float.Array.length kernel in
  if n > m then invalid_arg "average: dimensions mismatch (kernel too big)" ;
  (* some samples might be lost but we don't care *)
  let s = m / n in
  Float.Array.init s (fun i ->
      let acc = ref 0.0 in
      let start = i * n in
      for j = 0 to n - 1 do
        acc :=
          !acc
          +. (Float.Array.get signal (start + j) *. Float.Array.get kernel j)
      done ;
      !acc)

(** Map the first and second components of a sequence of pairs *)
let map_fst f seq = Seq.map (fun (x, y) -> (f x, y)) seq

let map_snd f seq = Seq.map (fun (x, y) -> (x, f y)) seq

(* ------------------------------------------------------------------------- *)
(** {2 Loading and preparing the data} *)

let rows =
  Containers.IO.with_in Sys.argv.(1) @@ fun ic ->
  let csv = Csv.of_channel ic in
  (* Correct the header of the CSV *)
  let header = Csv.next csv in
  Csv.Rows.set_header ~replace:true csv ("Date" :: List.tl header) ;
  Seq.of_list @@ Csv.Rows.input_all csv

(* We project out of the time series the output power and wind speed.
   Units are not explicited in the dataset: we posit that wind speed is given in m/s and
   power in kilowatts.

   Note that the dataset misses a lot of rows: for now we fill those with zeroes
   but we will have to deal with that later. *)

let proj_attribute row attribute =
  match float_of_string (Csv.Row.find row attribute) with
  | x -> x
  | exception _ -> 0.0

let tseries =
  let absolute_time =
    Seq.fmap
      (fun row ->
        match Ptime.of_rfc3339 @@ Csv.Row.find row "Date" with
        | Error _ -> None
        | Ok (date, _, _) ->
            let seconds = Ptime.to_float_s date in
            let power = proj_attribute row "ActivePower" in
            let wind_speed = proj_attribute row "WindSpeed" in
            Some (seconds, (power, wind_speed)))
      rows
  in
  let (t0, _) = Seq.head_exn absolute_time in
  (* Shift to t0 *)
  Seq.map (fun (t, data) -> (t -. t0, data)) absolute_time

(* The data is supposed to be sampled at 10mn intervals; we check that it is the case *)
let () =
  assert (
    Seq.for_all2
      (fun (t, _) (t', _) -> t' -. t = 10. *. minute)
      tseries
      (Seq.tail_exn tseries))

let sampling_period = 10. *. minute

(* ------------------------------------------------------------------------- *)

(** {2 Plotting the raw data}

    The data shows unsurprisingly that wind speed and power are highly
    correlated.  *)

let power = map_snd fst tseries

let wind_speed = map_snd snd tseries

let () =
  let open Plot in
  let to_days seconds = seconds /. day in
  let plot ~xaxis ~yaxis ~title ~legend data =
    plot2
      ~xaxis
      ~yaxis
      ~title
      [ Line.line_2d
          ~points:
            (data
            |> Seq.map (fun (s, p) -> Plot.r2 (to_days s) p)
            |> Plot.Data.of_seq)
          ~legend
          () ]
  in
  let wind_speed_plot =
    plot
      ~xaxis:"time (days)"
      ~yaxis:"wind speed (m/s)"
      ~title:"wind speed"
      ~legend:"wind speed (m/s)"
      (wind_speed |> restrict (1. *. month) (2. *. month))
  in
  let power_plot =
    plot
      ~xaxis:"time (days)"
      ~yaxis:"power (kW)"
      ~title:"power"
      ~legend:"power (kW)"
      (power |> restrict (1. *. month) (2. *. month))
  in
  let cumulative_power_plot =
    plot
      ~xaxis:"time (days)"
      ~yaxis:"power (kW)"
      ~title:"cumulative power (full range)"
      ~legend:"cumulative power (kW)"
      (cumulative power)
  in
  Plot.(
    run_matrix
      ~target:
        (png ~pixel_size:(1024, 512 * 3) ~png_file:"plots/raw_data.png" ())
      exec
      [| [| Some wind_speed_plot |];
         [| Some power_plot |];
         [| Some cumulative_power_plot |]
      |])

(** {2 Studying the wind speed and power distributions over each season} *)

(* in days *)
let season_A = ("season_A", (1, 180))

let season_B = ("season_B", (180, 250))

let season_C = ("season_C", (250, 520))

let season_D = ("season_D", (520, 620))

let season_E = ("season_E", (620, 820))

let seasons = [season_A; season_B; season_C; season_D; season_E]

let hist name series =
  let points = series |> Seq.map snd |> Seq.map Plot.r1 |> Plot.Data.of_seq in
  Plot.Histogram.hist ~points ~bins:50 ~legend:name ()

let season_hist name series =
  let open Plot in
  let plot (season_name, (a, b)) =
    let series = series |> restrict (float a *. day) (float b *. day) in
    hist season_name series
  in
  plot2
    ~xaxis:name
    ~yaxis:"freq"
    ~title:(sf "seasonal plot (%s)" name)
    (List.map plot seasons)
  |> run ~target:(png ~png_file:(sf "plots/%s_seasons.png" name) ()) exec

let () =
  let filtered = Seq.filter (fun (_, p) -> p > 0.0) wind_speed in
  season_hist "wind-speed" filtered

let () =
  let filtered = Seq.filter (fun (_, p) -> p > 0.0) power in
  season_hist "power" filtered

(* ------------------------------------------------------------------------- *)

(* Inferred back from looking at the data. Unit: kW *)
let max_power = 1750.

module Wind_to_power () = struct
  (** It is known that the power
      output of a wind turbine is proportional to the cube of the wind speed. *)
  let predicted_power a ws = Float.min max_power (a *. (ws ** 3.))

  (* - restrict the timeseries to a reasonable sub-interval (big enough for
       accurate statistics, not too big to not impair inference speed)
     - filter out elements where output power is negative *)
  let power_and_wind_speed =
    tseries
    (* restrict to window of interest*)
    |> restrict (6. *. month) (6.5 *. month)
    (* project time out *)
    |> Seq.map snd
    (* filter out samples with p <= 0 *)
    |> Seq.filter (fun (p, _) -> p > 0.0)
    (* convert to array for faster iteration *)
    |> Array.of_seq

  open Dagger.Lmh_inference

  let model =
    let open Infix in
    (* We set a reasonably large uniform prior on [a] *)
    let* a = sample @@ Stats_dist.float 3.0 in
    let+ () =
      Array_ops.iter
        (fun (p, ws) ->
          let predicted = predicted_power a ws in
          (* We score the prediction using a gaussian pdfs: the score will be
             maximal around the mean. The bigger the [std], the more lax we are. *)
          score @@ Stats.Pdfs.gaussian ~mean:p ~std:(max_power /. 10.) predicted)
        power_and_wind_speed
    in
    a

  let rng_state = Dagger.RNG.make_self_init ()

  let samples = stream_samples model rng_state

  let samples =
    samples |> Seq.drop 10_000 (* burn-in *) |> Seq.take 10_000 |> List.of_seq

  let () =
    let open Plot in
    run
      ~target:(png ~png_file:"plots/scaling_posterior.png" ())
      exec
      (plot2
         ~xaxis:"a"
         ~yaxis:"freq"
         ~title:"Posterior on scaling parameter"
         [ Histogram.hist
             ~points:(Data.of_list (List.map r1 samples))
             ~bins:50
             () ])

  let inferred_scaling =
    (* take the empirical mean *)
    List.fold_left ( +. ) 0.0 samples /. 10000.

  let () = Format.printf "P(t) = %f x ws(t)^3@." inferred_scaling

  let convert ws = predicted_power inferred_scaling ws

  let () =
    let series =
      tseries |> restrict (4. *. month) ((4. *. month) +. (3. *. day))
    in
    let actual = Seq.map (fun (t, (power, _)) -> Plot.r2 t power) series in
    let predicted =
      Seq.map
        (fun (t, (_, wind_speed)) -> Plot.r2 t (convert wind_speed))
        series
    in
    let open Plot in
    run
      ~target:(png ~png_file:"plots/wind_to_power.png" ())
      exec
      (plot2
         ~xaxis:"t"
         ~yaxis:"p"
         ~title:"Predicted vs actual power output"
         [ Line.line_2d ~points:(Data.of_seq actual) ~legend:"actual" ();
           Line.line_2d ~points:(Data.of_seq predicted) ~legend:"predicted" ()
         ])
end

(* ------------------------------------------------------------------------- *)

module Windowed_fold = struct
  module Queue = CCFQueue
  (* This module implements a sequence transformers implemented
     by folding over the input sequence a function taking
     a sliding window as argument. *)

  type ('x, 'a) state = { samples : 'x Queue.t; acc : 'a }

  let make_transducer f state next_sample =
    let (produced, acc) = f state.samples next_sample state.acc in
    let (samples, _old_sample) = Queue.take_back_exn state.samples in
    let samples = Queue.cons next_sample samples in
    ({ samples; acc }, produced)

  let apply f initial seq =
    let transducer = make_transducer f in
    fold_unfold transducer initial seq

  (* When encoutering a sequence of zeroes, replace by value of sliding average. *)
  let fill_zeroes_with_average window_duration seq =
    let nsamples = int_of_float (window_duration /. sampling_period) in
    let initial =
      { samples = Queue.of_list (List.init nsamples (fun _ -> 0.0));
        acc = `Keep
      }
    in
    apply
      (fun samples next_sample acc ->
        match (next_sample, acc) with
        | (0.0, `Fill avg) -> (avg, acc)
        | (_, `Fill _) -> (next_sample, `Keep)
        | (0.0, `Keep) ->
            let tot = Queue.fold ( +. ) 0.0 samples in
            let avg = tot /. float_of_int (Queue.size samples) in
            (avg, `Fill avg)
        | (x, `Keep) -> (x, `Keep))
      initial
      seq

  let convolute zero add smul kernel seq =
    let nsamples = List.length kernel in
    let initial =
      { samples = Queue.of_list (List.init nsamples (fun _ -> zero)); acc = () }
    in
    let conv samples =
      let samples = Queue.to_list samples in
      List.fold_left add zero (List.map2 smul samples kernel)
    in
    apply (fun samples _next_sample () -> (conv samples, ())) initial seq

  let convolute_float = convolute 0.0 ( +. ) ( *. )
end

(* ------------------------------------------------------------------------- *)

module Model = struct
  module State : sig
    (** The type of states *)
    type t

    (** [make] creates a state. [power, power', power''] are
        the estimates for respectively power, its first and second derivatives.  *)
    val make : power:float -> power':float -> power'':float -> t

    val power : t -> float

    val power' : t -> float

    val power'' : t -> float

    (** Vector space structure *)

    val add : t -> t -> t

    val smul : t -> float -> t

    val zero : t

    (** Evaluating dynamics forward *)

    val forward : t -> span:int -> float

    val forward_cumulative : t -> span:int -> float
  end = struct
    (* Using an array instead of a record makes defining the
       vector space structure more convenient. *)
    type t = Float.Array.t

    let (power, power', power'', dim) = (0, 1, 2, 3)

    let make ~power ~power' ~power'' =
      Float.Array.map_from_array Fun.id [| power; power'; power'' |]

    let power a = Float.Array.get a power

    let power' a = Float.Array.get a power'

    let power'' a = Float.Array.get a power''

    let add a a' = Float.Array.map2 ( +. ) a a'

    let smul a f = Float.Array.map (fun x -> x *. f) a

    let zero = Float.Array.make dim 0.0

    let forward state_t0 ~span =
      let pacc = ref (power state_t0) in
      let pacc' = ref (power' state_t0) in
      for _i = 1 to span do
        pacc := !pacc +. !pacc' ;
        pacc' := !pacc' +. power'' state_t0
      done ;
      !pacc

    let forward_cumulative (state_t0 : t) ~span =
      let pacc = ref (power state_t0) in
      let pacc' = ref (power' state_t0) in
      let cacc = ref 0.0 in
      for _i = 1 to span do
        pacc := !pacc +. !pacc' ;
        pacc' := !pacc' +. power'' state_t0 ;
        cacc := !cacc +. !pacc
      done ;
      !cacc
  end

  module Data = struct
    let start = 1.

    let stop = 300.

    let tick_duration = day

    let ticks_per_day = day /. tick_duration

    (* Restrict the time series to the interval of interest *)
    let power = restrict (day *. start) (day *. stop) power

    (* Coarsen the signal on a per-day basis *)
    let coarsened_power =
      let tick_samples = int_of_float (tick_duration /. sampling_period) in
      let window = Float.Array.make tick_samples 1.0 in
      (* Fill the missing gaps using a 5-day sliding window estimation of the
         average *)
      let gaps_filled =
        power |> Seq.map snd
        |> Windowed_fold.fill_zeroes_with_average (5. *. day)
        |> Float.Array.of_seq
      in
      average gaps_filled window

    let tick_to_day i = float_of_int i *. tick_duration /. day

    let ticks = Seq.unfold (fun i -> Some (i, i + 1)) 1

    let power = Float.Array.to_seq coarsened_power

    (* Coarsened power timeseries with time labelled in ticks *)
    let power_ticks = Seq.zip ticks power

    let power_days = Seq.zip (Seq.map tick_to_day ticks) power
  end

  module Smc = Dagger.Smc_inference.Make (struct
    type particle_output = State.t

    type resampling_state = unit
  end)

  let max_power_per_tick =
    let samples = Data.tick_duration /. sampling_period in
    samples *. max_power

  let forecast_days = 15.

  let forecast_span = int_of_float (forecast_days *. Data.ticks_per_day)

  let rec model (state : State.t) data =
    let open Smc in
    let open Smc.Infix in
    match data () with
    | Seq.Nil -> return ()
    | Seq.Cons (observed_power, rest) ->
        (*
          predict using forward Euler step
         *)
        let power' = State.power' state +. State.power'' state in
        let power = State.power state +. State.power' state in
        (*
          observe
         *)
        let forecast = State.forward state ~span:forecast_span in
        let in_bounds x =
          if x <= max_power_per_tick && x >= 0.0 then 1.0 else 0.0
        in
        let* () =
          (* reject particles with OOB forecast *)
          score (in_bounds forecast)
        in
        let* () =
          (* reject particles with OOB power *)
          score (in_bounds power)
        in
        let dist = observed_power -. power in

        let* () =
          (* condition on the fact that observed power is polluted by
             gaussian noise with given std *)
          score @@ Stats.Pdfs.gaussian ~mean:0.0 ~std:15_000. dist
        in
        let* () =
          (* condition on the fact that forecast can't be too far off the
             current power estmate - the [std] here should be proportional
             to [forecast_span], here we guesstimated it empirically *)
          score (Stats.Pdfs.gaussian ~mean:power ~std:120_000. forecast)
        in
        let* () = yield state in
        let* power'' = sample @@ Stats_dist.gaussian ~mean:0.0 ~std:500. in
        let power'' = power'' in
        let state = State.make ~power ~power' ~power'' in
        model state rest

  let filtered =
    let npart = 1000 in
    let rng_state = Dagger.RNG.make_self_init () in
    let populations =
      Smc.run
        (Dagger.Smc_inference.stratified_resampling ~ess_threshold:0.5)
        ()
        ~npart
        (model State.zero Data.power)
        rng_state
    in
    let normalize total arr =
      let itotal = 1. /. total in
      Array.map (fun (arr, w) -> (arr, w *. itotal)) arr
    in
    Seq.filter_map
      (fun pop ->
        if Array.length pop.Smc.active = 0 then None
        else Some (normalize pop.total_mass pop.active))
      populations
    |> Seq.memoize |> Seq.zip Data.ticks

  (* Statistics on populations *)

  let quantile q pop =
    let open Stats.Fin.Float in
    let mes = of_assoc Stats.float_table pop |> measure in
    Stats.Fin.Float.quantile (module Basic_structures.Std.Float) mes q

  let median pop = quantile 0.5 pop

  let mean pop =
    let open Stats.Fin.Float in
    of_assoc Stats.float_table pop |> measure |> normalize |> as_measure |> mean

  let avg_pop : (State.t * float) array -> State.t =
   fun dist ->
    Array.fold_left
      (fun acc (state, w) -> State.add acc (State.smul state w))
      State.zero
      dist

  let filtered statistics =
    let map f pop = Array.map (fun (state, w) -> (f state, w)) pop in
    Seq.map
      (fun (t, pop) ->
        if Array.length pop = 0 then (t, State.zero)
        else
          let power = map State.power pop |> statistics in
          let power' = map State.power' pop |> statistics in
          let power'' = map State.power'' pop |> statistics in
          (t, State.make ~power ~power' ~power''))
      filtered

  let filtered_mean = filtered mean

  let filtered_median = filtered median

  let filtered_quantile q = filtered (quantile q)

  let filtered_median_averaged =
    let (t, d) = Seq.unzip filtered_median in
    let d =
      Windowed_fold.convolute
        State.zero
        State.add
        State.smul
        (List.init 20 @@ Fun.const (1. /. 20.))
        d
    in
    Seq.zip t d

  let forecast_by_integration s =
    Seq.map
      (fun (tick, state) ->
        let forecast_tick = tick + forecast_span in
        let t = Data.tick_to_day forecast_tick in
        let forecast_power = State.forward state ~span:forecast_span in
        (t, forecast_power))
      s

  let forecast_by_extrapolation =
    Seq.map
      (fun (tick, state) ->
        let forecast_tick = tick + forecast_span in
        let t = Data.tick_to_day forecast_tick in
        let forecast_power = State.power state in
        (t, forecast_power))
      filtered_mean

  let cumulative_forecast_by_integration
      (filtered : (int * State.t) Smc.Seq_ops.t) =
    let filt = Hashtbl.of_seq filtered in
    Seq.map
      (fun (tick, cumu) ->
        let forecast_tick = tick + forecast_span in
        let state_at_tick =
          try Hashtbl.find filt tick
          with Not_found ->
            Format.printf "%d not found@." tick ;
            assert false
        in
        ( forecast_tick,
          cumu +. State.forward_cumulative state_at_tick ~span:forecast_span ))
      (cumulative Data.power_ticks)

  let cumulative_forecast_by_extrapolation
      (filtered : (int * State.t) Smc.Seq_ops.t) =
    let filt = Hashtbl.of_seq filtered in
    Seq.map
      (fun (tick, cumu) ->
        let forecast_tick = tick + forecast_span in
        let state_at_tick =
          try Hashtbl.find filt tick
          with Not_found ->
            Format.printf "%d not found@." tick ;
            assert false
        in
        ( forecast_tick,
          cumu +. (State.power state_at_tick *. float_of_int forecast_span) ))
      (cumulative Data.power_ticks)

  let black = Plot.Style.(default |> set_color Plot.Color.black)

  let green = Plot.Style.(default |> set_color Plot.Color.green)

  let blue = Plot.Style.(default |> set_color Plot.Color.blue)

  let red = Plot.Style.(default |> set_color Plot.Color.red)

  let green_squares =
    Plot.Style.(
      default
      |> set_color Plot.Color.(rgb 0.0 0.8 0.0)
      |> set_point ~ptyp:Plot.Pointtype.square ~psize:0.4)

  let blue_squares =
    Plot.Style.(
      default
      |> set_color Plot.Color.(rgb 0.0 0.0 0.8)
      |> set_point ~ptyp:Plot.Pointtype.square ~psize:0.4)

  let red_squares =
    Plot.Style.(
      default |> set_color Plot.Color.red
      |> set_point ~ptyp:Plot.Pointtype.square ~psize:1.0)

  let red_delta =
    Plot.Style.(
      default |> set_color Plot.Color.red
      |> set_point ~ptyp:Plot.Pointtype.delta ~psize:1.0)

  let black_disks =
    Plot.Style.(
      default |> set_color Plot.Color.black
      |> set_point ~ptyp:Plot.Pointtype.box ~psize:1.5)

  let to_days seq = map_fst Data.tick_to_day seq

  let plots =
    [ ("data", Data.power_days, black_disks);
      ("filt-mean", filtered_mean |> map_snd State.power |> to_days, green);
      ( "filt-0.25",
        filtered_quantile 0.25 |> map_snd State.power |> to_days,
        green_squares );
      ( "filt-0.75",
        filtered_quantile 0.75 |> map_snd State.power |> to_days,
        green_squares );
      ("forecast-int", forecast_by_integration filtered_mean, blue);
      ( "forecast-int-0.25",
        filtered_quantile 0.25 |> forecast_by_integration,
        blue_squares );
      ( "forecast-int-0.75",
        filtered_quantile 0.75 |> forecast_by_integration,
        blue_squares );
      ("forecast-ext", forecast_by_extrapolation, red_squares) ]

  let cumulative_power = cumulative Data.power_ticks

  let cumulative_forecast_by_integration s =
    cumulative_forecast_by_integration s

  let cumulative_forecast_by_integration_filtered_median_averaged =
    cumulative_forecast_by_integration filtered_median_averaged

  let cumulative_forecast_by_extrapolation_filtered_mean =
    cumulative_forecast_by_extrapolation filtered_mean

  let cumulative_forecast_by_extrapolation_filtered_median_averaged =
    cumulative_forecast_by_extrapolation filtered_median_averaged

  let rmse (truth : (int * float) Seq.t) (forecast : (int * float) Seq.t) =
    let t = Hashtbl.of_seq truth in
    let (acc, count) =
      Seq.fold_left
        (fun (acc, count) (i, x) ->
          match Hashtbl.find_opt t i with
          | None -> (acc, count)
          | Some y -> (acc +. ((x -. y) ** 2.), count + 1))
        (0.0, 0)
        forecast
    in
    sqrt @@ (acc /. float_of_int count)

  let cumulative_plots =
    [ ("cumulative", cumulative_power |> to_days, black);
      ( "cumu-forecast-mean",
        cumulative_forecast_by_integration filtered_mean |> to_days,
        blue );
      ( "cumu-forecast-0.25",
        filtered_quantile 0.25 |> cumulative_forecast_by_integration |> to_days,
        blue_squares );
      ( "cumu-forecast-0.75",
        filtered_quantile 0.75 |> cumulative_forecast_by_integration |> to_days,
        blue_squares ) ]

  let errors =
    [ ( "filt-mean",
        rmse cumulative_power
        @@ cumulative_forecast_by_integration filtered_mean );
      ( "filt-med-avg",
        rmse
          cumulative_power
          cumulative_forecast_by_integration_filtered_median_averaged );
      ( "filt-mean-ex",
        rmse cumulative_power cumulative_forecast_by_extrapolation_filtered_mean
      );
      ( "filt-med-avg-ex",
        rmse cumulative_power cumulative_forecast_by_extrapolation_filtered_mean
      ) ]

  let () =
    let errors = List.sort (fun (_, x) (_, y) -> Float.compare x y) errors in
    List.iter (fun (s, e) -> Format.printf "%s: %f@." s e) errors

  let () =
    let plot (legend, seq, style) =
      Plot.Line.line_2d
        ~points:(Plot.Data.of_seq (seq |> Seq.map Plot.tup_r2))
        ~legend
        ~style
        ()
    in
    let prepare_plots timeseries =
      let xtics =
        let open Plot.Tics in
        default
        |> set_position ~start:0.0 ~incr:5.0
        |> set_rotate ~degrees:(-90.)
      in
      Plot.plot2 ~xaxis:"t" ~yaxis:"kW" ~xtics (List.map plot timeseries)
    in
    let plot = prepare_plots plots in
    let cumulative = prepare_plots cumulative_plots in
    Plot.(
      run_matrix
        ~target:(png ~pixel_size:(1920, 1080) ~png_file:"plots/forecast.png" ())
        exec
        [| [| Some plot |]; [| Some cumulative |] |])

  (* let () =
   *   let prepare_plot timeseries predicted =
   *     let predicted = List.of_seq predicted in
   *     let series = Seq.map (fun (t, c) -> Plot.r2 t c) timeseries in
   *     let mean = List.map (fun (t, (mean, _, _)) -> Plot.r2 t mean) predicted in
   *     let low = List.map (fun (t, (_, low, _)) -> Plot.r2 t low) predicted in
   *     let high = List.map (fun (t, (_, _, high)) -> Plot.r2 t high) predicted in
   *     let xtics =
   *       let open Plot.Tics in
   *       default
   *       |> set_position ~start:0.0 ~incr:5.0
   *       |> set_rotate ~degrees:(-90.)
   *     in
   *     let data = Plot.Style.(default |> set_color Plot.Color.black) in
   *     let inferred = Plot.Style.(default |> set_color Plot.Color.blue) in
   *     Plot.plot2
   *       ~xaxis:"t"
   *       ~yaxis:"kW"
   *       ~xtics
   *       [ Plot.Line.line_2d
   *           ~points:(Plot.Data.of_seq series)
   *           ~legend:"data"
   *           ~style:data
   *           ();
   *         Plot.Line.line_2d
   *           ~points:(Plot.Data.of_list mean)
   *           ~legend:"mean"
   *           ~style:inferred
   *           ();
   *         Plot.Line.line_2d
   *           ~points:(Plot.Data.of_list low)
   *           ~legend:"low"
   *           ~style:inferred
   *           ();
   *         Plot.Line.line_2d
   *           ~points:(Plot.Data.of_list high)
   *           ~legend:"hi"
   *           ~style:inferred
   *           () ]
   *   in
   *   let power = prepare_plot timeseries predicted_from_median in
   *   let forecast = prepare_plot timeseries forecast in
   *   Plot.(
   *     run_matrix
   *     (\* ~target:(png ~pixel_size:(1280, 1280) ~png_file:"/tmp/forecast.png" ()) *\)
   *       ~target:(qt ~pixel_size:(1080, 1080) ())
   *       exec
   *       [| [| Some power |]; (\* [| None |] *\)
   *                            [| Some forecast |] |]) *)
end

let main = ()
