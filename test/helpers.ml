open Stats

(* This environment variable should be set to true whenever artifacts should be produced
   when running the tests. *)
let test_artifacts = "DAGGER_TEST_ARTIFACTS"

let produce_artifacts =
  try
    ignore (Sys.getenv test_artifacts) ;
    true
  with Not_found -> false

let histogram empirical =
  empirical
  |> Binning.(
       from_empirical
         (regular ~origin:0.0 ~width:0.1 ~truncate:(Some (-20., 100.))))
  |> Fin.Float.normalize |> Fin.Float.as_measure

let to_fin_mes ?oracle_pdf empirical =
  match oracle_pdf with
  | None ->
      let normalized_result = histogram empirical in
      (normalized_result, None)
  | Some pdf ->
      let normalized_result = histogram empirical in
      let (`Measure raw_data) = Fin.Float.list_of_measure normalized_result in
      let raw_data = Array.of_list raw_data in
      let raw_data =
        Array.map
          (fun (i, _mass) ->
            let leb_mes = 0.1 in
            let low = float_of_int i *. 0.1 in
            let hi = float_of_int (i + 1) *. 0.1 in
            let den_at_bucket = pdf ((low +. hi) /. 2.) in
            (i, leb_mes *. den_at_bucket))
          raw_data
      in
      ( normalized_result,
        Some Fin.Float.(measure @@ of_assoc int_table raw_data) )

let plot png_file samples =
  let hist =
    Plot.plot2 ~xaxis:"x" ~yaxis:"freq" ~title:"test"
    @@ List.map
         (fun (samples, legend) ->
           Plot.Histogram.hist
             ~points:(Plot.Data.of_seq @@ Seq.map Plot.r1 @@ samples)
             ~bins:100
             ~legend
             ())
         samples
  in
  Plot.run ~target:(Plot.png ~png_file ()) Plot.exec hist

let plot_binned png_file (l : ((int, float) Stats_intf.fin_mes * string) list) =
  let l =
    List.map
      (fun (m, legend) ->
        let (`Measure points) = Stats.Fin.Float.list_of_measure m in
        ( List.map (fun (i, x) -> (float_of_int i, x)) points
          |> List.sort (fun (i, _) (j, _) -> Float.compare i j)
          |> List.to_seq
          |> Seq.map (fun (x, y) -> Plot.r2 x y),
          legend ))
      l
  in
  let hist =
    Plot.plot2 ~xaxis:"x" ~yaxis:"freq" ~title:"test"
    @@ List.map
         (fun (samples, legend) ->
           Plot.Line.line_2d ~points:(Plot.Data.of_seq @@ samples) ~legend ())
         l
  in
  Plot.run ~target:(Plot.png ~png_file ()) Plot.exec hist

(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Simon Cruanes                                          *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
let rec take_aux n xs =
  let open Seq in
  if n = 0 then empty
  else fun () ->
    match xs () with Nil -> Nil | Cons (x, xs) -> Cons (x, take_aux (n - 1) xs)

let take n xs =
  if n < 0 then invalid_arg "Seq.take" ;
  take_aux n xs

let rec force_drop n xs =
  let open Seq in
  match xs () with
  | Nil -> Nil
  | Cons (_, xs) ->
      let n = n - 1 in
      if n = 0 then xs () else force_drop n xs

let drop n xs =
  if n < 0 then invalid_arg "Seq.drop"
  else if n = 0 then xs
  else fun () -> force_drop n xs
