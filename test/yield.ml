open Dagger.Smc_inference
module Smc = Unit_smc

let rec loop n acc =
  let open Smc in
  let open Infix in
  if n = 0 then
    let count = List.fold_left (fun c b -> if b then c + 1 else c) 0 acc in
    return (float_of_int count)
  else
    let* b = sample @@ Stats_dist.bernoulli ~bias:0.5 in
    if b then
      let* () = yield () in
      loop (n - 1) (b :: acc)
    else loop (n - 1) (b :: acc)

let rng_state = Random.State.make [| 149572; 3891981; 3847844 |]

let run () =
  let outcome =
    Smc.run
      (systematic_resampling ~ess_threshold:0.5)
      ()
      ~npart:10_000
      (loop 10 [])
      rng_state
  in
  let average_true =
    outcome
    |> Seq.filter_map (fun (pop : _ Smc.population) ->
           if Array.length pop.active = 0 then
             let avg =
               Array.fold_left
                 (fun acc (x, y) -> Float.fma x y acc)
                 0.0
                 pop.terminated
             in
             Some (avg /. pop.total_mass)
           else None)
    |> List.of_seq |> List.hd
  in
  let err = abs_float (average_true -. 5.0) in
  if err >. 0.015 then Format.kasprintf failwith "error: %f@." err else ()

let tests =
  [ ( QCheck.Test.make ~name:"non-uniform-yield" ~count:1 QCheck.unit
    @@ fun () ->
      run () ;
      true ) ]
