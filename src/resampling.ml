module type Particle = sig
  type t

  type field

  val weight : t -> field
end

module type S = sig
  type field

  type particle

  type 'a t := 'a Stateful_sampling_monad.t

  val resampling_generic_iterative :
    particle array -> (int -> field) -> int array

  val resampling_generic : particle array -> (int -> field t) -> int array t

  val resampling_generic_list :
    (int -> field t) ->
    (particle -> int -> 'a -> 'a) ->
    particle list ->
    'a ->
    'a t

  val stratified_resampling : target:int -> particle array -> int array t

  val stratified_resampling_list :
    target:int -> (particle -> int -> 'a -> 'a) -> particle list -> 'a -> 'a t

  val systematic_resampling : target:int -> particle array -> int array t

  val systematic_resampling_list :
    target:int -> (particle -> int -> 'a -> 'a) -> particle list -> 'a -> 'a t
end

module Make
    (F : Intf.Field)
    (P : Particle with type field = F.t) (Sampler : sig
      val uniform : F.t -> F.t Stateful_sampling_monad.t
    end) =
struct
  type field = F.t

  type particle = P.t

  type 'a t = 'a Stateful_sampling_monad.t

  (* Both stratified and systematic resampling are implemented through the
     same generic function. *)

  (* The [f] function is supposed to return the next "noisy" quantile boundary. *)
  let resampling_generic_iterative (dist : P.t array) f =
    let particles = Array.length dist in
    let replication_counts = Array.make particles 0 in
    let cumulative = ref F.zero in
    let particle_index = ref 0 in
    let partition_index = ref 1 in
    let last = ref (f !partition_index) in

    while !particle_index < particles do
      cumulative :=
        F.add !cumulative (P.weight (Array.get dist !particle_index)) ;

      while F.(!last < !cumulative) do
        let c = replication_counts.(!particle_index) in
        replication_counts.(!particle_index) <- c + 1 ;
        last := f !partition_index ;
        incr partition_index
      done ;

      incr particle_index
    done ;
    replication_counts

  let resampling_generic_list (f : int -> F.t t) cons pop acc =
    let open Stateful_sampling_monad.Infix in
    let rec particle_loop particles partition_index cumulative last acc =
      match particles with
      | [] -> return acc
      | particle :: rest ->
          let w = P.weight particle in
          if F.(w = zero) then
            particle_loop rest partition_index cumulative last acc
          else
            let cumulative = F.add cumulative w in
            counting_loop particle rest 0 partition_index cumulative last acc
    and counting_loop particle rest replication_count partition_index cumulative
        last acc =
      if F.(last < cumulative) then
        let replication_count = replication_count + 1 in
        let* last = f partition_index in
        let partition_index = partition_index + 1 in
        counting_loop
          particle
          rest
          replication_count
          partition_index
          cumulative
          last
          acc
      else
        particle_loop
          rest
          partition_index
          cumulative
          last
          (cons particle replication_count acc)
    in
    let* last = f 1 in
    particle_loop pop 1 F.zero last acc

  let resampling_generic (dist : P.t array) (f : int -> F.t t) =
    let open Stateful_sampling_monad.Infix in
    let particles = Array.length dist in
    let replication_counts = Array.make particles 0 in

    let rec particle_loop particle_index partition_index cumulative last =
      if particle_index < particles then
        let cumulative =
          F.add cumulative (P.weight (Array.get dist particle_index))
        in
        counting_loop particle_index partition_index cumulative last
      else return replication_counts
    and counting_loop particle_index partition_index cumulative last =
      if F.(last < cumulative) then (
        let c = replication_counts.(particle_index) in
        replication_counts.(particle_index) <- c + 1 ;
        let* last = f partition_index in
        let partition_index = partition_index + 1 in
        counting_loop particle_index partition_index cumulative last)
      else
        let particle_index = particle_index + 1 in
        particle_loop particle_index partition_index cumulative last
    in
    let* last = f 1 in
    particle_loop 0 1 F.zero last

  let stratified_resampling ~target mu =
    if target < 2 then invalid_arg "stratified_resampling" ;
    let open Stateful_sampling_monad.Infix in
    let tot = Array.fold_left (fun acc p -> F.add (P.weight p) acc) F.zero mu in
    let inv = F.div tot (F.of_int target) in
    resampling_generic mu (fun i ->
        let* rand = Sampler.uniform inv in
        return (F.add F.(div (mul tot (of_int i)) (of_int target)) rand))

  let systematic_resampling ~target mu =
    if target < 2 then invalid_arg "systematic_resampling" ;
    let open Stateful_sampling_monad.Infix in
    let tot = Array.fold_left (fun acc p -> F.add (P.weight p) acc) F.zero mu in
    let inv = F.div tot (F.of_int target) in
    let* rand = Sampler.uniform inv in
    resampling_generic mu (fun i ->
        return (F.add F.(div (mul tot (of_int i)) (of_int target)) rand))

  let stratified_resampling_list ~target cons pop acc =
    if target < 2 then invalid_arg "stratified_resampling" ;
    let open Stateful_sampling_monad.Infix in
    let tot = List.fold_left (fun acc p -> F.add (P.weight p) acc) F.zero pop in
    let inv = F.div tot (F.of_int target) in
    resampling_generic_list
      (fun i ->
        let* rand = Sampler.uniform inv in
        return (F.add F.(div (mul tot (of_int i)) (of_int target)) rand))
      cons
      pop
      acc

  let systematic_resampling_list ~target cons pop acc =
    if target < 2 then invalid_arg "systematic_resampling" ;
    let open Stateful_sampling_monad.Infix in
    let tot = List.fold_left (fun acc p -> F.add (P.weight p) acc) F.zero pop in
    let inv = F.div tot (F.of_int target) in
    let* rand = Sampler.uniform inv in
    resampling_generic_list
      (fun i ->
        return (F.add F.(div (mul tot (of_int i)) (of_int target)) rand))
      cons
      pop
      acc
end
[@@inline]

module Float_field : Intf.Field with type t = float = struct
  type t = float

  let add = ( +. )

  let sub = ( -. )

  let mul = ( *. )

  let div = ( /. )

  let zero = 0.0

  let one = 1.0

  let of_int = float_of_int

  let ( = ) (x : float) (y : float) = x = y [@@inline]

  let ( < ) (x : float) (y : float) = x < y [@@inline]
end

module Make_float (P : Particle with type field = float) =
  Make (Float_field) (P)
    (struct
      let uniform x rng_state = RNG.float rng_state x
    end)
