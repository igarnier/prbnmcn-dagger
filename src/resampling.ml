module type Particles = sig
  type p

  type o

  type r

  val iter : (p -> r -> unit) -> unit

  val fold : ('acc -> p -> r -> 'acc) -> 'acc -> 'acc

  val get_output : p -> o option

  val get_score : p -> r

  val append : p -> r -> unit

  val total : unit -> r

  val size : unit -> int

  val ess : unit -> r
end

type ('o, 'r) particles = (module Particles with type o = 'o and type r = 'r)

type ('o, 'r, 'resampling_state) strategy =
  target_size:int ->
  ('o, 'r) particles ->
  'resampling_state ->
  RNG.t ->
  'resampling_state

module Make_predefined
    (F : Intf.Field) (Sampler : sig
      val uniform : F.t -> RNG.t -> F.t
    end) =
struct
  let resampling_generic_iterative (type o) f ((module P) : (o, F.t) particles)
      (rng_state : RNG.t) =
    let cumulative = ref F.zero in
    let partition_index = ref 1 in
    let last = ref (f !partition_index rng_state) in
    let tot = P.total () in
    let res_w = F.div tot (F.of_int (P.size ())) in
    P.iter (fun particle w ->
        cumulative := F.add !cumulative w ;
        while F.(!last < !cumulative) do
          P.append particle res_w ;
          last := f !partition_index rng_state ;
          incr partition_index
        done)

  let in_unit_interval r = F.(zero <= r && r <= one)

  let stratified_resampling (type o) ~ess_threshold ~target_size
      ((module P) as particles : (o, F.t) particles) resampling_state rng_state
      =
    if P.size () < 2 then invalid_arg "stratified_resampling (P.size < 2)" ;
    if target_size < 2 then invalid_arg "stratified_resampling (card < 2)" ;
    if not (in_unit_interval ess_threshold) then
      invalid_arg "stratified_resampling (ess_threshold not in [0;1])" ;
    let ess = F.div (P.ess ()) (F.of_int (P.size ())) in
    if F.(ess < ess_threshold) then
      let tot = P.total () in
      let inv = F.div tot (F.of_int target_size) in
      resampling_generic_iterative
        (fun i rng_state ->
          let rand = Sampler.uniform inv rng_state in
          F.add F.(div (mul tot (of_int i)) (of_int target_size)) rand)
        particles
        rng_state
    else () ;
    resampling_state

  let systematic_resampling (type o) ~ess_threshold ~target_size
      ((module P) as particles : (o, F.t) particles) resampling_state rng_state
      =
    if P.size () < 2 then invalid_arg "systematic_resampling (P.size < 2)" ;
    if target_size < 2 then
      invalid_arg "systematic_resampling (target_size < 2)" ;
    if not (in_unit_interval ess_threshold) then
      invalid_arg "systematic_resampling (ess_threshold not in [0;1])" ;
    let ess = F.div (P.ess ()) (F.of_int (P.size ())) in
    if F.(ess < ess_threshold) then
      let tot = P.total () in
      let inv = F.div tot (F.of_int target_size) in
      let rand = Sampler.uniform inv rng_state in
      resampling_generic_iterative
        (fun i _rng_state ->
          F.add F.(div (mul tot (of_int i)) (of_int target_size)) rand)
        particles
        rng_state
    else () ;
    resampling_state
end
[@@inline]
