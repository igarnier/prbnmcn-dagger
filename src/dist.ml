type 'a dist = { sample : RNG.t -> 'a; ll : 'a -> Log_space.t }

type 'a kernel =
  { start : 'a; sample : 'a -> RNG.t -> 'a; ll : 'a -> 'a -> Log_space.t }

type 'a t = Stateless of 'a dist | Kernel of 'a kernel

let stateless sample ll = Stateless { sample; ll } [@@inline]

let kernel start sample ll = Kernel { start; sample; ll } [@@inline]

let dist0 sampler log_pdf = stateless sampler log_pdf [@@inline]

let dist1 sampler log_pdf arg =
  stateless (fun rng_state -> sampler arg rng_state) (fun x -> log_pdf arg x)
  [@@inline]

let dist2 sampler log_pdf arg1 arg2 =
  stateless
    (fun rng_state -> sampler arg1 arg2 rng_state)
    (fun x -> log_pdf arg1 arg2 x)
  [@@inline]

let kernel1 sampler log_pdf start arg =
  kernel
    start
    (fun x rng_state -> sampler arg x rng_state)
    (fun x y -> log_pdf arg x y)
  [@@inline]

let iid n dist =
  match dist with
  | Stateless dist ->
      let sample rng_state = Array.init n (fun _i -> dist.sample rng_state) in
      let ll arr =
        let acc = ref Log_space.one in
        for i = 0 to Array.length arr - 1 do
          acc := Log_space.mul (dist.ll arr.(i)) !acc
        done ;
        !acc
      in
      Stateless { sample; ll }
  | Kernel k ->
      let start = Array.make n k.start in
      let sample x rng_state =
        Array.init n (fun i -> k.sample x.(i) rng_state)
      in
      let ll x y =
        assert (Array.length x = Array.length y) ;
        let acc = ref Log_space.one in
        for i = 0 to Array.length x - 1 do
          acc := Log_space.mul (k.ll x.(i) y.(i)) !acc
        done ;
        !acc
      in
      Kernel { start; sample; ll }

let conv (type a b) (f : a -> b) (g : b -> a) (s : a t) : b t =
  match s with
  | Kernel k ->
      Kernel
        { sample = (fun x rng -> f (k.sample (g x) rng));
          ll = (fun x y -> k.ll (g x) (g y));
          start = f k.start
        }
  | Stateless d ->
      Stateless
        { sample = (fun rng -> f (d.sample rng)); ll = (fun x -> d.ll (g x)) }
