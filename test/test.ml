open Dagger_tests

let conv qctests = List.map QCheck_alcotest.to_alcotest qctests

let () =
  Alcotest.run
    "lmh"
    [ ("lmh", conv Basic_consistency.Lmh.tests);
      ("lmh-incremental", conv Basic_consistency.Lmh_incremental.tests);
      ("smc-systematic", conv Basic_consistency.Smc_systematic.tests);
      ("smc-stratified", conv Basic_consistency.Smc_stratified.tests);
      ("biased-coin", conv Biased_coin.tests);
      ("linear-regression", conv Linear_regression.tests);
      ("diffusions", conv Diffusions.tests);
      ("sprinkler", conv Sprinkler.tests);
      ("smc-resampling", conv Resampling_test.tests) ]
