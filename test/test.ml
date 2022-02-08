open Dagger_tests

let () =
  QCheck_runner.run_tests_main
    (List.concat
       [ Basic_consistency.Lmh.tests;
         Basic_consistency.Lmh_incremental.tests;
         Biased_coin.tests;
         Linear_regression.tests;
         Diffusions.tests;
         Sprinkler.tests ])
