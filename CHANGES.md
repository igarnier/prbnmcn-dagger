## 0.0.3

### API changes
- smc: provide API for programmable resampling (review by @nilsbecker, many thanks!)
- gsl_dist: functorize over GSL signature
  - band-aid for unavailability of `gsl` package in ocaml 5
  - since explicit dependency on `gsl` is dropped, relicensed to MIT

### code quality/bug fixes
- smc: much improved documentation (report and review by @nilsbecker)
- removed uses of `Obj.magic`
- bugfix: properly handle terminated particles in smc
  - terminated particles now continue to participate in resampling events
- bugfix: `Smc.fork` off-by-one corrected
- bugfix: `Cps_monad.map_array` reversed order of elements

### misc
- added gamma distribution to `stats_dist`
- added kalman filter example
- added polynomial regression example
- added approximate bayesian computation example
- more tests

## 0.0.2
- Dependency: `prbnmcn-stats.0.0.3` -> `prbnmcn-stats.0.0.4`
- Add beta distribution to Gsl samplers
- Refactor Cps monad
- Add SMC inference
- Simplify handler type, modularize effect definitions away from Cps_monad
- Fix typo: bernouilli -> bernoulli (report by @nilsbecker)

## 0.0.1
- First release of `prbnmcn-dagger`, a library for probabilistic programming
