# TODO

## Inference

- HMC, etc

## Benchmarks/Optimizations

- `lmh_incremental`: benchmark implementation of trace against other implementations
  of integer sets
- `lmh_incremental`: can we do without `undo`?
- `smc`: can we perform resampling directly in log space?

## Examples

### From http://okmij.org/ftp/kakuritu/Hakaru10/

- implement 'colored balls'
- implement 'Bird migration problem'

## Distributions

- Provide an `Owl`-backed set of samplers

## Misc

- Resampling wrt dirac distributions?
- More random walks (Levy processes, discrete random walks - eg on graphs, etc)

## Tests

- Change printf into proper logging (to be active when `DAGGER_TEST_ARTIFACTS` is set)
- Improve resampling coverage (esp for population count = 1)

## Error handling

- Some `assert`s deserve to be cleaner errors in `smc_inference`
