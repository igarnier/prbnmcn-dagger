# dagger<sup>†</sup>: a probabilistic programming library

The **dagger** library for probabilistic programming in OCaml.
Currently implements:
- Single-site Metropolis-Hastings, a.k.a. [lightweight Metropolis-Hastings][lightweight-link]
- Incrementalized single-site MH, similar to that implemented in [Hakaru10][hakaru-link]
- Sequential Monte-Carlo, with systematic and stratified resampling plus facilities
  for defining custom resampling strategies.

The main package is `prbnmcn-dagger`. Packages  `prbnmcn-dagger-gsl` and  `prbnmcn-dagger-stats`
provide distributions implemented respectively through the GSL (GPL-licensed) and
`prbnmcn-stats` (MIT-licensed).

Look no further for the [documentation][doc-link] and the examples it contains.

Some more examples are made available in the `examples` subdirectory. For now, you'll find:
- an implementation of a 2d ising model and a toy study of its behaviour around its critical temperature
- a basic kalman filter example
- an experiment on forecasting wind power production using an ad-hoc Kalman filter
- an implementation of the algorithm described in [Adaptive approximate Bayesian computation][abc-paper-link] by Beaumont et al.

## Contributing

Contributions and issue reports are welcome. Development currently happen on https://gitlab.com/igarnier/monorepo/ but
I can take care of cherry-picking pull requests submitted here.

## Trivia

The name **dagger** refers to two things:
- a good mathematical framework for giving a semantics to probabilistic
  programming is a certain [dagger category][dagger-cat-link] of Markov
  kernels, see eg [this][paper-1-link] or [that][paper-2-link] paper;
  Bayesian inversion corresponds to a particular symmetry of a mathematical
  structure and this symmetry is denoted using the † symbol.
- the underlying representation of the probabilistic model when using the
  incrementalized backend is as a directed acyclic graph (ie a DAG, which
  sounds exactly like the French translation of dagger)

[lightweight-link]: https://web.stanford.edu/~ngoodman/papers/lightweight-mcmc-aistats2011.pdf
[hakaru-link]: http://okmij.org/ftp/kakuritu/Hakaru10/
[dagger-cat-link]: https://ncatlab.org/nlab/show/dagger+category
[paper-1-link]: https://www.sciencedirect.com/science/article/pii/S1571066118300860
[paper-2-link]: https://hal.archives-ouvertes.fr/hal-01429663v2
[doc-link]: https://igarnier.github.io/prbnmcn-dagger/
[abc-paper-link]: https://arxiv.org/pdf/0805.2256.pdf
