(lang dune 2.8)
(name prbnmcn-dagger)
(version 0.0.5)
(generate_opam_files true)
(source (uri git+https://github.com/igarnier/prbnmcn-dagger))
(bug_reports http://github.com/igarnier/prbnmcn-dagger)
(homepage http://github.com/igarnier/prbnmcn-dagger)
(authors "Ilias Garnier")
(maintainers "igarnier@protonmail.com")
(use_standard_c_and_cxx_flags true)
(implicit_transitive_deps false)

(package
 (name prbnmcn-dagger)
 (synopsis "Probabilistic programming library")
 (description "A library for probabilistic programming. It takes the form of a signature for a monadic language exposing sampling and scoring primitives together with inference algorithms implementing that signature, including:
- sequential Monte Carlo (SMC)
- single-site lightweight Metropolis-Hastings")
 (license MIT)
 (tags (statistics))
 (depends
  (ocaml (>= 5.0.0))
  (domainslib (>= 0.5))
  (prbnmcn-cgrph (= 0.0.2))))

(package
 (name prbnmcn-dagger-gsl)
 (synopsis "Probabilistic programming library: GSL-based samplers")
 (description "dagger probabilistic programming library: GSL samplers")
 (license MIT)
 (depends
  (prbnmcn-dagger (= :version))))

(package
 (name prbnmcn-dagger-stats)
 (synopsis "Probabilistic programming library: prbnmcn-stats-based samplers")
 (description "dagger probabilistic programming library: prbnmcn-stats samplers")
 (license MIT)
 (depends
  (prbnmcn-dagger (= :version))
  (prbnmcn-stats (>= 0.0.8))))

(package
 (name prbnmcn-dagger-test)
 (synopsis "Probabilistic programming library: tests")
 (description "dagger probabilistic programming library: tests")
 (license MIT)
 (depends
  (prbnmcn-dagger (= :version))
  (prbnmcn-stats (>= 0.0.8))
  (qcheck (>= 0.17))
  (qcheck-alcotest (>= 0.18.1))
  (prbnmcn-dagger-gsl (= :version))
  (prbnmcn-dagger-stats (= :version))
  (prbnmcn-gnuplot (= 0.0.5))))
