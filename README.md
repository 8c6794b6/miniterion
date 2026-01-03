# Miniterion

[![ci][ci-badge-svg]][ci-badge-link]
[![codecov][codecov-badge-svg]][codecov-badge-link]
[![Hackage][hackage-badge]][hackage-package]
[![Stackage LTS][stackage-lts-badge]][stackage-lts-package]
[![Stackage nightly][stackage-nightly-badge]][stackage-nightly-package]


## Summary

Miniterion is a lightweight Haskell cabal package containing utilities
for writing benchmark codes. The package has an API subset of
[`criterion`][criterion] package, so switching to other benchmarking
packages ([`criterion`][criterion], [`gauge`][gauge], and
[`tasty-bench`][tasty-bench]) should be easily done.

As in `criterion`, the executable built with the `defaultMain`
supports selecting the running benchmarks with prefix match, case
insensitive prefix match, substring match, or glob pattern match via
the command line option. The executable has options to write CSV
summary, JSON summary, and HTML report. Invoke the benchmark
executable with `--help` option to see other available options.


## Motivation

The goal of the miniterion package is to have a reasonably useful and
lightweight benchmarking utility with a small amount of maintenance
effort. For robust and feature-rich benchmarking utility, use the
other packages mentioned above.

The miniterion package is designed to have a small number of package
dependencies. At the time of writing, the direct dependency packages
are only two: `base` and `deepseq`. The miniterion package does not
have rich features, but compared to other benchmarking packages, the
package and benchmark executable should compile faster, and the
resulting benchmark executable should be smaller.


## Example

The following shows a simple benchmark with a naive Fibonacci
function.

In cabal configuration:

```
benchmark fibo
    default-language: Haskell2010
    type:             exitcode-stdio-1.0
    hs-source-dirs:   bench
    main-is:          Main.hs
    build-depends:    base
                    , miniterion
```

And in file `bench/Main.hs`:

```haskell
module Main where

import Miniterion

fib :: Int -> Int
fib m | m < 0 = error "negative!"
      | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n-1) + go (n-2)

main :: IO ()
main = defaultMain [
  bgroup "fib" [ bench "1" $ whnf fib 1
               , bench "5" $ whnf fib 5
               , bench "9" $ whnf fib 9
               , bench "11" $ whnf fib 11
               ]
  ]
```

then compile and run the benchmark with `cabal bench`:

```console
$ cabal bench
Build profile: -w ghc-9.12.2 -O1
In order, the following will be built (use -v for more details):
 - miniterion-0.1.2.0 (bench:fibo) (first run)
Preprocessing benchmark 'fibo' for miniterion-0.1.2.0...
Building benchmark 'fibo' for miniterion-0.1.2.0...
Running 1 benchmarks...
Benchmark fibo: RUNNING...
benchmarking fib/1
time                 4.954 ns   (4.953 ns .. 4.954 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.957 ns   (4.947 ns .. 4.961 ns)
std dev              12.00 ps   (12.00 ps .. 944.2 μs)

benchmarking fib/5
time                 39.93 ns   (39.88 ns .. 39.98 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 39.93 ns   (39.73 ns .. 40.62 ns)
std dev              780.0 ps   (780.0 ps .. 943.8 μs)

benchmarking fib/9
time                 325.6 ns   (325.5 ns .. 325.6 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 324.9 ns   (324.7 ns .. 325.1 ns)
std dev              370.0 ps   (370.0 ps .. 944.1 μs)

benchmarking fib/11
time                 852.6 ns   (852.3 ns .. 852.9 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 854.2 ns   (852.1 ns .. 856.3 ns)
std dev              4.072 ns   (4.072 ns .. 944.6 μs)


Benchmark fibo: FINISH
```

Run:

```console
$ cabal run -- fibo --help
```

to see the help message.

<!-- links -->

[ci-badge-svg]: https://github.com/8c6794b6/miniterion/actions/workflows/ci.yml/badge.svg
[ci-badge-link]: https://github.com/8c6794b6/miniterion/actions/workflows/ci.yml

[codecov-badge-svg]: https://codecov.io/github/8c6794b6/miniterion/graph/badge.svg
[codecov-badge-link]: https://codecov.io/github/8c6794b6/miniterion

[hackage-badge]: http://img.shields.io/hackage/v/miniterion.svg
[hackage-package]: http://hackage.haskell.org/package/miniterion

[stackage-lts-badge]: http://stackage.org/package/miniterion/badge/lts
[stackage-lts-package]: http://stackage.org/lts/package/miniterion

[stackage-nightly-badge]: http://stackage.org/package/miniterion/badge/nightly
[stackage-nightly-package]: http://stackage.org/nightly/package/miniterion

[criterion]: http://hackage.haskell.org/package/criterion
[gauge]: http://hackage.haskell.org/package/gauge
[tasty-bench]: http://hackage.haskell.org/package/tasty-bench
