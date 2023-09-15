# Miniterion

[![ci][ci-badge-svg]][ci-badge-link]
[![Hackage][hackage-badge]][hackage-package]
[![Stackage LTS][stackage-lts-badge]][stackage-lts-package]

## Summary

Miniterion is a lightweight Haskell cabal package containing utilities
for writing benchmark codes. The package has an API subset of
[`criterion`](criterion) package, so switching to other benchmark
packages ([`criterion`](criterion), [`gauge`](gauge), and
[`tasty-bench`](tasty-bench)) should be easily done.

The miniterion package is designed to have a small number of package
dependencies. At the time of writing, the dependency packages are only
two: `base` and `deepseq`. The package does not have rich features,
but compared to other benchmarking packages, the package and benchmark
executable should compile faster, and the resulting benchmark
executable should be smaller.

As in `criterion`, the executable built with the `defaultMain`
function supports selecting the running benchmarks with prefix match,
case insensitive prefix match, substring match, or glob pattern match
via the command line option. Invoke the benchmark executable with `--help`
option to see other available options.

## Example

The following shows a simple benchmark with a naive Fibonacci
function.

In cabal configuration:

```
benchmark fibo-mi
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

fib :: Integer -> Integer
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

```
$ cabal bench
Build profile: -w ghc-9.6.2 -O1
In order, the following will be built (use -v for more details):
 - miniterion-0.1.0.0 (bench:fibo-mi) (first run)
Preprocessing benchmark 'fibo-mi' for miniterion-0.1.0.0..
Building benchmark 'fibo-mi' for miniterion-0.1.0.0..
Running 1 benchmarks...
Benchmark fibo-mi: RUNNING...
benchmarking fib/1
mean                 13.58 ns
std dev              686.0 ps

benchmarking fib/5
mean                 216.6 ns
std dev              15.78 ns

benchmarking fib/9
mean                 1.586 μs
std dev              89.60 ns

benchmarking fib/11
mean                 4.175 μs
std dev              92.17 ns

Benchmark fibo-mi: FINISH
```

Run:

```
$ cabal run -- fibo-mi --help
```

to see the help message.

<!-- links -->

[ci-badge-svg]: https://github.com/8c6794b6/miniterion/actions/workflows/ci.yml/badge.svg
[ci-badge-link]: https://github.com/8c6794b6/miniterion/actions/workflows/ci.yml
[hackage-badge]: http://img.shields.io/hackage/v/miniterion.svg
[hackage-package]: http://hackage.haskell.org/package/miniterion
[stackage-lts-badge]: http://stackage.org/package/miniterion/badge/lts
[stackage-lts-package]: http://stackage.org/lts/package/miniterion

[criterion]: http://hackage.haskell.org/package/criterion
[gauge]: http://hackage.haskell.org/package/gauge
[tasty-bench]: http://hackage.haskell.org/package/tasty-bench
