{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
module Main where

-- base
import           Data.IORef (modifyIORef', newIORef, readIORef)

-- Renamed with cabal mixins
import           Miniterion

succi, predi :: Int -> Int
succi = succ
predi = pred

b01 :: [Benchmark]
b01 =
  [ bgroup "group1"
    [ bench "succi" (nf succi 9)
    , bench "predi" (nf predi 10) ]

  , bgroup "group2"
    [ bgroup "a"
      [ bgroup "foo"
        [ bench "succi" (nf succi 9)
        , bench "predi" (nf predi 9) ]]
    , bgroup "b"
      [ bgroup "bar"
        [ bench "succi" (nf succi 8)
        , bench "predi" (nf predi 8) ]]]

  , env (pure 8) $ \r ->
      bgroup "group3"
      [ bench "succi" (nf succi r)
      , bench "predi" (nf predi r) ]

    -- Using irrefutable pattern to support 'list' mode.
  , env (putStrLn "[group4]" >> pure (8, True)) $ \ ~(r, _) ->
      bgroup "group4"
      [ bench "succi" (nf succi r)
      , bench "predi" (nf predi r) ]

  , envWithCleanup
    (putStrLn "[group5] acquire" >> pure 8)
    (putStrLn . ("[group5] cleanup: " ++) . show) $ \r ->
      bgroup "group5"
      [ bench "succi" (nf succi r)
      , bench "predi" (nf predi r) ]
  ]

b02 :: [Benchmark]
b02 =
  [ bgroup "b02-1"
   [ bench "succi" (nf succi 9)
   , bench "predi" (nf predi 9) ]

  -- As in criterion, use of irrefutable pattern in the lambda
  -- expression below is necessary to support listing benchmark
  -- names with dry-run option.
  , bgroup "b02-2"
    [ env (pure (8, ())) $ \(a, _) ->
        bench "succi" (nf succi a)
    , bench "predi" (nf predi 9) ]
  ]

wc :: String -> Int
wc = length . words

wcIO :: FilePath -> IO Int
wcIO = fmap wc . readFile

b03 :: [Benchmark]
b03 =
  let file = "/usr/share/dict/words"
  in [ bgroup "fib"
       [ bench "nf" (nf fib 8)
       , bench "whnf" (whnf fib 8) ]
     , env (readFile file) $ \contents ->
         bgroup "wc"
         [ bench "nf" (nf wc contents)
         , bench "whnf" (whnf wc contents) ]
     , bgroup "names"
       [ bench "containing \"double quotes\"" (nf fib 8)
       , bench "containing\nnew\nlines" (nf fib 16)
       , bench "containing '\"' quoted double quote" (nf fib 8)
       , bench "containing gt <, lt >, plus +, and amp &" (nf fib 8)
       , bench "containing backslash \\\\" (nf fib 8)
       ]
     , bgroup "wcIO"
       [ bench "nfAppIO" (nfAppIO wcIO file)
       , bench "whnfAppfIO" (whnfAppIO wcIO file) ]
     ]


fib :: Int -> Integer
fib n = if n < 2 then toInteger n else fib (n-1) + fib (n-2)

-- Loop unrolled variant.
fib2 :: Int -> Integer
fib2 n = if n < 2 then toInteger n else fib2a (n-1) + fib2a (n-2)
{-# INLINE fib2 #-}

fib2a :: Int -> Integer
fib2a n = if n < 2 then toInteger n else fib2b (n-1) + fib2b (n-2)
{-# INLINE fib2a #-}

fib2b :: Int -> Integer
fib2b n = if n < 2 then toInteger n else fib2 (n-1) + fib2 (n-2)
{-# INLINE fib2b #-}

fib3 :: Int -> Integer
fib3 =
  let fibs = (0 :: Int) : 1 : rest
      rest = case fibs of
        _:tl -> zipWith (+) fibs tl
        []   -> error "impossible happened!"
  in  toInteger . (fibs !!)

fibBench :: [Benchmark]
fibBench = mkFibBench fib

fib2Bench :: [Benchmark]
fib2Bench = mkFibBench fib2

fib3Bench :: [Benchmark]
fib3Bench = mkFibBench fib3

fib4Bench :: [Benchmark]
fib4Bench = mkFibBench fib4

mkFibBench :: (Int -> Integer) -> [Benchmark]
mkFibBench fn =
  [ bgroup "fib" $
    map (\n -> bench (show n) (nf fn n))
    [4, 8, 16, 32]
  ]

fib4 :: Int -> Integer
fib4 = go 0 1
  where
    go :: Int -> Int -> Int -> Integer
    go !a !b !i
      | i == 0 = toInteger a
      | otherwise = go b (a+b) (i-1)

b04 :: [Benchmark]
b04 =
  let f n = bench ("succ " ++ show n) (nf succi n)
      g n = bgroup ("group " ++ show n) (map f [((n-1) * 100) + 1 .. (n * 100)])
  in  map g [1 .. 20]

b05 :: [Benchmark]
b05 =
  [ bgroup "perXXX"
    [ let alloc = newIORef 0
          cleanup ref = readIORef ref >>= \v -> v `seq` pure ()
          run ref = bench "whole" (whnfIO (modifyIORef' ref succi))
      in envWithCleanup alloc cleanup run

#ifndef MIN_VERSION_tasty_bench
    , let alloc _n = newIORef 0
          cleanup _n ref = readIORef ref >>= \v -> v `seq` pure ()
          run ref = modifyIORef' ref succi
      in  bench "batchenv" $ perBatchEnvWithCleanup alloc cleanup run

    , let alloc = newIORef 0
          cleanup ref = readIORef ref >>= \v -> v `seq` pure ()
          run ref = modifyIORef' ref succi
      in  bench "runenv" $ perRunEnvWithCleanup alloc cleanup run

    , with_runenv "fib4-runenv" 4 (fromInteger . fib)
    , with_runenv "fib8-runenv" 8 (fromInteger . fib)
    , with_runenv "fib16-runenv" 16 (fromInteger . fib)
    , with_runenv "fib32-runenv" 32 (fromInteger . fib)

    , let alloc = newIORef 0
          cleanup ref = readIORef ref >>= \v -> v `seq` pure ()
          run ref = wcIO file >>= \v -> modifyIORef' ref (const v)
          file = "/usr/share/dict/words"
      in  bench "wcIO-runenv" $ perRunEnvWithCleanup alloc cleanup run
#endif
    ]
  ]

#ifndef MIN_VERSION_tasty_bench
with_runenv :: String -> a -> (a -> a) -> Benchmark
with_runenv name ini f =
  let alloc = newIORef ini
      cleanup ref = readIORef ref >>= \v -> v `seq` pure ()
      run ref = modifyIORef' ref f
  in  bench name $ perRunEnvWithCleanup alloc cleanup run
#endif

b06 :: [Benchmark]
b06 =
  [ bgroup "comparison"
    [ bench "exp" $ whnf exp (2 :: Double)
    , bench "log" $ whnf log (2 :: Double)
    , bench "sqrt" $ whnf sqrt (2 :: Double)
    ]
  ]

b07 :: [Benchmark]
b07 =
  [ bgroup "id"
    [ bench "nf" $ nf id ()
    , bench "whnf" $ whnf id ()
    ]
  ]

main :: IO ()
main =
  defaultMain
    (concat [ b01
            -- , b02
            , fibBench
            -- , fib2Bench
            -- , fib3Bench
            -- , fib4Bench
            , b03
            -- , b04
            , b05
            , b06
            , b07
            ])

s, p :: Benchmark
s = bench "succ" (nf (succ :: Int -> Int) 1)
p = bench "pred" (nf (pred :: Int -> Int) 1)

bench_nesting_env_strict :: IO ()
bench_nesting_env_strict =
  defaultMain
  [ bgroup "a"
    [ bgroup "1" [s, p]
    , bgroup "2" [s, p] ]
  , env (pure (1, 2)) $ \ (a, b) ->
      bgroup "b"
      [ bench "fiba" (nf fib a)
      , bench "fibb" (nf fib b) ]]
