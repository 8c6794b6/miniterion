{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# OPTIONS_GHC -funbox-strict-fields  #-}
{- |
Module:       Miniterion
License:      MIT

Simple benchmarking utilities with API subset of
<https://hackage.haskell.org/package/criterion criterion> (and also a
subset of <https://hackage.haskell.org/package/gauge gauge> and
<https://hackage.haskell.org/package/tasty-bench tasty-bench>).

The goal of this package is to provide simple and lightweight
benchmark utilities with less amount of codes and dependency
packages. For robust and feature rich benchmarking utility, use the
other packages mentioned above.

This is the only module exposed from the @miniterion@ package. The
dependency packages of @miniterion@ are kept small (at the moment
@base@ and @deepseq@) to make the compilation time and installation
time short, by dropping some functionalities and efficiencies.

-}
module Miniterion
  (
    -- * Types
    Benchmark
  , Benchmarkable

    -- * Creating benchmark suite
  , env
  , envWithCleanup
  , perBatchEnv
  , perBatchEnvWithCleanup
  , perRunEnv
  , perRunEnvWithCleanup
  , toBenchmarkable
  , bench
  , bgroup

  -- * Running a benchmark
  , nf
  , whnf
  , nfIO
  , whnfIO
  , nfAppIO
  , whnfAppIO

    -- * Turning a suite of benchmarks into a program
  , defaultMain

    -- * For interactive use
  , benchmark

#ifdef DEV
    -- * For development, exposed for testing
  , showPicos5
  , showBytes
  , mu
#endif
  ) where

-- base
import           Control.Exception     (Exception (..), SomeException (..),
                                        bracket, evaluate, handle, throw,
                                        throwIO)
import           Control.Monad         (guard, replicateM, void, when)
import           Data.Char             (toLower)
import           Data.Foldable         (find)
import           Data.Int              (Int64)
import           Data.List             (intercalate, isPrefixOf, nub,
                                        stripPrefix, tails)
import           Data.Word             (Word64)
import           GHC.Clock             (getMonotonicTime)
import           GHC.Stats             (RTSStats (..), getRTSStats,
                                        getRTSStatsEnabled)
import           System.Console.GetOpt (ArgDescr (..), ArgOrder (..),
                                        OptDescr (..), getOpt', usageInfo)
import           System.CPUTime        (cpuTimePrecision, getCPUTime)
import           System.Environment    (getArgs, getProgName)
import           System.Exit           (die, exitFailure)
import           System.IO             (BufferMode (..), Handle, IOMode (..),
                                        hFlush, hIsTerminalDevice, hPutStrLn,
                                        hSetBuffering, stderr, stdout, withFile)
import           System.IO.Unsafe      (unsafePerformIO)
import           System.Mem            (performGC, performMinorGC)
import           System.Timeout        (timeout)
import           Text.Printf           (printf)
import           Text.Read             (readMaybe)

#if !MIN_VERSION_base(4,20,0)
import           Data.Foldable         (foldl')
#endif

#if MIN_VERSION_base(4,15,0)
import           GHC.Exts              (SPEC (..))
#else
import           GHC.Exts              (SpecConstrAnnotation (..))
#endif

#if MIN_VERSION_base(4,5,0)
import           GHC.IO.Encoding       (getLocaleEncoding, setLocaleEncoding,
                                        textEncodingName, utf8)
#endif

#if defined(mingw32_HOST_OS)
import           Data.Word             (Word32)
#endif

-- deepseq
import           Control.DeepSeq       (NFData, force, rnf)


-- ------------------------------------------------------------------------
-- Exported
-- ------------------------------------------------------------------------

-- | Benchmarks are simple tree structure with names, and additional
-- information to support 'envWithCleanup'.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#t:Benchmark Benchmark>@.
data Benchmark
  = Bench String Benchmarkable
  | Bgroup String [Benchmark]
  | forall e. NFData e => Environment (IO e) (e -> IO ()) (e -> Benchmark)

-- | Something that can be benchmarked, produced by 'nf', 'whnf',
-- 'nfIO', 'whnfIO', 'nfAppIO', and 'whnfAppIO'.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#t:Benchmarkable Benchmarkable>@.
data Benchmarkable = forall a. NFData a =>
  Benchmarkable { allocEnv      :: Word64 -> IO a
                , cleanEnv      :: Word64 -> a -> IO ()
                , runRepeatedly :: a -> Word64 -> IO ()
                , perRun        :: Bool }

-- | Construct a t'Benchmarkable' value from an impure action, where
-- the 'Word64' parameter indicates the number of times to run the
-- action.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:toBenchmarkable toBenchmarkable>@.
toBenchmarkable :: (Word64 -> IO ()) -> Benchmarkable
toBenchmarkable f = Benchmarkable noop (const noop) (const f) False
{-# INLINE toBenchmarkable #-}

-- | Run benchmarks and report results, providing an interface
-- compatible with @Criterion.Main.<https://hackage.haskell.org/package/criterion/docs/Criterion-Main.html#v:defaultMain defaultMain>@.
defaultMain :: [Benchmark] -> IO ()
defaultMain bs = do
  let act = defaultMainWith defaultConfig bs
#if MIN_VERSION_base(4,5,0)
  setLocaleEncoding utf8
#endif
#if defined(mingw32_HOST_OS)
  codePage <- getConsoleOutputCP
  bracket (setConsoleOutputCP 65001) (\_ -> setConsoleOutputCP codePage)
          (const act)
#else
  act
#endif

-- | Attach a name to t'Benchmarkable'.
--
-- The type signature is compatible with
-- @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:bench bench>@.
bench
  :: String -- ^ Name of this benchmark.
  -> Benchmarkable -- ^ Benchmark target.
  -> Benchmark
bench = Bench

-- | Attach a name to a group of 'Benchmark'.
--
-- The type signature is compatible with
-- @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:bgroup bgroup>@.
bgroup
  :: String -- ^ Name of this benchmark group.
  -> [Benchmark] -- ^ List of benchmarks in the group.
  -> Benchmark
bgroup = Bgroup

-- | Run a benchmark (or collection of benchmarks) in the given
-- environment, usually reading large input data from file.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:env env>@.
env
  :: NFData env
  => IO env -- ^ Action to create the environment.
  -> (env -> Benchmark) -- ^ A function returning benchmark.
  -> Benchmark
env alloc = envWithCleanup alloc noop

-- | Similar to 'env', but includes an additional argument to clean up
-- the environment.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:envWithCleanup envWithCleanup>@.
envWithCleanup
  :: NFData env
  => IO env -- ^ Action to create the environment.
  -> (env -> IO a) -- ^ Action to cleanup the environment.
  -> (env -> Benchmark) -- ^ A function returning benchmark.
  -> Benchmark
envWithCleanup alloc clean = Environment alloc (void . clean)

-- | Create a Benchmarkable where a fresh environment is allocated for every
-- batch of runs of the benchmarkable.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:perBatchEnv perBatchEnv>@.
perBatchEnv
  :: (NFData env, NFData b)
  => (Word64 -> IO env)
  -- ^ Action to create an environment for a batch of N runs.
  -> (env -> IO b)
  -- ^ Benchmark body function.
  -> Benchmarkable
perBatchEnv alloc = perBatchEnvWithCleanup alloc (const noop)

-- | Same as `perBatchEnv`, but but allows for an additional callback
-- to clean up the environment.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:perBatchEnvWithCleanup perBatchEnvWithCleanup>@.
perBatchEnvWithCleanup
  :: (NFData env, NFData b)
  => (Word64 -> IO env)
  -- ^ Action to create an environment for a batch of N runs.
  -> (Word64 -> env -> IO ())
  -- ^ Action to cleanup the environment.
  -> (env -> IO b)
  -- ^ Benchmark body function.
  -> Benchmarkable
perBatchEnvWithCleanup alloc clean run = Benchmarkable alloc clean run' False
  where
    run' = ioToBench rnf .  run

-- | Create a Benchmarkable where a fresh environment is allocated for
-- every run of the operation to benchmark.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:perRunEnv perRunEnv>@.
--
-- __NOTE__: This function does not work well (or not work at all) if
-- the time spent in the initialization work is relatively long
-- compared to the time spent in the benchmark body function. In such
-- case, consider modifying the benchmark body function to spend more
-- elapsed time, or switch to the @criterion@ package.
perRunEnv
  :: (NFData env, NFData b)
  => IO env -- ^ Action to create an environment for a single run.
  -> (env -> IO b) -- ^ Benchmark body function.
  -> Benchmarkable
perRunEnv alloc = perRunEnvWithCleanup alloc noop

-- | Same as `perBatchEnv`, but allows for an additional callback to
-- clean up the environment.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:perRunEnvWithCleanup perRunEnvWithCleanup>@.
--
-- __NOTE__: See the note in 'perRunEnv'.
perRunEnvWithCleanup
  :: (NFData env, NFData b)
  => IO env -- ^ Action to create an environment for a single run.
  -> (env -> IO ()) -- ^ Action to cleanup the environment.
  -> (env -> IO b) -- ^ Benchmark body function.
  -> Benchmarkable
perRunEnvWithCleanup alloc clean run = bm {perRun = True}
  where
    bm = perBatchEnvWithCleanup (const alloc) (const clean) run

-- | 'nf' @f@ @x@ measures time to compute a normal form (by means of
-- 'Control.DeepSeq.rnf', not 'force') of an application of @f@ to
-- @x@.  This does not include time to evaluate @f@ or @x@ themselves.
-- Ideally @x@ should be a primitive data type like 'Data.Int.Int'.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:nf nf>@.
nf :: NFData b => (a -> b) -> a -> Benchmarkable
nf = fmap toBenchmarkable . funcToBench rnf
{-# INLINE nf #-}

-- | 'whnf' @f@ @x@ measures time to compute a weak head normal form
-- of an application of @f@ to @x@.  This does not include time to
-- evaluate @f@ or @x@ themselves.  Ideally @x@ should be a primitive
-- data type like 'Data.Int.Int'.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:whnf whnf>@.
whnf :: (a -> b) -> a -> Benchmarkable
whnf = fmap toBenchmarkable . funcToBench id
{-# INLINE whnf #-}

-- | 'nfIO' @x@ measures time to evaluate side-effects of @x@ and
-- compute its normal form (by means of 'force', not
-- 'Control.DeepSeq.rnf').
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:nfIO nfIO>@.
nfIO :: NFData a => IO a -> Benchmarkable
nfIO = toBenchmarkable . ioToBench rnf
{-# INLINE nfIO #-}

-- | 'whnfIO' @x@ measures time to evaluate side-effects of @x@ and
-- compute its weak head normal form.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:whnfIO whnfIO>@.
whnfIO :: IO a -> Benchmarkable
whnfIO = toBenchmarkable . ioToBench id
{-# INLINE whnfIO #-}

-- | 'nfAppIO' @f@ @x@ measures time to evaluate side-effects of an
-- application of @f@ to @x@ and compute its normal form (by means of
-- 'force', not 'Control.DeepSeq.rnf').  This does not include time to
-- evaluate @f@ or @x@ themselves.  Ideally @x@ should be a primitive
-- data type like 'Data.Int.Int'.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:nfAppIO nfAppIO>@.
nfAppIO :: NFData b => (a -> IO b) -> a -> Benchmarkable
nfAppIO = fmap toBenchmarkable . ioFuncToBench rnf
{-# INLINE nfAppIO #-}

-- | 'whnfAppIO' @f@ @x@ measures time to evaluate side-effects of an
-- application of @f@ to @x@ and compute its weak head normal form.
-- This does not include time to evaluate @f@ or @x@ themselves.
-- Ideally @x@ should be a primitive data type like 'Data.Int.Int'.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:whnfAppIO whnfAppIO>@.
whnfAppIO :: (a -> IO b) -> a -> Benchmarkable
whnfAppIO = fmap toBenchmarkable . ioFuncToBench id
{-# INLINE whnfAppIO #-}

-- | Run a benchmark interactively, providing an interface compatible with
-- @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:benchmark benchmark>@.
benchmark :: Benchmarkable -> IO ()
benchmark = void . runBenchmark defaultConfig . bench "..."


-- ------------------------------------------------------------------------
-- Main
-- ------------------------------------------------------------------------

defaultMainWith :: Config -> [Benchmark] -> IO ()
defaultMainWith cfg0 bs = handleMiniterionException $ do
  args <- getArgs
  let (opts, _pats, invalids, errs) = getOpt' order options args
      order = ReturnInOrder $ \str o ->
        o {cfgPatterns = (cfgMatch o, str) : cfgPatterns o}
      cfg1 = foldl' (flip id) cfg0 opts
      cfg2 = cfg1 {cfgPatterns = reverse (cfgPatterns cfg1)}
      with_csv_cfg act =
        case cfgCsvPath cfg2 of
          Nothing -> act cfg2
          Just path -> withFile path WriteMode $ \hdl -> do
            hSetBuffering hdl LineBuffering
            let extras | hasGCStats = ",Allocated,Copied,Peak Memory"
                       | otherwise = ""
            hPutStrLn hdl ("Name,Mean (ps),2*Stdev (ps)" ++ extras)
            act cfg2 {cfgCsvHandle = Just hdl}
      root_bs = bgroup "" bs
      do_bench = with_csv_cfg $ \cfg -> do
        baseline <- maybe mempty readBaseline (cfgBaselinePath cfg)
        rs <- runBenchmark (cfg {cfgBaselineSet = baseline}) root_bs
        summariseResults rs
  case () of
    _ | cfgHelp cfg2        -> showHelp
      | cfgVersion cfg2     -> putStrLn builtWithMiniterion
      | cfgList cfg2        -> showNames cfg2 root_bs
      | not (null errs)     -> errorOptions errs
      | not (null invalids) -> invalidOptions invalids
      | otherwise           -> do_bench

showHelp :: IO ()
showHelp = do
  me <- getProgName
  putStrLn . (`usageInfo` options) $ intercalate "\n"
    [ "Microbenchmark suite - " ++ builtWithMiniterion ++ "\n"
    , yellow "USAGE:"
    , "  " ++ green me ++ " [OPTIONS] [PATTERN]...\n"
    , yellow "ARGS:"
    , "  <PATTERN>...  Pattern(s) to select running benchmarks. If no pattern was"
    , "                given, run all benchmarks. Multiple patterns are combined"
    , "                with 'OR'. Selections are done by prefix match by default."
    , "                See also \"--match\" option below.\n"
    , yellow "OPTIONS:"
    ]

#ifndef VERSION_miniterion
#define VERSION_miniterion "development version"
#endif

builtWithMiniterion :: String
builtWithMiniterion = "built with miniterion " ++ VERSION_miniterion

errorOptions :: [String] -> IO ()
errorOptions = exitWithOptions id

invalidOptions :: [String] -> IO ()
invalidOptions = exitWithOptions (\o -> "invalid option `" ++ o ++ "'\n")

exitWithOptions :: (String -> String) -> [String] -> IO ()
exitWithOptions f opts = do
  me <- getProgName
  let f' opt = me ++ ": " ++ f opt
  die (concatMap f' opts ++ briefUsageOf me)

briefUsageOf :: String -> String
briefUsageOf me = "Try `" ++ me ++ " --help' for more information."

showNames :: Config -> Benchmark -> IO ()
showNames cfg = mapM_ (\n -> when (isMatched cfg n) (putStrLn n)) . benchNames []


-- ------------------------------------------------------------------------
-- Result
-- ------------------------------------------------------------------------

data Result
  = Done -- ^ Successfully finished running the benchmark.
  | TooSlow String -- ^ Too slow compared to given baseline.
  | TooFast String -- ^ Too fast compared to given baseline.
  | TimedOut String -- ^ Timed out.

summariseResults :: [Result] -> IO ()
summariseResults rs = do
  let (num_result, num_failed) = foldl' f z rs
      z :: (Int, Int)
      z = (0, 0)
      f (!done, !fl) r = case r of
        Done -> (done + 1, fl)
        _    -> (done + 1, fl + 1)
      bs = if 1 < num_result then "benchmarks" else "benchmark"
      pr (name, why) = putStrLn ("  - " ++ name ++ " (" ++ why ++ ")")
  when (0 < num_failed) $ do
    printf "\n%d out of %d %s failed:\n" num_failed num_result bs
    mapM_ (mapM_ pr . failedNameAndReason) rs
    exitFailure

isTooFast, isTooSlow :: Result -> Bool

isTooFast TooFast {} = True
isTooFast _          = False

isTooSlow TooSlow {} = True
isTooSlow _          = False

failedNameAndReason :: Result -> Maybe (String, String)
failedNameAndReason = \case
  Done          -> Nothing
  TooSlow name  -> Just (name, "too slow")
  TooFast name  -> Just (name, "too fast")
  TimedOut name -> Just (name, "timed out")


-- ------------------------------------------------------------------------
-- Running benchmarks
-- ------------------------------------------------------------------------

runBenchmark :: Config -> Benchmark -> IO [Result]
runBenchmark cfg = go []
  where
    go acc0 bnch = case bnch of
      Bench name act -> pure <$> runBenchmarkable cfg acc0 name act
      Bgroup name bs ->
        let acc1 = consNonNull name acc0
            to_run = filter (any (isMatched cfg) . benchNames acc1) bs
        in  concat <$> mapM (go acc1) to_run
      Environment alloc clean f ->
        let alloc' = alloc >>= \e -> evaluate (rnf e) >> pure e
        in  bracket alloc' clean (go acc0 . f)

runBenchmarkable :: Config -> [String] -> String -> Benchmarkable -> IO Result
runBenchmarkable cfg parents name b = do
  let fullname = pathToName parents name

  infoStr cfg (white "benchmarking " ++ boldCyan fullname ++ " ")
  debugStr cfg "\n"
  hFlush stdout
  mb_est <- withTimeout (cfgTimeout cfg) (measureUntil cfg b)

  let upper = 1 + cfgFailIfSlower cfg
      lower = 1 - cfgFailIfFaster cfg
      is_acceptable cmp
        | upper <= cmp = TooSlow fullname
        | cmp <= lower = TooFast fullname
        | otherwise = Done
      (result, mb_cmp) = case mb_est of
        Nothing -> (TimedOut fullname, Nothing)
        Just est -> case compareVsBaseline (cfgBaselineSet cfg) fullname est of
          Nothing  -> (Done, Nothing)
          Just cmp -> (is_acceptable cmp, Just cmp)
      csvname = encodeCsv fullname
      put_csv_line hdl =
        mapM_ (\e -> hPutStrLn hdl (csvname ++ "," ++ csvEstimate e)) mb_est

  infoStr cfg (formatResult result mb_est mb_cmp)
  mapM_ put_csv_line (cfgCsvHandle cfg)
  pure result

withTimeout :: Timeout -> IO a -> IO (Maybe a)
withTimeout tout act = case tout of
  Timeout micro -> timeout (fromIntegral micro) act
  NoTimeout     -> fmap Just act

benchNames :: [String] -> Benchmark -> [String]
benchNames = go
  where
    go acc b = case b of
      Bench name _      -> [pathToName acc name]
      Bgroup name bs    -> concatMap (go (consNonNull name acc)) bs
      Environment _ _ f -> go acc (f (throw (UninitializedEnv acc)))

pathToName :: [String] -> String -> String
pathToName prevs me = foldr (\a b -> a ++ "/" ++ b) me (reverse prevs)

groupsToName :: [String] -> String
groupsToName = \case
  []      -> ""
  (hd:tl) -> pathToName tl hd

consNonNull :: String -> [String] -> [String]
consNonNull x xs = if null x then xs else x : xs

noop :: Applicative m => a -> m ()
noop = const (pure ())
{-# INLINE noop #-}


-- ------------------------------------------------------------------------
-- Printing with verbosity
-- ------------------------------------------------------------------------

infoStr, debugStr :: Config -> String -> IO ()

infoStr = putWith 1 putStr
debugStr = putWith 2 putStr

putWith :: Applicative m => Int -> (a -> m ()) -> Config -> a -> m ()
putWith n act cfg x = when (n <= cfgVerbosity cfg) $ act x


-- ------------------------------------------------------------------------
-- Formatting
-- ------------------------------------------------------------------------

formatResult :: Result -> Maybe Estimate -> Maybe Double -> String
formatResult _ Nothing _ =
  red "FAIL" ++ "\n" ++
  yellow "Timed out while running this benchmark\n\n"
formatResult res (Just (Estimate m stdev)) mb_cmp =
  fail_or_blank ++ "\n" ++
  white "mean                 " ++ showPicos5 (measTime m) ++
  maybe "" (formatSlowDown res) mb_cmp ++ "\n" ++
  white "std dev              " ++ showPicos5 (2 * stdev) ++
  formatGC m ++ "\n\n"
  where
    fail_or_blank
      | isTooFast res || isTooSlow res = red "FAIL"
      | otherwise = ""

formatSlowDown :: Result -> Double -> String
formatSlowDown result ratio = case percents `compare` 0 of
  LT -> in_yellow isTooFast $ printf " (%2i%% less than baseline)" (-percents)
  EQ -> white                          "       (same as baseline)"
  GT -> in_yellow isTooSlow $ printf " (%2i%% more than baseline)" percents
  where
    percents :: Int64
    percents = truncate ((ratio - 1) * 100)
    in_yellow test = if test result then yellow else white

-- | Show picoseconds, fitting number in 5 characters.
showPicos5 :: Word64 -> String
showPicos5 i
  | t < 10     = printf "%.3f ps" t
  | t < 100    = printf "%.2f ps" t
  | t < 1000   = printf "%.1f ps" t
  | t < 999e1  = printf "%.3f ns" (t / 1e3)
  | t < 999e2  = printf "%.2f ns" (t / 1e3)
  | t < 999e3  = printf "%.1f ns" (t / 1e3)
  | t < 999e4  = printf "%.3f %cs" (t / 1e6) mu
  | t < 999e5  = printf "%.2f %cs" (t / 1e6) mu
  | t < 999e6  = printf "%.1f %cs" (t / 1e6) mu
  | t < 999e7  = printf "%.3f ms" (t / 1e9)
  | t < 999e8  = printf "%.2f ms" (t / 1e9)
  | t < 999e9  = printf "%.1f ms" (t / 1e9)
  | t < 999e10 = printf "%.3f s " (t / 1e12)
  | t < 999e11 = printf "%.2f s " (t / 1e12)
  | t < 999e12 = printf "%.1f s " (t / 1e12)
  | otherwise  = printf "%4.1f s" (t / 1e12)
  where
    t = word64ToDouble i

formatGC :: Measurement -> String
formatGC (Measurement _ a c p)
  | hasGCStats = "\n" ++
    white "        alloc  copied    peak" ++ "\n" ++
    white "gc     " ++ sb a ++ "  " ++ sb c ++ "  " ++ sb p
  | otherwise = ""
  where
    sb = showBytes

showBytes :: Word64 -> String
showBytes i
  | t < 1000                 = printf " %3.0f B" t
  | t < 10189                = printf "%3.1f KB" (t / 1024)
  | t < 1023488              = printf "%3.0f KB" (t / 1024)
  | t < 10433332             = printf "%3.1f MB" (t / 1048576)
  | t < 1048051712           = printf "%3.0f MB" (t / 1048576)
  | t < 10683731149          = printf "%3.1f GB" (t / 1073741824)
  | t < 1073204953088        = printf "%3.0f GB" (t / 1073741824)
  | t < 10940140696372       = printf "%3.1f TB" (t / 1099511627776)
  | t < 1098961871962112     = printf "%3.0f TB" (t / 1099511627776)
  | t < 11202704073084108    = printf "%3.1f PB" (t / 1125899906842624)
  | t < 1125336956889202624  = printf "%3.0f PB" (t / 1125899906842624)
  | t < 11471568970838126592 = printf "%3.1f EB" (t / 1152921504606846976)
  | otherwise                = printf "%3.0f EB" (t / 1152921504606846976)
  where
    t = word64ToDouble i

formatMeasurement :: Measurement -> String
formatMeasurement (Measurement t a c m) =
  printf "%d ps, alloc: %d copied: %d max: %d" t a c m


-- ------------------------------------------------------------------------
-- Matching benchmark names
-- ------------------------------------------------------------------------

data MatchMode
  = Pattern -- ^ Substring match
  | Prefix  -- ^ Prefix match
  | IPattern -- ^ Case insensitive prefix match
  | Glob -- ^ Glob pattern match

isMatched :: Config -> String -> Bool
isMatched Config{..} fullname = no_pat || has_match
  where
    no_pat = null cfgPatterns
    has_match = any is_match cfgPatterns
    is_match (mode, str) = case mode of
      Glob     -> glob str fullname
      IPattern -> substring (map toLower str) (map toLower fullname)
      Pattern  -> substring str fullname
      Prefix   -> str `isPrefixOf` fullname

substring :: String -> String -> Bool
substring pat = any (pat `isPrefixOf`) . tails

-- Simple, inefficient, and improper glob. Does not support special
-- character class names like `[:alnum:]', `[:digit:]', ... etc.
glob :: String -> String -> Bool
glob pat0 = go pat0
  where
    go [] [] = True
    go ('\\':p:ps) (c:cs) = p == c && go ps cs
    go ('?':ps) (_:cs) = go ps cs
    go ['*'] _ = True
    go ('*':ps) cs = any (go ps) (cs : tails cs)
    go ('[':'!':ps) (c:cs) = cclass notElem c ps cs
    go ('[':ps) (c:cs) = cclass elem c ps cs
    go (p:ps) (c:cs) | p == c = go ps cs
    go _ _ = False

    cclass test c ps cs =
      let lp close acc xs =
            case xs of
              []              -> throw (GlobUnbalancedBracket pat0)
              '\\':x:xs'      -> lp True (x:acc) xs'
              ']':xs' | close -> test c acc && go xs' cs
              x0:'-':']':xs'  -> test c ('-':x0:acc) && go xs' cs
              x0:'-':x1:xs'   -> lp True ([x0 .. x1] ++ acc) xs'
              x:xs'           -> lp True (x:acc) xs'
      in  lp False [] ps


-- ------------------------------------------------------------------------
-- Terminal stuffs
-- ------------------------------------------------------------------------

red, green, yellow, boldCyan, white :: String -> String

red      = coloredString "1;31"
green    = coloredString "0;32"
yellow   = coloredString "0;33"
boldCyan = coloredString "1;36"
white    = coloredString "0;37"

coloredString :: String -> String -> String
coloredString param str
  | isTerminalDevice = "\ESC[" ++ param ++ "m" ++ str ++ "\ESC[0m"
  | otherwise = str

isTerminalDevice :: Bool
isTerminalDevice = unsafePerformIO (hIsTerminalDevice stdout)
{-# NOINLINE isTerminalDevice #-}

mu :: Char
mu = if hasUnicodeSupport then 'μ' else 'u'

hasUnicodeSupport :: Bool
#if MIN_VERSION_base(4,5,0)
hasUnicodeSupport = take 3 (textEncodingName enc) == "UTF"
#if defined(mingw32_HOST_OS)
  && unsafePerformIO getConsoleOutputCP == 65001
#endif
  where
    enc = unsafePerformIO getLocaleEncoding
#else
hasUnicodeSupport = False
#endif
{-# NOINLINE hasUnicodeSupport #-}


-- ------------------------------------------------------------------------
-- CSV
-- ------------------------------------------------------------------------

-- XXX: Could use `Data.Set.Set'.
type Baseline = [String]

csvEstimate :: Estimate -> String
csvEstimate (Estimate m stdev)
  | hasGCStats = time ++ "," ++ gc
  | otherwise = time
  where
    time = show (measTime m) ++ "," ++ show (2 * stdev)
    gc = show (measAllocs m) ++ "," ++ show (measCopied m) ++ "," ++
         show (measMaxMem m)

readBaseline :: FilePath -> IO Baseline
readBaseline path = handle handler go
  where
    handler :: SomeException -> IO a
    handler _ = throwIO (CannotReadFile (Just "baseline") path)
    go = readFile path >>= evaluate . force . nub . joinQuotedFields . lines

joinQuotedFields :: [String] -> [String]
joinQuotedFields [] = []
joinQuotedFields (x : xs)
  | areQuotesBalanced x = x : joinQuotedFields xs
  | otherwise = case span areQuotesBalanced xs of
    (_, [])      -> [] -- malformed CSV
    (ys, z : zs) -> unlines (x : ys ++ [z]) : joinQuotedFields zs
  where
    areQuotesBalanced = even . length . filter (== '"')

compareVsBaseline :: Baseline -> String -> Estimate -> Maybe Double
compareVsBaseline baseline name (Estimate m stdev) = fmap comp mb_old
  where
    comp (old_time, old_sigma_x_2) =
      if abs (time - old_time) < max (2 * word64ToInt64 stdev) old_sigma_x_2
        then 1
        else int64ToDouble time / int64ToDouble old_time

    time = word64ToInt64 $ measTime m

    mb_old :: Maybe (Int64, Int64)
    mb_old = do
      let prefix = encodeCsv name ++ ","
          (_, breaked) = break (isPrefixOf prefix) baseline
      line <- case breaked of
        []   -> Nothing
        hd:tl -> case break (isPrefixOf prefix) tl of
          (_, []) -> pure hd
          _       -> Nothing

      (time_cell, ',' : rest) <- span (/= ',') <$> stripPrefix prefix line
      let sigma_x_2_cell = takeWhile (/= ',') rest
      (,) <$> readMaybe time_cell <*> readMaybe sigma_x_2_cell

encodeCsv :: String -> String
encodeCsv xs
  | any (`elem` xs) ",\"\n\r" = '"' : go xs -- opening quote
  | otherwise = xs
  where
    go []         = ['"'] -- closing quote
    go ('"' : ys) = '"' : '"' : go ys
    go (y : ys)   = y : go ys


-- ------------------------------------------------------------------------
-- Configuration
-- ------------------------------------------------------------------------

data Config = Config
  { cfgHelp         :: Bool
    -- ^ True when showing help message.
  , cfgList         :: Bool
    -- ^ True when showing benchmark names.
  , cfgBaselinePath :: Maybe FilePath
    -- ^ Path to a file containing baseline data, usually a CSV file
    -- made with @--csv@ option in advance.
  , cfgBaselineSet  :: Baseline
    -- ^ Set containing baseline information, made from the file
    -- specified by cfgBaselinePath.
  , cfgCsvPath      :: Maybe FilePath
    -- ^ Path to a file for writing results in CSV format.
  , cfgCsvHandle    :: Maybe Handle
    -- ^ File handle to write benchmark result in CSV format.
  , cfgFailIfFaster :: Double
    -- ^ Upper bound of acceptable speed up.
  , cfgFailIfSlower :: Double
    -- ^ Upper bound of acceptable slow down.
  , cfgMatch        :: MatchMode
    -- ^ Which mode to use for benchmark name pattern match.
  , cfgPatterns     :: [(MatchMode,String)]
    -- ^ Patterns to filter running benchmarks.
  , cfgRelStDev     :: Double
    -- ^ Relative standard deviation for measuring benchmarks.
  , cfgTimeMode     :: TimeMode
    -- ^ Time mode for measuring benchmarks.
  , cfgTimeout      :: Timeout
    -- ^ Timeout duration in seconds.
  , cfgVerbosity    :: Int
    -- ^ Verbosity level.
  , cfgVersion      :: Bool
    -- ^ True when showing version info.
  }

defaultConfig :: Config
defaultConfig = Config
  { cfgHelp = False
  , cfgList = False
  , cfgBaselinePath = Nothing
  , cfgBaselineSet = mempty
  , cfgCsvPath = Nothing
  , cfgCsvHandle = Nothing
  , cfgFailIfFaster = 1.0 / 0.0
  , cfgFailIfSlower = 1.0 / 0.0
  , cfgPatterns = []
  , cfgMatch = Prefix
  , cfgRelStDev = 0.05
  , cfgTimeMode = CpuTime
  , cfgTimeout = NoTimeout
  , cfgVerbosity = 1
  , cfgVersion = False
  }

options :: [OptDescr (Config -> Config)]
options =
  [ Option ['h'] ["help"]
    (NoArg (\o -> o {cfgHelp = True}))
    "Show this help text"

  , Option ['L'] ["time-limit"]
    (ReqArg (\str o -> case readMaybe str :: Maybe Double of
                Just n -> o {cfgTimeout = Timeout (floor (1e6 * n))}
                _      -> throw (InvalidArgument "time-limit" str))
      "SECS")
    (unlines
      ["Time limit to run a benchmark"
      ,"(default: no timeout)"])

  , Option [] ["baseline"]
    (ReqArg (\str o -> o {cfgBaselinePath = Just str})
    "FILE")
    "File to read CSV summary from as baseline"

  , Option [] ["csv"]
    (ReqArg (\str o -> o {cfgCsvPath = Just str})
     "FILE")
    "File to write CSV summary to"

  , Option [] ["fail-if-faster"]
    (ReqArg (\str o -> case parsePositivePercents str of
                Just x -> o {cfgFailIfFaster = x}
                _      -> throw (InvalidArgument "fail-if-faster" str))
      "NUM")
    (unlines
     ["Upper bound acceptable speed up in percents. If a"
     ,"benchmark is unacceptable faster than baseline (see"
     ,"--baseline), it will be reported as failed"])

  , Option [] ["fail-if-slower"]
    (ReqArg (\str o -> case parsePositivePercents str of
                Just x -> o {cfgFailIfSlower = x}
                _      -> throw (InvalidArgument "fail-if-slower" str))
      "NUM")
    (unlines
     ["Upper bound acceptable slow down in percents. If a"
     ,"benchmark is unacceptable slower than baseline (see"
     ,"--baseline), it will be reported as failed"])

  , Option ['s'] ["stdev"]
    (ReqArg (\str o -> case parsePositivePercents str of
                Just x -> o {cfgRelStDev = x}
                _      -> throw (InvalidArgument "stdev" str))
     "NUM")
    (unlines
     ["Target relative standard deviation of measurement"
     ,"in percents (default: 5)"])

  , Option [] ["time-mode"]
    (ReqArg (\str o -> case str of
                "cpu"  -> o {cfgTimeMode = CpuTime}
                "wall" -> o {cfgTimeMode = WallTime}
                _      -> throw (InvalidArgument "time-mode" str))
    "cpu|wall")
    (unlines
     ["Whether to measure CPU (\"cpu\") time or wall-clock"
     ,"time (\"wall\") (default: cpu)"])

  , Option ['v'] ["verbosity"]
    (ReqArg (\str o -> case readMaybe str :: Maybe Int of
                Just n | 0 <= n && n <= 2 -> o {cfgVerbosity = n}
                _ -> throw (InvalidArgument "verbosity" str))
      "INT")
     "Verbosity level (default: 1)"

  , Option ['m'] ["match"]
    (let modes = [("glob", Glob)
                 ,("pattern", Pattern)
                 ,("prefix", Prefix)
                 ,("ipattern", IPattern)]
         match str = isPrefixOf str . fst
     in  ReqArg (\str o -> case find (match str) modes of
                    Just (_, mode) -> o {cfgMatch = mode}
                    _              -> throw (InvalidArgument "match" str))
      "MODE")
    (unlines
     ["How to match benchmark names (\"prefix\", \"glob\","
     ,"\"pattern\" (substring), or \"ipattern\")"])

  , Option ['l'] ["list"]
    (NoArg (\o -> o {cfgList = True}))
    "List benchmarks"

  , Option [] ["version"]
    (NoArg (\o -> o {cfgVersion = True}))
    "Show version info"
  ]

parsePositivePercents :: String -> Maybe Double
parsePositivePercents xs = do
  x <- readMaybe xs
  guard (x > 0)
  pure (x / 100)


-- ------------------------------------------------------------------------
-- Exception
-- ------------------------------------------------------------------------

data MiniterionException
  = InvalidArgument String String
  | CannotReadFile (Maybe String) String
  | UninitializedEnv [String]
  | GlobUnbalancedBracket String
  deriving (Show)

instance Exception MiniterionException where
  displayException = displayMiniterionException

displayMiniterionException :: MiniterionException -> String
displayMiniterionException = \case
  InvalidArgument lbl arg ->
    "invalid argument `" ++ arg ++ "'" ++ maybe_label (Just lbl)
  CannotReadFile mb_lbl path ->
    "cannot read file `" ++ path ++ "'" ++ maybe_label mb_lbl
  UninitializedEnv groups ->
    "uninitialized env" ++
    (if null groups then "" else " under `" ++ groupsToName groups ++ "'") ++
    "\nuse irrefutable pattern in the function taking the env."
  GlobUnbalancedBracket pat ->
    "unbalanced bracket in glob pattern `" ++ pat ++ "'"
  where
    maybe_label = maybe "" (\lbl -> " for `--" ++ lbl ++ "'")

handleMiniterionException :: IO a -> IO a
handleMiniterionException =
  handle $ \e -> maybe (throwIO e) complain_and_die (fromException e)
  where
    complain_and_die :: MiniterionException -> IO a
    complain_and_die he = do
      me <- getProgName
      die (me ++ ": " ++ displayException he ++ "\n" ++ briefUsageOf me)


-- ------------------------------------------------------------------------
-- Getting current time
-- ------------------------------------------------------------------------

data TimeMode
  = CpuTime -- ^ Measure CPU time.
  | WallTime -- ^ Measure wall-clock time.

getTimePicoSecs :: TimeMode -> IO Word64
getTimePicoSecs = \case
  CpuTime  -> fromInteger <$> getCPUTime
  WallTime -> round . (1e12 *) <$> getMonotonicTime


-- ------------------------------------------------------------------------
-- Getting GC info
-- ------------------------------------------------------------------------

getAllocsAndCopied :: IO (Word64, Word64, Word64)
getAllocsAndCopied =
#if MIN_VERSION_base(4,10,0)
  if not hasGCStats then pure (0, 0, 0) else
    (\s -> (allocated_bytes s, copied_bytes s, max_mem_in_use_bytes s))
    <$> getRTSStats
#elif MIN_VERSION_base(4,6,0)
  if not hasGCStats then pure (0, 0, 0) else
    (\s -> (int64ToWord64 $ bytesAllocated s,
            int64ToWord64 $ bytesCopied s,
            int64ToWord64 $ peakMegabytesAllocated s * 1024 * 1024))
    <$> getGCStats
#else
    pure (0, 0, 0)
#endif

hasGCStats :: Bool
#if MIN_VERSION_base(4,10,0)
hasGCStats = unsafePerformIO getRTSStatsEnabled
#elif MIN_VERSION_base(4,6,0)
hasGCStats = unsafePerformIO getGCStatsEnabled
#else
hasGCStats = False
#endif
{-# NOINLINE hasGCStats #-}


-- ------------------------------------------------------------------------
-- Measuring
-- ------------------------------------------------------------------------

data Timeout
  = Timeout Prelude.Integer -- ^ number of microseconds (e.g., 200000)
  | NoTimeout

data Measurement = Measurement
  { measTime   :: {-# UNPACK #-} !Word64 -- ^ time in picoseconds
  , measAllocs :: {-# UNPACK #-} !Word64 -- ^ allocations in bytes
  , measCopied :: {-# UNPACK #-} !Word64 -- ^ copied bytes
  , measMaxMem :: {-# UNPACK #-} !Word64 -- ^ max memory in use
  }

data Estimate = Estimate
  { estMean  :: {-# UNPACK #-} !Measurement
  , estStdev :: {-# UNPACK #-} !Word64  -- ^ stdev in picoseconds
  }

sqr :: Num a => a -> a
sqr x = x * x
{-# INLINE sqr #-}

predict
  :: Measurement -- ^ time for @n@ run
  -> Measurement -- ^ time for @2*n@ runs
  -> Estimate
predict (Measurement t1 a1 c1 m1) (Measurement t2 a2 c2 m2) = Estimate
  { estMean  = Measurement t (fit a1 a2) (fit c1 c2) (max m1 m2)
  , estStdev = truncate (sqrt d)
  }
  where
    fit x1 x2 = x1 `quot` 5 + 2 * (x2 `quot` 5)
    t = fit t1 t2
    t' = word64ToDouble t
    d = sqr (word64ToDouble t1 - t') + sqr (word64ToDouble t2 - 2 * t')

predictPerturbed :: Measurement -> Measurement -> Estimate
predictPerturbed t1 t2 = Estimate
  { estMean = estMean (predict t1 t2)
  , estStdev = max
    (estStdev (predict (lo t1) (hi t2)))
    (estStdev (predict (hi t1) (lo t2)))
  }
  where
    prec = max (fromInteger cpuTimePrecision) 1000000000 -- 1 ms
    hi meas = meas { measTime = measTime meas + prec }
    lo meas = meas { measTime = measTime meas - prec }

measure :: Config -> Word64 -> Benchmarkable -> IO Measurement
measure cfg n Benchmarkable{..} =
  bracket (allocEnv n) (cleanEnv n) $ \env0 -> do
    let getTimePicoSecs' = getTimePicoSecs (cfgTimeMode cfg)
    performGC
    startTime <- getTimePicoSecs'
    (startAllocs, startCopied, startMaxMemInUse) <- getAllocsAndCopied
    runRepeatedly env0 n
    endTime <- getTimePicoSecs'
    performMinorGC -- update RTSStats
    (endAllocs, endCopied, endMaxMemInUse) <- getAllocsAndCopied
    let meas = Measurement
          { measTime   = endTime - startTime
          , measAllocs = endAllocs - startAllocs
          , measCopied = endCopied - startCopied
          , measMaxMem = max endMaxMemInUse startMaxMemInUse
          }

    debugStr cfg $
      show n
      ++ (if n == 1 then " iteration gives " else " iterations give ")
      ++ formatMeasurement meas ++ "\n"

    pure meas

measureUntil :: Config -> Benchmarkable -> IO Estimate
measureUntil cfg@Config{..} b = do
  t1 <- measure' 1 b
  if isInfinite cfgRelStDev && cfgRelStDev > 0
    then pure Estimate {estMean = t1, estStdev = 0}
    else getAggregateMaybe t1 >>= go 1 t1 0
  where
    measure' = measure cfg

    numInit :: Num a => a
    numInit = 8

    getAggregateMaybe t1
      | perRun b = do
          ts <- replicateM (numInit - 1) (measure' 1 b)
          pure $ Just $ initAgg (t1:ts)
      | otherwise = pure Nothing

    go :: Word64 -> Measurement -> Word64 -> Maybe Aggregate -> IO Estimate
    go n t1 sumOfTs mb_agg = do
      let n' | perRun b = 1
             | otherwise = 2 * n
          scale = (`quot` n)
          sumOfTs' = sumOfTs + measTime t1

      t2 <- measure' n' b

      let Estimate (Measurement meanN allocN copiedN maxMemN) stdevN =
            case mb_agg of
              Nothing  -> predictPerturbed t1 t2
              Just agg -> aggToEstimate t1 t2 agg
          isTimeoutSoon =
            case cfgTimeout of
              NoTimeout -> False
              Timeout us ->
                let extra | perRun b = (3 + numInit) * meanN
                          | otherwise = 3 * measTime t2
                    divis = 1000000 * 10 `quot` 12
                in  (sumOfTs' + extra) `quot` divis >= fromIntegral us
          isStDevInTargetRange =
            stdevN < truncate (cfgRelStDev * word64ToDouble meanN)
          meas = Measurement (scale meanN) (scale allocN) (scale copiedN) maxMemN
          mb_agg' = updateAgg (measTime t2) <$> mb_agg

      case cfgTimeout of
        NoTimeout | sumOfTs' + measTime t2 > 100 * 1000000000000 ->
          hPutStrLn stderr $
                    "\n" ++
                    "This benchmark takes more than 100 seconds.\n" ++
                    "Conosider setting --time-limit, if this is\n" ++
                    "unexpected (or to silence this warning)."
        _ -> pure ()

      if isStDevInTargetRange || isTimeoutSoon
        then pure $ Estimate {estMean = meas, estStdev = scale stdevN}
        else go n' t2 sumOfTs' mb_agg'


-- ------------------------------------------------------------------------
-- State for perRunEnvWithCleanup
-- ------------------------------------------------------------------------

data Aggregate = Aggregate
  { aggCount :: {-# UNPACK #-} !Word64 -- ^ Number of computations.
  , aggMean  :: {-# UNPACK #-} !Double -- ^ Mean of the time.
  , aggM2    :: {-# UNPACK #-} !Double
  -- ^ Sum of squares of differences from the current mean.
  }

aggToEstimate :: Measurement -> Measurement -> Aggregate -> Estimate
aggToEstimate (Measurement _ a1 c1 m1) (Measurement _ a2 c2 m2) agg = est
  where
    est = Estimate mean stdev
    mean | hasGCStats = Measurement am' (avg a1 a2) (avg c1 c2) (max m1 m2)
         | otherwise  = Measurement am' 0 0 0
    avg a b = (a + b) `quot` 2
    stdev = truncate (sqrt (aggM2 agg / word64ToDouble (aggCount agg - 1)))
    am' = truncate (aggMean agg)

-- Welford's online algorithm, see:
--
--   https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_online_algorithm

updateAgg :: Word64 -> Aggregate -> Aggregate
updateAgg t (Aggregate n am am2) = Aggregate n' am' am2'
  where
    n' = n + 1
    am' = am + (delta / word64ToDouble n')
    am2' = am2 + (delta * delta2)
    delta = t' - am
    delta2 = t' - am'
    t' = word64ToDouble t

initAgg :: [Measurement] -> Aggregate
initAgg ms = Aggregate {aggCount = n, aggMean = mean0, aggM2 = m20}
  where
    n :: Num a => a
    n = fromIntegral (length ms)
    mean0 = word64ToDouble (foldr ((+) . measTime) 0 ms) / n
    m20 = foldr ((+) . sqrdiff) 0 ms / (n - 1)
    sqrdiff t = sqr (mean0 - word64ToDouble (measTime t))


-- ------------------------------------------------------------------------
-- Converting numbers
-- ------------------------------------------------------------------------

#if !MIN_VERSION_base(4,10,0) && MIN_VERSION_base(4,6,0)
int64ToWord64 :: Int64 -> Word64
int64ToWord64 = fromIntegral
{-# INLINE int64ToWord64 #-}
#endif

int64ToDouble :: Int64 -> Double
int64ToDouble = fromIntegral
{-# INLINE int64ToDouble #-}

word64ToInt64 :: Word64 -> Int64
word64ToInt64 = fromIntegral
{-# INLINE word64ToInt64 #-}

word64ToDouble :: Word64 -> Double
word64ToDouble = fromIntegral
{-# INLINE word64ToDouble #-}


-- ------------------------------------------------------------------------
-- Running function repeatedly
-- ------------------------------------------------------------------------

-- criterion-measurement-0.2.1 uses NOINLINE pragma, gauge-0.2.5 and
-- tasty-bench-0.3.4 use INLINE pragma for following wrapper
-- functions.  At the moment, this module is using NOINLINE.

#if !MIN_VERSION_base(4,15,0)
data SPEC = SPEC
{-# ANN type SPEC ForceSpecConstr #-}
#endif

funcToBench :: (b -> c) -> (a -> b) -> a -> Word64 -> IO ()
funcToBench frc = benchLoop SPEC
  where
    -- Explicitly passing `f' and `x' as the arguments of `benchLoop',
    -- so that ghc won't optimize away them. This approach is taken in
    -- tasty-bench. Criterion, as of criterion-measurement 0.2.1,
    -- defines the looping function in a separate module and that
    -- module has -fno-full-laziness GHC_OPTIONS pragma hard coded.
    benchLoop !_ f x n
      | n == 0 = pure ()
      | otherwise = do
          val <- evaluate (f x)
          frc val `seq` benchLoop SPEC f x (n - 1)
{-# NOINLINE funcToBench #-}

ioToBench :: (a -> b) -> IO a -> (Word64 -> IO ())
ioToBench frc a = go
  where
    go n
      | n == 0 = pure ()
      | otherwise = do
          val <- a
          frc val `seq` go (n - 1)
{-# NOINLINE ioToBench #-}

ioFuncToBench :: (b -> c) -> (a -> IO b) -> a -> Word64 -> IO ()
ioFuncToBench frc f x = go
  where
    go n
      | n <= 0 = pure ()
      | otherwise = do
          val <- f x
          frc val `seq` go (n - 1)
{-# NOINLINE ioFuncToBench #-}


-- ------------------------------------------------------------------------
-- Windows stuffs
-- ------------------------------------------------------------------------

#if defined(mingw32_HOST_OS)
#  if defined(i386_HOST_ARCH)
foreign import stdcall unsafe "windows.h GetConsoleOutputCP"
  getConsoleOutputCP :: IO Word32
foreign import stdcall unsafe "windows.h SetConsoleOutputCP"
  setConsoleOutputCP :: Word32 -> IO ()
#  else
foreign import ccall unsafe "windows.h GetConsoleOutputCP"
  getConsoleOutputCP :: IO Word32
foreign import ccall unsafe "windows.h SetConsoleOutputCP"
  setConsoleOutputCP :: Word32 -> IO ()
#  endif
#endif
