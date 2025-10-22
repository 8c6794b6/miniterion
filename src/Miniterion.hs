{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
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

  , Config
  , defaultConfig

  , MEnv
  , getDefaultMEnv

  , Doc
  , docToString
#endif
  ) where

-- base
import           Control.Exception     (Exception (..), SomeException (..),
                                        bracket, evaluate, finally, handle,
                                        throw, throwIO)
import           Control.Monad         (guard, replicateM_, void, when)
import           Data.Char             (toLower)
import           Data.Foldable         (find)
import           Data.Int              (Int64)
import           Data.List             (isPrefixOf, nub, stripPrefix, tails)
import           Data.String           (IsString (..))
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
benchmark = void . runBenchmark defaultMEnv . bench "..."


-- ------------------------------------------------------------------------
-- Main
-- ------------------------------------------------------------------------

defaultMainWith :: Config -> [Benchmark] -> IO ()
defaultMainWith cfg0 bs = handleMiniterionException $ do
  args <- getArgs
  let (opts, pats, invalids, errs) = getOpt' Permute options args
      cfg1 = foldl' (flip id) cfg0 opts
  default_menv <- getDefaultMEnv cfg1
  let menv0 = default_menv {mePatterns = pats}
      root_bs = bgroup "" bs
      do_iter n = iterBenchmark n menv0 root_bs >>= summariseResults
      do_bench menv = runBenchmark menv root_bs >>= summariseResults
  case cfgRunMode cfg1 of
    Help     -> showHelp menv0
    _         | not (null errs)     -> errorOptions errs
              | not (null invalids) -> invalidOptions invalids
    Version  -> putStrLn builtWithMiniterion
    DoList   -> showNames menv0 root_bs
    DoIter n -> do_iter n
    DoBench  -> withCsvSettings menv0 do_bench

showHelp :: MEnv -> IO ()
showHelp menv = do
  me <- fmap fromString getProgName
  putStr $ (`usageInfo` options) $ docToString menv $ mconcat
    [ "Microbenchmark suite - " <> stringToDoc builtWithMiniterion <> "\n\n"
    , boldYellow "USAGE:" <> " " <> boldGreen me <> " [OPTIONS] [PATTERN]...\n\n"
    , boldYellow "ARGS:\n"
    , "  <PATTERN>...  Pattern(s) to select running benchmarks. If no pattern was\n"
    , "                given, run all benchmarks. Multiple patterns are combined\n"
    , "                with 'OR'. Selections are done by prefix match by default.\n"
    , "                See also \"--match\" option below.\n\n"
    , boldYellow "OPTIONS:"
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

showNames :: MEnv -> Benchmark -> IO ()
showNames menv =
  mapM_ (\n -> when (isMatched menv n) (putStrLn n)) . benchNames []


-- ------------------------------------------------------------------------
-- Miniterion's environment
-- ------------------------------------------------------------------------

-- | Internal environment for miniterion.
data MEnv = MEnv
  { meConfig          :: !Config
    -- ^ Configuration of this environment.
  , mePatterns        :: ![String]
    -- ^ Patterns to filter running benchmarks
  , meCsvHandle       :: !(Maybe Handle)
    -- ^ File handle to write benchmark result in CSV format.
  , meBaselineSet     :: !Baseline
    -- ^ Set containing baseline information, made from the file
    -- specified by 'cfgBaselinePath'.
  , meUseColor        :: !Bool
    -- ^ 'True' if the output is a terminal device.
  , meSupportsUnicode :: !Bool
    -- ^ 'True' if unicode is supported.
  , meHasGCStats      :: !Bool
    -- ^ 'True' if GC statistics are available.
  }

-- | The default environment.
defaultMEnv :: MEnv
defaultMEnv = MEnv
  { meCsvHandle = Nothing
  , meBaselineSet = mempty
  , mePatterns = []
  , meConfig = defaultConfig
  , meUseColor = False
  , meSupportsUnicode = False
  , meHasGCStats = False
  }

-- | Get the default t'MEnv' from given t'Config'.
getDefaultMEnv :: Config -> IO MEnv
getDefaultMEnv !cfg = do
  use_color <- case cfgUseColor cfg of
    Always -> pure True
    Never  -> pure False
    Auto   -> hIsTerminalDevice stdout
  supports_unicode <- isUnicodeSupported
  has_gc_stats <- getGCStatsEnabled
  pure $! defaultMEnv
    { meConfig = cfg
    , meUseColor = use_color
    , meSupportsUnicode = supports_unicode
    , meHasGCStats = has_gc_stats
    }


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
      bs = if 1 < num_result then "benchmarks" else "benchmark" :: String
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

runBenchmark :: MEnv -> Benchmark -> IO [Result]
runBenchmark = runBenchmarkWith runBenchmarkable

iterBenchmark :: Word64 -> MEnv -> Benchmark -> IO [Result]
iterBenchmark n = runBenchmarkWith (iterBenchmarkable n)

runBenchmarkWith :: (MEnv -> [String] -> String -> Benchmarkable -> IO a)
                 -> MEnv -> Benchmark -> IO [a]
runBenchmarkWith !run menv = go []
  where
    go !acc0 bnch = case bnch of
      Bench name act -> pure <$> run menv acc0 name act
      Bgroup name bs ->
        let acc1 = consNonNull name acc0
            to_run = filter (any (isMatched menv) . benchNames acc1) bs
        in  concat <$> mapM (go acc1) to_run
      Environment alloc clean f ->
        let alloc' = alloc >>= \e -> evaluate (rnf e) >> pure e
        in  bracket alloc' clean (go acc0 . f)

runBenchmarkable :: MEnv -> [String] -> String -> Benchmarkable -> IO Result
runBenchmarkable menv@MEnv {meConfig=Config{..}} parents name b = do
  let fullname = pathToName parents name

  infoBenchname menv fullname
  debugStr menv "\n"
  hFlush stdout

  mb_sum <- withTimeout cfgTimeout (measureUntil menv b)

  let (result, mb_cmp) = case mb_sum of
        Nothing -> (TimedOut fullname, Nothing)
        Just (Summary est _ _ _ _) ->
          case compareVsBaseline (meBaselineSet menv) fullname est of
            Nothing  -> (Done, Nothing)
            Just cmp ->
              let is_acceptable
                    | 1 + cfgFailIfSlower <= cmp = TooSlow fullname
                    | cmp <= 1 - cfgFailIfFaster = TooFast fullname
                    | otherwise                  = Done
              in  (is_acceptable, Just cmp)

  infoStr menv (formatResult result mb_sum mb_cmp)
  mapM_ (putCsvLine menv fullname mb_sum) (meCsvHandle menv)
  pure result

iterBenchmarkable :: Word64 -> MEnv -> [String] -> String -> Benchmarkable
                  -> IO Result
iterBenchmarkable n menv@MEnv{meConfig=cfg} parents name Benchmarkable{..} = do
  let fullname = pathToName parents name
      -- Repeating the `run 1' when the perRun field of the benchmark
      -- is True, to initialize and cleanup the environment every
      -- time.
      go | perRun    = replicateM_ (fromIntegral n) (run 1)
         | otherwise = run n
        where
          run i =
            bracket (allocEnv i) (cleanEnv i) (`runRepeatedly` i)

  infoBenchname menv fullname
  hFlush stdout
  mb_unit <- withTimeout (cfgTimeout cfg) go

  case mb_unit of
    Just () -> infoStr menv "\n" >> pure Done
    _ -> do
      let result = TimedOut fullname
      infoStr menv (formatResult result Nothing Nothing)
      pure result

infoBenchname :: MEnv -> String -> IO ()
infoBenchname menv name =
  infoStr menv (white "benchmarking " <> boldCyan (fromString name) <> " ")
{-# INLINE infoBenchname #-}

withTimeout :: Timeout -> IO a -> IO (Maybe a)
withTimeout tout act = case tout of
  Timeout micro -> timeout (fromIntegral micro) act
  NoTimeout     -> fmap Just act

benchNames :: [String] -> Benchmark -> [String]
benchNames = go
  where
    go !acc b = case b of
      Bench name _      -> [pathToName acc name]
      Bgroup name bs    -> concatMap (go (consNonNull name acc)) bs
      Environment _ _ f -> go acc (f (throw (UninitializedEnv acc)))

pathToName :: [String] -> String -> String
pathToName !prevs !me = foldl' (\b a -> a ++ "/" ++ b) me prevs

groupsToName :: [String] -> String
groupsToName = \case
  []    -> ""
  hd:tl -> pathToName tl hd

consNonNull :: String -> [String] -> [String]
consNonNull !x !xs = if null x then xs else x : xs

noop :: Applicative m => a -> m ()
noop = const (pure ())
{-# INLINE noop #-}


-- ------------------------------------------------------------------------
-- Printing with verbosity
-- ------------------------------------------------------------------------

infoStr, debugStr :: MEnv -> Doc -> IO ()
infoStr = putDocWith 1
debugStr = putDocWith 2

putDocWith :: Int -> MEnv -> Doc -> IO ()
putDocWith n menv doc =
  when (n <= cfgVerbosity (meConfig menv)) $ putDoc menv doc


-- ------------------------------------------------------------------------
-- Formatting
-- ------------------------------------------------------------------------

formatResult :: Result -> Maybe Summary -> Maybe Double -> Doc
formatResult _ Nothing _ =
  boldRed "FAIL" <> "\n" <>
  yellow "Timed out while running this benchmark\n\n"
formatResult res (Just summary) mb_cmp =
  fail_or_blank <> "\n" <>
  --
  white "time                 " <> showPicos5 (irMid ols)   <> "   " <>
  show_minmax (irLo ols) (irHi ols) <>
  maybe "" (formatSlowDown res) mb_cmp <> "\n" <>
  --
  id    "                     " <> rsq_str id (irMid rsq) <> "   " <>
  show_minmax_rsq <> "\n" <>
  --
  white "mean                 " <> showPicos5 (measTime m) <> "   " <>
  show_minmax (irLo mean) (irHi mean) <> "\n" <>
  --
  white "std dev              " <> showPicos5 (2 * irMid sd) <> "   " <>
  show_minmax (2 * irLo sd) (2 * irHi sd) <>
  --
  formatGC m <> "\n\n"
  where
    Summary (Estimate m _) ols rsq sd mean = summary
    fail_or_blank
      | isTooFast res || isTooSlow res = boldRed "FAIL"
      | otherwise = ""
    show_minmax lo hi =
      white ("(" <> showPicos5 lo <> " .. " <> showPicos5 hi <> ")")
    rsq_str on_other val = color (stringToDoc (printf "%.3f R²" val))
      where
        color | val < 0.90 = boldRed
              | val < 0.99 = yellow
              | otherwise = on_other
    show_minmax_rsq =
      white "(" <> rsq_str white (irLo rsq) <> white " .. " <>
      rsq_str white (irHi rsq) <> white ")"

formatSlowDown :: Result -> Double -> Doc
formatSlowDown result ratio = case percents `compare` 0 of
  LT -> in_yellow isTooFast $ printf " (%2i%% less than baseline)" (-percents)
  EQ -> white                         "       (same as baseline)"
  GT -> in_yellow isTooSlow $ printf " (%2i%% more than baseline)" percents
  where
    percents :: Int64
    percents = truncate ((ratio - 1) * 100)
    in_yellow test = (if test result then yellow else white) . fromString

-- | Show picoseconds, fitting number in 5 characters.
showPicos5 :: Word64 -> Doc
showPicos5 i
  | t < 10     = f $ printf "%.3f ps" t
  | t < 100    = f $ printf "%.2f ps" t
  | t < 1000   = f $ printf "%.1f ps" t
  | t < 999e1  = f $ printf "%.3f ns" (t / 1e3)
  | t < 999e2  = f $ printf "%.2f ns" (t / 1e3)
  | t < 999e3  = f $ printf "%.1f ns" (t / 1e3)
  | t < 999e4  = print_mu "%.3f %cs"
  | t < 999e5  = print_mu "%.2f %cs"
  | t < 999e6  = print_mu "%.1f %cs"
  | t < 999e7  = f $ printf "%.3f ms" (t / 1e9)
  | t < 999e8  = f $ printf "%.2f ms" (t / 1e9)
  | t < 999e9  = f $ printf "%.1f ms" (t / 1e9)
  | t < 999e10 = f $ printf "%.3f s " (t / 1e12)
  | t < 999e11 = f $ printf "%.2f s " (t / 1e12)
  | t < 999e12 = f $ printf "%.1f s " (t / 1e12)
  | otherwise  = f $ printf "%4.1f s" (t / 1e12)
  where
    t = word64ToDouble i
    f = fromString
    print_mu fmt = Doc (printf fmt (t / 1e6) . mu)

formatGC :: Measurement -> Doc
formatGC (Measurement _ a c p) = Doc $ \ !e ->
  if meHasGCStats e then
    let sb !b = fromString $! showBytes b
    in  docToString e $ "\n" <>
        white "        alloc  copied    peak" <> "\n" <>
        white "gc     " <> sb a <> "  " <> sb c <> "  " <> sb p
  else
    ""

-- | Show bytes with unit.
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

formatMeasurement :: MEnv -> Word64 -> Measurement -> Doc
formatMeasurement menv n (Measurement t a c m) =
  showPicos5 (t `quot` n) <> fromString (printf " (%d/%d)" t n) <>
  if meHasGCStats menv then
    fromString (printf " alloc: %d copied: %d max: %d" a c m)
  else
    ""


-- ------------------------------------------------------------------------
-- Matching benchmark names
-- ------------------------------------------------------------------------

data MatchMode
  = Pattern -- ^ Substring match
  | Prefix  -- ^ Prefix match
  | IPattern -- ^ Case insensitive prefix match
  | Glob -- ^ Glob pattern match

isMatched :: MEnv -> String -> Bool
isMatched MEnv{..} fullname = null mePatterns || has_match
  where
    has_match = any is_match mePatterns
    is_match str = case cfgMatch meConfig of
      Glob     -> glob str fullname
      IPattern -> substring (map toLower str) (map toLower fullname)
      Pattern  -> substring str fullname
      Prefix   -> str `isPrefixOf` fullname

substring :: String -> String -> Bool
substring pat = any (pat `isPrefixOf`) . tails

-- | Simple, inefficient, and improper glob. Does not support special
-- character class names like @[:alnum:]@, @[:digit:]@, ... etc.
glob :: String -> String -> Bool
glob pat0 = go pat0
  where
    go [] [] = True
    go ('\\':p:ps) (c:cs) | p == c = go ps cs
    go ('?':ps) (_:cs) = go ps cs
    go ['*'] _ = True
    go ('*':ps) cs = any (go ps) (cs : tails cs)
    go ('[':'!':ps) (c:cs) = cclass notElem c ps cs
    go ('[':ps) (c:cs) = cclass elem c ps cs
    go ('{':ps) cs = brace ps cs
    go (p:ps) (c:cs) | p == c = go ps cs
    go _ _ = False

    cclass test c ps cs = lp False [] ps
      where
        lp close acc xs =
          case xs of
            []              -> throw (GlobUnbalancedBracket pat0)
            '\\':x:xs'      -> lp True (x:acc) xs'
            ']':xs' | close -> test c acc && go xs' cs
            x0:'-':']':xs'  -> test c ('-':x0:acc) && go xs' cs
            x0:'-':x1:xs'   -> lp True ([x0 .. x1] ++ acc) xs'
            x:xs'           -> lp True (x:acc) xs'

    brace ps cs = any (\p -> go (p ++ ps') cs) pats
      where
        (pats, ps') = alts (0 :: Int) [] [] ps
        alts depth tmp acc xs = case xs of
          []         -> throw (GlobUnbalancedBrace pat0)
          '\\':x:xs' -> alts depth (x:'\\':tmp) acc xs'
          x:xs'      -> case x of
            '}' | depth == 0 -> (reverse (reverse tmp : acc), xs')
                | otherwise  -> alts (depth - 1) (x:tmp) acc xs'
            '{'              -> alts (depth + 1) (x:tmp) acc xs'
            ',' | depth == 0 -> alts depth [] (reverse tmp : acc) xs'
            _other           -> alts depth (x:tmp) acc xs'


-- ------------------------------------------------------------------------
-- Terminal stuffs
-- ------------------------------------------------------------------------

yellow, white :: Doc -> Doc
yellow = coloredDoc "0;33"
white = coloredDoc "0;37"

boldRed, boldGreen, boldYellow, boldCyan :: Doc -> Doc
boldRed = coloredDoc "1;31"
boldGreen = coloredDoc "1;32"
boldYellow = coloredDoc "1;33"
boldCyan = coloredDoc "1;36"

coloredDoc :: String -> Doc -> Doc
coloredDoc !param (Doc !g) = Doc f
  where
    f !e
      | meUseColor e = "\ESC[" ++ param ++ "m" ++ g e ++ "\ESC[0m"
      | otherwise = g e
{-# INLINABLE coloredDoc #-}

-- | Unit character for microseconds.
mu :: MEnv -> Char
mu menv = if meSupportsUnicode menv then 'μ' else 'u'
{-# INLINE mu #-}

isUnicodeSupported :: IO Bool
#if MIN_VERSION_base(4,5,0)
isUnicodeSupported = do
  enc <- getLocaleEncoding
  let utf_prefix = take 3 (textEncodingName enc) == "UTF"
#  if defined(mingw32_HOST_OS)
  is_65001 <- fmap (== 65001) getConsoleOutputCP
  pure utf_prefix && is_65001
#  else
  pure utf_prefix
#  endif
#else
  pure False
#endif
{-# INLINABLE isUnicodeSupported #-}


-- ------------------------------------------------------------------------
-- Terminal specific string
-- ------------------------------------------------------------------------

-- | Newtype wrapper for 'String' taking t'MEnv', to decide whether to
-- use color and unicode.
newtype Doc = Doc {unDoc :: MEnv -> String}

instance Semigroup Doc where
  Doc !d1 <> Doc !d2 = Doc (\ !e -> d1 e <> d2 e)
  {-# INLINE (<>) #-}

instance Monoid Doc where
  mempty = Doc (const "")
  {-# INLINE mempty #-}

instance IsString Doc where
  fromString = stringToDoc
  {-# INLINE fromString #-}

-- | Lift given 'String' to t'Doc'.
stringToDoc :: String -> Doc
stringToDoc !str = Doc (\ !_ -> str)
{-# INLINE stringToDoc #-}

-- | Convert t'Doc' to 'String'.
docToString :: MEnv -> Doc -> String
docToString !menv !d = unDoc d menv
{-# INLINE docToString #-}

-- | Like 'putStr', but for t'Doc'.
putDoc :: MEnv -> Doc -> IO ()
putDoc !menv = putStr . docToString menv
{-# INLINE putDoc #-}


-- ------------------------------------------------------------------------
-- CSV
-- ------------------------------------------------------------------------

-- XXX: Could use `Data.Set.Set'.
type Baseline = [String]

withCsvSettings :: MEnv -> (MEnv -> IO a) -> IO a
withCsvSettings menv0@MEnv{meConfig=cfg} act = do
  baseline <- maybe mempty readBaseline (cfgBaselinePath cfg)
  let menv1 = menv0 {meBaselineSet = baseline}
  case cfgCsvPath cfg of
    Nothing -> act menv1 {meCsvHandle = Nothing}
    Just path -> withFile path WriteMode $ \hdl -> do
      hSetBuffering hdl LineBuffering
      let extras | meHasGCStats menv0 = ",Allocated,Copied,Peak Memory"
                 | otherwise = ""
          header = "Name,Mean,MeanLB,MeanUB,Stddev,StddevLB,StddevUB"
      hPutStrLn hdl (header ++ extras)
      act menv1 {meCsvHandle = Just hdl}

putCsvLine :: MEnv -> String -> Maybe Summary -> Handle -> IO ()
putCsvLine menv name mb_summary hdl =
  let put_line s = hPutStrLn hdl (encodeCsv name ++ "," ++ csvSummary menv s)
  in  maybe (pure ()) put_line mb_summary

csvSummary :: MEnv -> Summary -> String
csvSummary menv (Summary (Estimate m _) _ols _rsq stdev mean)
  | meHasGCStats menv = time ++ "," ++ gc
  | otherwise = time
  where
    time =
      show (measTime m) ++ "," ++
      show (irLo mean) ++ "," ++ show (irHi mean) ++ "," ++
      show (2 * irMid stdev) ++ "," ++
      show (2 * irLo stdev) ++ "," ++ show (2 * irHi stdev)
    gc =
      show (measAllocs m) ++ "," ++ show (measCopied m) ++ "," ++
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

      (time_cell, ',' : rest0) <- span (/= ',') <$> stripPrefix prefix line
      (_time_lb_cell, ',' : rest1) <- pure (span (/= ',') rest0)
      (_time_ub_cell, ',' : rest2) <- pure (span (/= ',') rest1)
      let sigma_x_2_cell = takeWhile (/= ',') rest2
      (,) <$> readMaybe time_cell <*> readMaybe sigma_x_2_cell

encodeCsv :: String -> String
encodeCsv xs
  | any (`elem` xs) (",\"\n\r" :: String) = '"' : go xs -- opening quote
  | otherwise = xs
  where
    go []         = ['"'] -- closing quote
    go ('"' : ys) = '"' : '"' : go ys
    go (y : ys)   = y : go ys


-- ------------------------------------------------------------------------
-- Configuration
-- ------------------------------------------------------------------------

-- | Data type to hold configuration information.
data Config = Config
  { cfgRunMode      :: !RunMode
    -- ^ Mode to run the main program.
  , cfgUseColor     :: !UseColor
    -- ^ When to use colored outputs.
  , cfgBaselinePath :: Maybe FilePath
    -- ^ Path to a file containing baseline data, usually a CSV file
    -- made with @--csv@ option in advance.
  , cfgCsvPath      :: Maybe FilePath
    -- ^ Path to a file for writing results in CSV format.
  , cfgFailIfFaster :: Double
    -- ^ Upper bound of acceptable speed up.
  , cfgFailIfSlower :: Double
    -- ^ Upper bound of acceptable slow down.
  , cfgMatch        :: !MatchMode
    -- ^ Which mode to use for benchmark name pattern match.
  , cfgRelStDev     :: !Double
    -- ^ Relative standard deviation for measuring benchmarks.
  , cfgTimeMode     :: !TimeMode
    -- ^ Time mode for measuring benchmarks.
  , cfgTimeout      :: !Timeout
    -- ^ Timeout duration in seconds.
  , cfgVerbosity    :: !Int
    -- ^ Verbosity level.
  }

-- | Mode to execute the main function.
data RunMode
  = Help          -- ^ Show help message.
  | Version       -- ^ Show version info.
  | DoList        -- ^ Show benchmark names.
  | DoIter Word64 -- ^ Run benchmarks, don't analyse.
  | DoBench       -- ^ Run benchmarks.

-- | When to use colored output.
data UseColor
  = Always -- ^ Always use color.
  | Auto   -- ^ Use color if the output is a terminal device.
  | Never  -- ^ Don't use color.

-- | Default configuration used for running benchmarks.
defaultConfig :: Config
defaultConfig = Config
  { cfgRunMode = DoBench
  , cfgUseColor = Auto
  , cfgBaselinePath = Nothing
  , cfgCsvPath = Nothing
  , cfgFailIfFaster = 1.0 / 0.0
  , cfgFailIfSlower = 1.0 / 0.0
  , cfgMatch = Prefix
  , cfgRelStDev = 0.05
  , cfgTimeMode = CpuTime
  , cfgTimeout = NoTimeout
  , cfgVerbosity = 1
  }

options :: [OptDescr (Config -> Config)]
options =
  [ Option ['h'] ["help"]
    (NoArg (\o -> o {cfgRunMode = Help}))
    "Show this help text"

  , Option ['L'] ["time-limit"]
    (ReqArg (\str o -> case readMaybe str :: Maybe Double of
                Just n -> o {cfgTimeout = Timeout (floor (1e6 * n))}
                _      -> throw (InvalidArgument "time-limit" str))
      "SECS")

    (unlines
      ["Time limit to run a benchmark"
      ,"(default: no timeout)"])

    , Option [] ["color"]
      (let whens = [("always", Always)
                   ,("auto", Auto)
                   ,("never", Never)]
           match str = isPrefixOf str . fst
       in  ReqArg (\str o -> case find (match str) whens of
                      Just (_, uc) -> o {cfgUseColor = uc}
                      _            -> throw (InvalidArgument "color" str))
           "WHEN")
      (unlines
       ["When to use colors, \"auto\", \"always\", or \"never\""
       ,"(default: auto)"])

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

  , Option ['n'] ["iters"]
    (ReqArg (\str o -> case readMaybe str :: Maybe Word64 of
                Just n -> o {cfgRunMode = DoIter n}
                _      -> throw (InvalidArgument "iters" str))
    "INT")
    "Run benchmarks, don't analyse"

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
    (NoArg (\o -> o {cfgRunMode = DoList}))
    "List benchmarks"

  , Option [] ["version"]
    (NoArg (\o -> o {cfgRunMode = Version}))
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
  | GlobUnbalancedBrace String
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
  GlobUnbalancedBrace pat ->
    "unbalanced brace in glob pattern `" ++ pat ++ "'"
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

getAllocsAndCopied :: MEnv -> IO (Word64, Word64, Word64)
getAllocsAndCopied menv =
#if MIN_VERSION_base(4,10,0)
  if not (meHasGCStats menv) then pure (0, 0, 0) else
    (\s -> (allocated_bytes s, copied_bytes s, max_mem_in_use_bytes s))
    <$> getRTSStats
#elif MIN_VERSION_base(4,6,0)
  if not (meHasGCStats menv) then pure (0, 0, 0) else
    (\s -> (int64ToWord64 $ bytesAllocated s,
            int64ToWord64 $ bytesCopied s,
            int64ToWord64 $ peakMegabytesAllocated s * 1024 * 1024))
    <$> getGCStats
#else
    pure (0, 0, 0)
#endif
{-# INLINABLE getAllocsAndCopied #-}

getGCStatsEnabled :: IO Bool
#if MIN_VERSION_base(4,10,0)
getGCStatsEnabled = getRTSStatsEnabled
#elif MIN_VERSION_base(4,6,0)
getGCStatsEnabled = getGCStatsEnabled
#else
getGCStatsEnabled = pure False
#endif
{-# INLINE getGCStatsEnabled #-}


-- ------------------------------------------------------------------------
-- Measuring
-- ------------------------------------------------------------------------

data Timeout
  = Timeout !Word64
  -- ^ Duration in microseconds (e.g., 2000000 for 2 seconds).
  | NoTimeout
  -- ^ Run without timeout.

data Measurement = Measurement
  { measTime   :: {-# UNPACK #-} !Word64 -- ^ time in picoseconds
  , measAllocs :: {-# UNPACK #-} !Word64 -- ^ allocations in bytes
  , measCopied :: {-# UNPACK #-} !Word64 -- ^ copied bytes
  , measMaxMem :: {-# UNPACK #-} !Word64 -- ^ max memory in use
  }

-- | Measurement paired with end time.
type Measured = (Measurement, Word64)

data Estimate = Estimate
  { estMean  :: {-# UNPACK #-} !Measurement
  , estStdev :: {-# UNPACK #-} !Word64 -- ^ stdev in picoseconds
  }

type OLS = Ranged Word64
type R2 = Ranged Double
type Mean = Ranged Word64
type Stdev = Ranged Word64

data Summary = Summary
  { smEstimate :: {-# UNPACK #-} !Estimate
  , smOLS      :: {-# UNPACK #-} !OLS
  , smR2       :: {-# UNPACK #-} !R2
  , smStdev    :: {-# UNPACK #-} !Stdev
  , smMean     :: {-# UNPACK #-} !Mean
  }

square :: Num a => a -> a
square x = x * x
{-# INLINE square #-}

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
    d = square (word64ToDouble t1 - t') + square (word64ToDouble t2 - 2 * t')

predictPerturbed :: Measurement -> Measurement -> Estimate
predictPerturbed t1 t2 = Estimate
  { estMean = estMean (predict t1 t2)
  , estStdev = max
    (estStdev (predict (lo t1) (hi t2)))
    (estStdev (predict (hi t1) (lo t2)))
  }
  where
    hi meas = meas { measTime = measTime meas + precision }
    lo meas | measTime meas > precision =
              meas { measTime = measTime meas - precision }
            | otherwise = meas { measTime = 0 }

precision :: Word64
precision = max (fromInteger cpuTimePrecision) oneMillisecond
{-# INLINEABLE precision #-}

-- | One millisecond in picoseconds.
oneMillisecond :: Num a => a
oneMillisecond = 1000000000
{-# INLINE oneMillisecond #-}

-- See 'Criterion.Measurement.runBenchmarkable' in the
-- criterion-measurement package.
runLoop :: Benchmarkable -> Word64 -> (Word64 -> IO () -> IO Measured)
        -> IO Measured
runLoop Benchmarkable{..} n f
  | perRun    = work >>= go (n - 1)
  | otherwise = work
  where
    go 0 result   = pure result
    go !i !result = work >>= go (i - 1) . comb result

    count | perRun = 1
          | otherwise = n

    work = do
      e <- allocEnv count
      let clean = cleanEnv count e
          run = runRepeatedly e count
      clean `seq` run `seq` evaluate (rnf e)
      f count run `finally` clean
    {-# INLINE work #-}

    comb (!m1, _) (!m2, !e2) = (m3, e2)
      where
        on h g = h (g m1) (g m2)
        add = on (+)
        max_of = on max
        m3 = Measurement { measTime = add measTime
                         , measAllocs = add measAllocs
                         , measCopied = add measCopied
                         , measMaxMem = max_of measMaxMem
                         }
    {-# INLINE comb #-}
{-# INLINE runLoop #-}

measure :: MEnv -> Word64 -> Benchmarkable -> IO Measured
measure menv@MEnv{meConfig=cfg} num b =
  runLoop b num $ \ !n act -> do
    let getTimePicoSecs' = getTimePicoSecs (cfgTimeMode cfg)
    performMinorGC
    start_time <- getTimePicoSecs'
    (start_allocs, start_copied, start_max_mem) <- getAllocsAndCopied menv
    act
    end_time <- getTimePicoSecs'
    performMinorGC
    (end_allocs, end_copied, end_max_mem) <- getAllocsAndCopied menv
    let meas = Measurement
          { measTime = end_time - start_time
          , measAllocs = end_allocs - start_allocs
          , measCopied = end_copied - start_copied
          , measMaxMem = max end_max_mem start_max_mem
          }

    debugStr menv $
      fromString (show n) <>
      (if n == 1 then " iteration gives " else " iterations give ") <>
      formatMeasurement menv n meas <> "\n"

    pure (meas, end_time)

measureUntil :: MEnv -> Benchmarkable -> IO Summary
measureUntil menv@MEnv{meConfig=Config{..}} b
  | is_once = fmap (measToSummary . fst) (measure menv 1 b)
  | otherwise = init_and_go
  where
    is_once = isInfinite cfgRelStDev && 0 < cfgRelStDev

    init_and_go = do
      performGC
      start_time <- getTimePicoSecs cfgTimeMode
      (m0, acc) <- initializeAcc menv b
      go start_time m0 acc

    go start_time m1 !acc = do
      (m2, end_time) <- measure menv (acNumRepeats acc) b

      let est@(Estimate measN stdevN) = predictPerturbed m1 m2
          meanN = measTime measN
          end_time' = end_time + measTime m2 * 2 + (30 * oneMillisecond)
          is_timeout_soon = timeoutSoon cfgTimeout start_time end_time'
          target_stdev = truncate (cfgRelStDev * word64ToDouble meanN)
          is_stdev_in_target_range = stdevN < target_stdev

      warnOnTooLongBenchmark cfgTimeout start_time end_time

      if is_stdev_in_target_range || is_timeout_soon
        then pure $ summarize acc m2 est
        else go start_time m2 (updateForNextRun acc stdevN m2)

measToSummary :: Measurement -> Summary
measToSummary m@(Measurement t _ _ _) = Summary est mean stdev med rsq
  where
    est = Estimate m 0
    mean = toRanged t
    stdev = toRanged 0
    med = toRanged t
    rsq = toRanged 1
{-# INLINABLE measToSummary #-}

timeoutSoon :: Timeout -> Word64 -> Word64 -> Bool
timeoutSoon tout t_start t_end_of_next_run =
  case tout of
    NoTimeout      -> False
    Timeout micros -> 1000000 * micros <= (t_end_of_next_run - t_start)
{-# INLINABLE timeoutSoon #-}

warnOnTooLongBenchmark :: Timeout -> Word64 -> Word64 -> IO ()
warnOnTooLongBenchmark tout t_start t_now =
  case tout of
    NoTimeout | t_now - t_start > 100 * 1000000000000 ->
      hPutStrLn stderr $
                "\n" ++
                "This benchmark takes more than 100 seconds.\n" ++
                "Conosider setting --time-limit, if this is\n" ++
                "unexpected (or to silence this warning)."
    _ -> pure ()
{-# INLINABLE warnOnTooLongBenchmark #-}


-- ------------------------------------------------------------------------
-- Accumulator for measureUntil
-- ------------------------------------------------------------------------

data Acc = Acc
  { acNumRepeats :: {-# UNPACK #-} !Word64
  , acMeans      :: {-# UNPACK #-} ![Word64]
  , acStdevs     :: {-# UNPACK #-} ![Word64]
  }

initializeAcc :: MEnv -> Benchmarkable -> IO (Measurement, Acc)
initializeAcc menv b = do
  debugStr menv "*** Starting initialization\n"
  go 0 1
  where
    threshold = precision * 30
    {-# INLINE threshold #-}

    go :: Word64 -> Word64 -> IO (Measurement, Acc)
    go !i !n = do
      meas@(Measurement t _ _ _) <- fmap fst (measure menv n b)
      -- Discarding Measurement data when the total duration is
      -- shorter than threshold. Too short measurement is considered
      -- imprecise and unreliable. When the total duration is less
      -- than 2 * threshold and iteration is less than twice,
      -- considering that the warming up is not enough, running the
      -- measurement again.
      if t < threshold || (i < 2 && t < threshold * 2)
        then go (i + 1) (n * 2)
        else do
          debugStr menv "*** Initialization done\n"
          pure ( meas
               , Acc { acNumRepeats = 2 * n
                     , acMeans = (t `quot` n) !: []
                     , acStdevs = []
                     })
    {-# INLINE go #-}

summarize :: Acc -> Measurement -> Estimate -> Summary
summarize ac m2 (Estimate measN stdevN) =
  let Measurement meanN allocN copiedN maxMemN = measN
      scale = (`quot` (acNumRepeats ac `quot` 2))
      mean_scaled = scale meanN
      meas = Measurement mean_scaled (scale allocN) (scale copiedN) maxMemN

      -- The list 'acMeans ac' contains normalized measurement values.
      mean_m2 = measTime m2 `quot` acNumRepeats ac
      means = reverse (mean_m2 !: acMeans ac)
      (mean_min, mean_max) = minMax means
      mean_ranged = Ranged mean_min mean_scaled mean_max

      stdev_all = computeSSD means
      stdev_all_w64 = ceiling stdev_all
      stdev_scaled = scale stdevN
      sds = stdev_all_w64 !: stdev_scaled !: acStdevs ac
      (stdev_min, stdev_max) = minMax sds
      stdev_ranged = Ranged stdev_min stdev_all_w64 stdev_max

      -- In the list of normalized means, each value is computed from
      -- twice the number of repeats than the previous value.
      niters = iterate (* 2) 1
      xs_and_ys = zipWith (\x y -> (x, x * word64ToDouble y)) niters means
      (ols, rsq) = regress stdev_all xs_and_ys

  in  Summary { smEstimate = Estimate meas stdev_scaled
              , smOLS = ols
              , smR2 = rsq
              , smStdev = stdev_ranged
              , smMean = mean_ranged
              }
{-# INLINE summarize #-}

updateForNextRun :: Acc -> Word64 -> Measurement -> Acc
updateForNextRun ac sd m =
  ac { acNumRepeats = acNumRepeats ac * 2
     , acMeans = (measTime m `quot` acNumRepeats ac) !: acMeans ac
     , acStdevs = (sd `quot` acNumRepeats ac) !: acStdevs ac
     }
{-# INLINE updateForNextRun #-}

minMax :: [Word64] -> (Word64, Word64)
minMax = foldr f (maxBound, minBound)
  where
    f x (amin, amax) = (min amin x, max amax x)
{-# INLINABLE minMax #-}

-- Apply bang pattern to the first argument, then (:).
(!:) :: a -> [a] -> [a]
!x !: xs = x : xs
{-# INLINE (!:) #-}

infixr 5 !:


-- ------------------------------------------------------------------------
-- Ordered values
-- ------------------------------------------------------------------------

-- | A value in a range.
data Ranged a = Ranged {irLo :: !a, irMid :: !a, irHi :: !a}

instance Functor Ranged where
  fmap f (Ranged l m h) = Ranged (f l) (f m) (f h)
  {-# INLINE fmap #-}

-- | Ranged value with identical lo, high, and the body values.
toRanged :: a -> Ranged a
toRanged x = Ranged x x x
{-# INLINE toRanged #-}


-- ------------------------------------------------------------------------
-- Analysis
-- ------------------------------------------------------------------------

-- | Simple linear regression with ordinary least square.
regress :: Double -> [(Double, Double)] -> (OLS, R2)
regress ssd xs_and_ys = (ols, r2)
  where
    ols = fmap ceiling (ci95 sample_size ssd a)
    r2 = Ranged (fr2 (-1)) (fr2 0) (fr2 1)

    -- means and sample size
    (x_mean, y_mean, sample_size) = (sum_x / n, sum_y / n, len)
      where
        n = fromIntegral len
        (sum_x, sum_y, len) = foldl' f z xs_and_ys
        f (!sx, !sy, !sl) (x,y) = (sx + x, sy + y, sl + 1)
        z = (0, 0, 0 :: Int)

    -- ols
    nume = sum [(x - x_mean) * (y - y_mean) | (x,y) <- xs_and_ys]
    deno = sum [square (x - x_mean) | (x,_) <- xs_and_ys]
    a = nume / deno
    b = y_mean - (a * x_mean)

    -- R^2
    p k x = a * x + k * b
    ssr k = sum [square (y - p k x) | (x,y) <- xs_and_ys]
    sst = sum [square (y - y_mean) | (_,y) <- xs_and_ys]
    fr2 k = 1 - (ssr k / sst)
{-# INLINABLE regress #-}

-- | Compute sample standard deviation.
computeSSD :: [Word64] -> Double
computeSSD normalized_means =
  let n = fromIntegral (length normalized_means)
      ys = map word64ToDouble normalized_means
      y_mean = sum ys / n
  in  sqrt (sum [square (y - y_mean) | y <- ys] / (n - 1))
{-# INLINABLE computeSSD #-}

-- | Compute 95% confidence interval from sample standard deviation.
ci95 :: Int -- ^ Number of samples.
     -> Double -- ^ Sample standard deviation.
     -> Double -- ^ The point value.
     -> Ranged Double
ci95 n ssd x =
  let se = ssd / (sqrt (fromIntegral n))
      w = se * 1.96
  in  Ranged (x - w) x (x + w)
{-# INLINABLE ci95 #-}


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
