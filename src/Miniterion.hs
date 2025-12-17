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

    -- * Miniterion specific
    -- $miniterion_specific
  , defaultMainWith
  , defaultConfig
  , Config(..)
  , UseColor(..)
  , MatchMode(..)
  , TimeMode(..)
  , Timeout(..)

#ifdef DEV
    -- * For development, exposed for testing
  , showPicos5
  , showBytes
  , mu

  , MEnv
  , getDefaultMEnv

  , Doc
  , docToString
#endif
  ) where

-- base
import           Control.Exception      (Exception (..), SomeException (..),
                                         evaluate, finally, handle, throw,
                                         throwIO)
import           Control.Monad          (guard, unless, void, when)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Char              (toLower)
import           Data.Foldable          (find, foldlM)
import           Data.Int               (Int64)
import           Data.List              (intercalate, isPrefixOf, nub, sort,
                                         stripPrefix, tails, unfoldr)
import           Data.String            (IsString (..))
import           Data.Word              (Word64)
import           GHC.Clock              (getMonotonicTime)
import           GHC.Stats              (RTSStats (..), getRTSStats,
                                         getRTSStatsEnabled)
import           System.Console.GetOpt  (ArgDescr (..), ArgOrder (..),
                                         OptDescr (..), getOpt', usageInfo)
import           System.CPUTime         (cpuTimePrecision, getCPUTime)
import           System.Environment     (getArgs, getProgName)
import           System.Exit            (die, exitFailure)
import           System.IO              (BufferMode (..), Handle, IOMode (..),
                                         hFlush, hGetLine, hIsEOF,
                                         hIsTerminalDevice, hPutStr, hPutStrLn,
                                         hSetBuffering, stderr, stdout,
                                         withFile)
import           System.Mem             (performGC, performMinorGC)
import           System.Timeout         (timeout)
import           Text.Printf            (printf)
import           Text.Read              (readMaybe)

#if !MIN_VERSION_base(4,20,0)
import           Data.Foldable          (foldl')
#endif

#if MIN_VERSION_base(4,15,0)
import           GHC.Exts               (SPEC (..))
#else
import           GHC.Exts               (SpecConstrAnnotation (..))
#endif

#if MIN_VERSION_base(4,5,0)
import           GHC.IO.Encoding        (getLocaleEncoding, setLocaleEncoding,
                                         textEncodingName, utf8)
#endif

#if defined(mingw32_HOST_OS)
import           Control.Exception      (bracket)
import           Data.Word              (Word32)
#endif

-- deepseq
import           Control.DeepSeq        (NFData, force, rnf)

#if IS_PACKAGE_BUILD
-- Internal
import           Paths_miniterion       (getDataFileName)
#endif


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
defaultMain = defaultMainWith defaultConfig

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
nf = fmap toBenchmarkable . nf' rnf

-- | 'whnf' @f@ @x@ measures time to compute a weak head normal form
-- of an application of @f@ to @x@.  This does not include time to
-- evaluate @f@ or @x@ themselves.  Ideally @x@ should be a primitive
-- data type like 'Data.Int.Int'.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:whnf whnf>@.
whnf :: (a -> b) -> a -> Benchmarkable
whnf = fmap toBenchmarkable . whnf'

-- | 'nfIO' @x@ measures time to evaluate side-effects of @x@ and
-- compute its normal form (by means of 'force', not
-- 'Control.DeepSeq.rnf').
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:nfIO nfIO>@.
nfIO :: NFData a => IO a -> Benchmarkable
nfIO = toBenchmarkable . ioToBench rnf

-- | 'whnfIO' @x@ measures time to evaluate side-effects of @x@ and
-- compute its weak head normal form.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:whnfIO whnfIO>@.
whnfIO :: IO a -> Benchmarkable
whnfIO = toBenchmarkable . ioToBench id

-- | 'nfAppIO' @f@ @x@ measures time to evaluate side-effects of an
-- application of @f@ to @x@ and compute its normal form (by means of
-- 'force', not 'Control.DeepSeq.rnf').  This does not include time to
-- evaluate @f@ or @x@ themselves.  Ideally @x@ should be a primitive
-- data type like 'Data.Int.Int'.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:nfAppIO nfAppIO>@.
nfAppIO :: NFData b => (a -> IO b) -> a -> Benchmarkable
nfAppIO = fmap toBenchmarkable . ioFuncToBench rnf

-- | 'whnfAppIO' @f@ @x@ measures time to evaluate side-effects of an
-- application of @f@ to @x@ and compute its weak head normal form.
-- This does not include time to evaluate @f@ or @x@ themselves.
-- Ideally @x@ should be a primitive data type like 'Data.Int.Int'.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:whnfAppIO whnfAppIO>@.
whnfAppIO :: (a -> IO b) -> a -> Benchmarkable
whnfAppIO = fmap toBenchmarkable . ioFuncToBench id

-- | Run a benchmark interactively, providing an interface compatible with
-- @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:benchmark benchmark>@.
benchmark :: Benchmarkable -> IO ()
benchmark = void . runBenchmark defaultMEnv . bench "..."

-- $miniterion_specific
--
-- Functions and data types for Miniterion specific configuration.
--
-- The data type t'Config' has the same name as
-- @Criterion.Types.<https://hackage.haskell.org/package/criterion/docs/Criterion-Types.html#t:Config Config>@,
-- but the implementation is different.

-- | An entry point that can be used as a @main@ function, with
-- configurable defaults.
defaultMainWith :: Config -> [Benchmark] -> IO ()
defaultMainWith cfg bs = do
  let act = defaultMainWith' cfg bs
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
{-# INLINABLE defaultMainWith #-}

-- | Default configuration used for running benchmarks.
defaultConfig :: Config
defaultConfig = Config
  { cfgUseColor = Auto
  , cfgMatch = Prefix
  , cfgTimeMode = CpuTime
  , cfgTimeout = NoTimeout
  , cfgBaselinePath = Nothing
  , cfgCsvPath = Nothing
  , cfgJsonPath = Nothing
  , cfgReportPath = Nothing
  , cfgFailIfFaster = 1.0 / 0.0
  , cfgFailIfSlower = 1.0 / 0.0
  , cfgRelStDev = 0.05
  , cfgVerbosity = 1
  }

-- | Data type to hold configuration information.
data Config = Config
  { cfgUseColor     :: !UseColor
    -- ^ When to use colored outputs.
  , cfgMatch        :: !MatchMode
    -- ^ Which mode to use for benchmark name pattern match.
  , cfgTimeMode     :: !TimeMode
    -- ^ Time mode for measuring benchmarks.
  , cfgTimeout      :: !Timeout
    -- ^ Timeout duration in seconds.
  , cfgBaselinePath :: Maybe FilePath
    -- ^ Path to a file containing baseline data, usually a CSV file
    -- made with @--csv@ option in advance.
  , cfgCsvPath      :: Maybe FilePath
    -- ^ Path to a file for writing results in CSV format.
  , cfgJsonPath     :: Maybe FilePath
    -- ^ Path to a file for writing JSON summary.
  , cfgReportPath   :: Maybe FilePath
    -- ^ Path to a file for writing HTML report.
  , cfgFailIfFaster :: Double
    -- ^ Upper bound of acceptable speed up.
  , cfgFailIfSlower :: Double
    -- ^ Upper bound of acceptable slow down.
  , cfgRelStDev     :: !Double
    -- ^ Relative standard deviation for terminating benchmarks.
  , cfgVerbosity    :: !Int
    -- ^ Verbosity level.
  }

-- | When to use colored output.
data UseColor
  = Always -- ^ Always use color.
  | Auto   -- ^ Use color if the output is a terminal device.
  | Never  -- ^ Don't use color.

-- | Data type to express how to match benchmark names.
data MatchMode
  = Pattern  -- ^ Substring match
  | Prefix   -- ^ Prefix match
  | IPattern -- ^ Case insensitive prefix match
  | Glob     -- ^ Glob pattern match

-- | Data type to select the function to get times.
data TimeMode
  = CpuTime  -- ^ Measure CPU time.
  | WallTime -- ^ Measure wall-clock time.

-- | Express duration for timeout.
data Timeout
  = Timeout !Word64
  -- ^ Duration in microseconds (e.g., 2000000 for 2 seconds).
  | NoTimeout
  -- ^ Run without timeout.


-- ------------------------------------------------------------------------
-- Main
-- ------------------------------------------------------------------------

-- | Mode to execute the main function.
data RunMode
  = Help           -- ^ Show help message.
  | Version        -- ^ Show version info.
  | DoList         -- ^ Show benchmark names.
  | DoIter !Word64 -- ^ Run benchmarks for given repeat count, don't analyse.
  | DoBench        -- ^ Run benchmarks.

defaultMainWith' :: Config -> [Benchmark] -> IO ()
defaultMainWith' cfg0 bs = handleMiniterionException $ do
  args <- getArgs
  let (!opts, !pats, invalids, errs) = getOpt' Permute options args
      O !cfg1 !run_mode = foldl' (flip id) (O cfg0 DoBench) opts
  default_menv <- getDefaultMEnv cfg1
  let menv0 = default_menv {mePatterns = pats}
      root_bs = bgroup "" bs
      do_iter n = iterBenchmark n menv0 root_bs >>= summariseResults
      do_bench !menv = runBenchmark menv root_bs
  case run_mode of
    Help     -> showHelp menv0
    _         | not (null errs)     -> errorOptions errs
              | not (null invalids) -> invalidOptions invalids
    Version  -> putStrLn builtWithMiniterion
    DoList   -> showNames menv0 root_bs
    DoIter n -> do_iter n
    DoBench  -> withHandles menv0 do_bench >>= summariseResults

withHandles :: MEnv -> (MEnv -> IO a) -> IO a
withHandles menv0 act =
  withCsvSettings menv0 $ \menv1 -> withJSONSettings menv1 act
{-# INLINE withHandles #-}

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
#define VERSION_miniterion "unknown version"
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
  { meConfig          :: Config
    -- ^ Configuration of this environment.
  , mePatterns        :: ![String]
    -- ^ Patterns to filter running benchmarks
  , meCsvHandle       :: !(Maybe Handle)
    -- ^ File handle to write benchmark result in CSV format.
  , meJsonHandle      :: !(Maybe Handle)
    -- ^ File handle to write benchmark result of JSON summary.
  , meBaselineSet     :: !Baseline
    -- ^ Set containing baseline information, made from the file
    -- specified by 'cfgBaselinePath'.
  , meUseColor        :: !Bool
    -- ^ 'True' if using colored output.
  , meSupportsUnicode :: !Bool
    -- ^ 'True' if unicode is supported.
  , meHasGCStats      :: !Bool
    -- ^ 'True' if GC statistics are available.
  }

-- | The default environment.
defaultMEnv :: MEnv
defaultMEnv = MEnv
  { meCsvHandle = Nothing
  , meJsonHandle = Nothing
  , meBaselineSet = mempty
  , mePatterns = []
  , meConfig = defaultConfig
  , meUseColor = False
  , meSupportsUnicode = False
  , meHasGCStats = False
  }
{-# INLINABLE defaultMEnv #-}

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
{-# INLINABLE getDefaultMEnv #-}

-- | A monad to run 'IO' actions with t'MEnv', basically same as
-- @ReaderT MEnv IO@.
newtype Miniterion a = Miniterion {runMiniterion :: MEnv -> IO a}

instance Functor Miniterion where
  fmap f (Miniterion r) = Miniterion (fmap f . r)
  {-# INLINE fmap #-}

instance Applicative Miniterion where
  pure x = Miniterion (const (pure x))
  {-# INLINE pure #-}

  Miniterion f <*> Miniterion m = Miniterion (\e -> f e <*> m e)
  {-# INLINE (<*>) #-}

instance Monad Miniterion where
  Miniterion r >>= k = Miniterion (\e -> r e >>= \a -> runMiniterion (k a) e)
  {-# INLINE (>>=) #-}

instance MonadIO Miniterion where
  liftIO io = Miniterion (const io)
  {-# INLINE liftIO #-}

getMEnv :: Miniterion MEnv
getMEnv = Miniterion pure
{-# INLINE getMEnv #-}


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
  let (!num_result, !num_failed) = foldl' f z rs
      z :: (Int, Int)
      z = (0, 0)
      f (!done, !fl) = \case
        Done -> (done + 1, fl)
        _    -> (done + 1, fl + 1)
      bs | 1 < num_result = "benchmarks"
         | otherwise = "benchmark" :: String
      pr (name, why) = putStrLn ("  - " ++ name ++ " (" ++ why ++ ")")
  when (0 < num_failed) $ do
    printf "\n%d out of %d %s failed:\n" num_failed num_result bs
    mapM_ (mapM_ pr . failedNameAndReason) (reverse rs)
    exitFailure
{-# INLINABLE summariseResults #-}

isTooFast, isTooSlow :: Result -> Bool

isTooFast TooFast {} = True
isTooFast _          = False
{-# INLINE isTooFast #-}

isTooSlow TooSlow {} = True
isTooSlow _          = False
{-# INLINE isTooSlow #-}

failedNameAndReason :: Result -> Maybe (String, String)
failedNameAndReason = \case
  Done          -> Nothing
  TooSlow name  -> Just (name, "too slow")
  TooFast name  -> Just (name, "too fast")
  TimedOut name -> Just (name, "timed out")
{-# INLINE failedNameAndReason #-}


-- ------------------------------------------------------------------------
-- Running benchmarks
-- ------------------------------------------------------------------------

runBenchmark :: MEnv -> Benchmark -> IO [Result]
runBenchmark = runBenchmarkWith runBenchmarkable

iterBenchmark :: Word64 -> MEnv -> Benchmark -> IO [Result]
iterBenchmark n = runBenchmarkWith (iterBenchmarkable n)

runBenchmarkWith :: (Int -> String -> Benchmarkable -> Miniterion a)
                 -> MEnv -> Benchmark -> IO [a]
runBenchmarkWith !run menv b = fst <$> runMiniterion (go [] 0 b) menv
  where
    -- Benchmarks are always wrapped with the root group in
    -- defaultMainWith', selecting the benchmarks to run in Bgroup's
    -- case.
    go !parents !i bnch = case bnch of
      Bench name act -> do
        r <- run i (pathToName parents name) act
        pure ([r], i+1)
      Bgroup name bs -> do
        let !parents' = consNonNull name parents
            f (!rs, !j) !bnch'
              | any (isMatched menv) (benchNames parents' bnch') = do
                  (!rs', j') <- go parents' j bnch'
                  pure (rs' ++ rs, j')
              | otherwise = pure (rs, j)
        foldlM f ([],i) bs
      Environment !alloc !clean f -> liftIO $ do
        e <- alloc >>= \e -> evaluate (rnf e) >> pure e
        runMiniterion (go parents i (f e)) menv `finally` clean e

runBenchmarkable :: Int -> String -> Benchmarkable -> Miniterion Result
runBenchmarkable idx fullname b = do
  menv@MEnv{meConfig=Config{..}, ..} <- getMEnv

  infoBenchname fullname
  debugStr "\n"
  liftIO $ hFlush stdout
  mb_sum <- withTimeout cfgTimeout (liftIO $ measureUntil menv b)

  let (!result, mb_cmp) = case mb_sum of
        Nothing -> (TimedOut fullname, Nothing)
        Just (Summary {smEstimate=est}) ->
          case compareVsBaseline meBaselineSet fullname est of
            Nothing  -> (Done, Nothing)
            Just cmp ->
              let is_acceptable
                    | 1 + cfgFailIfSlower <= cmp = TooSlow fullname
                    | cmp <= 1 - cfgFailIfFaster = TooFast fullname
                    | otherwise                  = Done
              in  (is_acceptable, Just cmp)

  infoStr (formatResult result mb_sum mb_cmp)
  liftIO $ case mb_sum of
    Nothing -> mapM_ (putFailedJSON idx fullname) meJsonHandle
    Just summary -> do
      mapM_ (putCsvLine meHasGCStats fullname summary) meCsvHandle
      mapM_ (putSummaryJSON idx fullname summary) meJsonHandle

  pure result

iterBenchmarkable :: Word64 -> Int -> String -> Benchmarkable
                  -> Miniterion Result
iterBenchmarkable n _idx fullname b = do
  MEnv{meConfig=Config{..}} <- getMEnv

  infoBenchname fullname
  liftIO $ hFlush stdout
  mb_unit <- withTimeout cfgTimeout (liftIO $ runLoop b n id)

  case mb_unit of
    Just () -> infoStr "\n" >> pure Done
    _ -> do
      let result = TimedOut fullname
      infoStr (formatResult result Nothing Nothing)
      pure result

infoBenchname :: String -> Miniterion ()
infoBenchname name =
  infoStr (white "benchmarking " <> boldCyan (fromString name) <> " ")
{-# INLINE infoBenchname #-}

withTimeout :: Timeout -> Miniterion a -> Miniterion (Maybe a)
withTimeout tout m@(Miniterion r) = case tout of
  Timeout micro -> Miniterion (timeout (fromIntegral micro) . r)
  NoTimeout     -> fmap Just m

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

infoStr, _warnStr, debugStr :: Doc -> Miniterion ()
infoStr = Miniterion . flip infoStr'
_warnStr = Miniterion . flip _warnStr'
debugStr = Miniterion . flip debugStr'

infoStr', _warnStr', debugStr' :: MEnv -> Doc -> IO ()
infoStr' = putDocWith 1
_warnStr' = putDocWith 2
debugStr' = putDocWith 3

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
  white "time                 " <> showPicos5 (irMid smOLS)   <> "   " <>
  show_minmax (irLo smOLS) (irHi smOLS) <>
  maybe "" (formatSlowDown res) mb_cmp <> "\n" <>
  --
        "                     " <> rsq_str id (irMid smR2) <> "   " <>
  show_minmax_rsq <> "\n" <>
  --
  white "mean                 " <> showPicos5 (irMid smMean) <> "   " <>
  show_minmax (irLo smMean) (irHi smMean) <> "\n" <>
  --
  white "std dev              " <> showPicos5 (2 * irMid smStdev) <> "   " <>
  show_minmax (2 * irLo smStdev) (2 * irHi smStdev) <>
  --
  formatOutlierVariance smOutlierVar <>
  --
  formatGC m <> "\n\n"
  where
    Summary {smEstimate=Estimate m _, ..} = summary
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
      white "(" <> rsq_str white (irLo smR2) <> white " .. " <>
      rsq_str white (irHi smR2) <> white ")"

formatSlowDown :: Result -> Double -> Doc
formatSlowDown result ratio = case percents `compare` 0 of
  LT -> in_yellow isTooFast $ printf " (%2i%% less than baseline)" (-percents)
  EQ -> white                         "       (same as baseline)"
  GT -> in_yellow isTooSlow $ printf " (%2i%% more than baseline)" percents
  where
    percents :: Int64
    percents = truncate ((ratio - 1) * 100)
    in_yellow test = (if test result then yellow else white) . fromString

formatOutlierVariance :: OutlierVariance -> Doc
formatOutlierVariance (OutlierVariance !oe _ frac) = case oe of
  Moderate -> show_oe
  Severe   -> show_oe
  _        -> ""
  where
    show_oe =
      white "\nvariance introduced by outliers: " <>
      stringToDoc (printf "%2d%% " (round (frac * 100) :: Int)) <>
      white ("(" <> stringToDoc (map toLower (show oe)) <> "ly inflated)")

formatGC :: Measurement -> Doc
formatGC (Measurement {measAllocs=a, measCopied=c, measMaxMem=p}) =
  Doc $ \ !e ->
  if meHasGCStats e then
    let sb !b = fromString $! showBytes b
    in  docToString e $ "\n" <>
        white "        alloc  copied    peak" <> "\n" <>
        white "gc     " <> sb a <> "  " <> sb c <> "  " <> sb p
  else
    ""

formatMeasurement :: Measurement -> Doc
formatMeasurement (Measurement n t a c m) =
  fromString (show n) <>
  (if n == 1 then " iteration gives " else " iteragions give ") <>
  showPicos5 (t `quot` n) <> fromString (printf " (%d/%d)" t n) <>
  Doc (\ !menv ->
         if meHasGCStats menv then
           printf " alloc: %d copied: %d max: %d" a c m
         else
           "") <>
  "\n"

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


-- ------------------------------------------------------------------------
-- Matching benchmark names
-- ------------------------------------------------------------------------

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

putCsvLine :: Bool -> String -> Summary -> Handle -> IO ()
putCsvLine has_gc name summary hdl =
  hPutStrLn hdl (encodeCsv name ++ "," ++ csvSummary has_gc summary)

csvSummary :: Bool -> Summary -> String
csvSummary has_gc (Summary {smEstimate=Estimate m _, ..})
  | has_gc    = time ++ "," ++ gc
  | otherwise = time
  where
    time =
      show (measTime m) ++ "," ++
      show (irLo smMean) ++ "," ++ show (irHi smMean) ++ "," ++
      show (2 * irMid smStdev) ++ "," ++
      show (2 * irLo smStdev) ++ "," ++ show (2 * irHi smStdev)
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
compareVsBaseline []       _     _                 = Nothing
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
-- JSON
-- ------------------------------------------------------------------------

-- The JSON report made by Miniterion differs from the one made by
-- Criterion. Some of the keys have the same names but the meaning
-- differs (e.g., confIntLDX, confIntUDX), some of the values are
-- missing (e.g., 'y' in regCoeffs, Measurement fields). Hope that the
-- use of the same names will help reusing the JSON parser between
-- Miniterion and Criterion.

withJSONSettings :: MEnv -> (MEnv -> IO a) -> IO a
withJSONSettings menv@MEnv{meConfig=Config{..}} act =
  -- When HTML report is specified without JSON output, writing JSON
  -- data to a temporary file.
  case cfgJsonPath of
    Just json -> do
      r <- withJSONFile json menv act
      mapM_ (writeReport json) cfgReportPath
      pure r
    Nothing | Just html <- cfgReportPath -> do
      r <- withJSONFile tmpJSONFile menv act
      writeReport tmpJSONFile html
      pure r
    _ -> act menv {meJsonHandle = Nothing}
{-# INLINABLE withJSONSettings #-}

-- | Temporary file to write JSON data for generating report when the
-- JSON path was not specified.
tmpJSONFile :: FilePath
tmpJSONFile = ".miniterion-tmp.json"
{-# INLINE tmpJSONFile #-}

withJSONFile :: FilePath -> MEnv -> (MEnv -> IO a) -> IO a
withJSONFile !file !menv !act =
  withFile file WriteMode $ \hdl -> do
    hSetBuffering hdl NoBuffering
    hPutStr hdl $ "[\"miniterion\",\"" ++ VERSION_miniterion ++ "\",["
    act menv {meJsonHandle = Just hdl} `finally` hPutStr hdl "]]"
{-# INLINABLE withJSONFile #-}

putFailedJSON :: Int -> String -> Handle -> IO ()
putFailedJSON !idx name hdl = do
  when (idx /= 0) $ hPutStr hdl ","
  hPutStr hdl $
    "{\"reportAnalysis\":" ++ analysis ++
    ",\"reportKDEs\":" ++ kdes ++
    ",\"reportKeys\":" ++ keys ++
    ",\"reportMeasured\":" ++ measured ++
    ",\"reportName\":" ++ escapeJSON name ++
    ",\"reportNumber\":" ++ show idx ++
    "}"
  where
    analysis =
      "{\"anMean\":" ++ est0 ++
      "," ++ anOutlierVar ++
      ",\"anRegress\":[{\"regCoeffs\":{\"iters\":" ++ est0 ++
      "},\"regRSquare\":" ++ est0 ++
      "}],\"anStdDev\":" ++ est0 ++
      "}"
    anOutlierVar =
      "\"anOutlierVar\":" ++
      "{\"ovDesc\":\"no\"" ++
      ",\"ovEffect\":\"Unaffected\"" ++
      ",\"ovFraction\":0}"
    est0 = "{\"estError\":{\"confIntLDX\":0,\"confIntUDX\":0},\"estPoint\":0}"
    kdes = "[{\"kdePDF\":[],\"kdeType\":\"time\",\"kdeValues\":[]}]"
    keys = "[]"
    measured = "[]"
{-# INLINABLE putFailedJSON #-}

putSummaryJSON :: Int -> String -> Summary -> Handle -> IO ()
putSummaryJSON !idx name Summary{..} hdl = do
  when (idx /= 0) $ hPutStr hdl ","
  hPutStr hdl $
    "{\"reportAnalysis\":" ++ analysis ++
    ",\"reportKDEs\":" ++ kdes ++
    ",\"reportKeys\":" ++ keys ++
    ",\"reportMeasured\":" ++ measured ++
    ",\"reportName\":" ++ escapeJSON name ++
    ",\"reportNumber\":" ++ show idx ++
    "}"
  where
    analysis =
      "{\"anMean\":" ++ est (fmap picoToSecs smMean) ++
      ",\"anOutlierVar\":" ++ variance smOutlierVar ++
      ",\"anRegress\":[{\"regCoeffs\":{\"iters\":" ++
      est (fmap picoToSecs smOLS) ++
      "},\"regRSquare\":" ++ est smR2 ++
      "}],\"anStdDev\":" ++ est (fmap picoToSecs smStdev) ++
      "}"
      where
        est (Ranged lo mid hi) =
          "{\"estError\":{\"confIntLDX\":" ++ show (mid - lo) ++
          ",\"confIntUDX\":" ++ show (hi - mid) ++
          "},\"estPoint\":" ++ show mid ++
          "}"
        variance OutlierVariance{..} =
          "{\"ovDesc\":\"" ++ ovDesc ++ "\"" ++
          ",\"ovEffect\":\"" ++ show ovEffect ++ "\"" ++
          ",\"ovFraction\":" ++ show ovFraction ++
          "}"
    kdes =
      "[{\"kdePDF\":" ++ show (kdPDF smKDEs) ++
      ",\"kdeType\":\"time\"" ++
      ",\"kdeValues\":" ++ show (kdValues smKDEs) ++
      "}]"
    keys =
      "[\"time\",\"iters\",\"allocated\",\"peakMbAllocated\",\"bytesCopied\"]"
    measured =
      "[" ++ intercalate "," (map meas_to_arr smMeasured) ++ "]"
      where
        meas_to_arr (Measurement n t a c m) =
          "[" ++ show (picoToSecs t) ++ "," ++ show n ++ "," ++
          (if a == 0 then "null" else show a) ++ "," ++
          (if m == 0 then "null" else show (m `quot` 1000000)) ++ "," ++
          (if c == 0 then "null" else show c) ++ "]"
{-# INLINABLE putSummaryJSON #-}

-- Simplified variant of Criterion.Report.escapeJSON for String
-- instead of Text. Does not escape plus character (@+@) and NULL
-- (@\0@).
escapeJSON :: String -> String
escapeJSON xs = '"' : foldr f ['"'] xs
  where
    f '\n'     = ("\\n" ++)
    f '\\'     = ("\\\\" ++)
    f '"'      = ("\\\"" ++)
    f '<'      = ("\\u003c" ++)
    f '>'      = ("\\u003e" ++)
    f '&'      = ("\\u0026" ++)
    f '\x2028' = ("\\u2028" ++) -- line separator
    f '\x2029' = ("\\u2029" ++) -- paragraph separator
    f c        = (c:)
{-# INLINABLE escapeJSON #-}


-- ------------------------------------------------------------------------
-- HTML report
-- ------------------------------------------------------------------------

-- | Write HTML report from JSON data.
writeReport :: FilePath -- ^ Path of the input JSON file
            -> FilePath -- ^ Path of the HTML output file
            -> IO ()
#if IS_PACKAGE_BUILD
writeReport infile outfile = do
  template_path <- getDataFileName data_template_html
  withFile template_path ReadMode $ \ihdl ->
    withFile outfile WriteMode $ \ohdl ->
      let go = do
            is_eof <- hIsEOF ihdl
            unless is_eof $ do
              line <- hGetLine ihdl
              if trim line == "{{{json}}}"
                then hPutStrLn ohdl . second_element =<< readFile infile
                else hPutStrLn ohdl line
              go
      in  go
  where
    -- Path to the template HTML file for generating report, contains
    -- OS specific path separator. Could be done with (</>) defined in
    -- the 'filepath' package, but using CPP at the moment (using
    -- backslash on Windows, or slash otherwise).
#if defined(mingw32_HOST_OS)
    data_template_html = "data\\template.html"
#else
    data_template_html = "data/template.html"
#endif
    -- Simple white space removal to find embedded JSON mark.
    trim = takeWhile (/= ' ') . dropWhile (== ' ')

    -- Removing pre and post characters to get the second element of
    -- the JSON array. The number of characters before the beginning
    -- of the second element is known in advance
    -- (@["miniterion","w.x.y.z",@, 24 characters). The last closing
    -- bracket is removed with `init'.
    second_element = init . drop 24
#else
writeReport _ _ = do
  me <- getProgName
  putStrLn ("*** Writing HTML report is NOT supported in " <> me)
  putStrLn (me <> " was built with non-packaged version of Miniterion")
#endif


-- ------------------------------------------------------------------------
-- Command line options
-- ------------------------------------------------------------------------

data Opts = O Config RunMode

options :: [OptDescr (Opts -> Opts)]
options =
  [ Option ['h'] ["help"]
    (NoArg (\(O c _) -> O c Help))
    "Show this help text"

  , Option ['L'] ["time-limit"]
    (ReqArg (\str (O c m) -> case readMaybe str :: Maybe Double of
                Just n -> O (c {cfgTimeout = Timeout (floor (1e6 * n))}) m
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
       in  ReqArg (\str (O c m) -> case find (match str) whens of
                      Just (_, uc) -> O (c {cfgUseColor = uc}) m
                      _            -> throw (InvalidArgument "color" str))
           "WHEN")
      (unlines
       ["When to use colors, \"auto\", \"always\", or \"never\""
       ,"(default: auto)"])

  , Option [] ["baseline"]
    (ReqArg (\str (O c m) -> O (c {cfgBaselinePath = Just str}) m)
    "FILE")
    "File to read CSV summary from as baseline"

  , Option [] ["csv"]
    (ReqArg (\str (O c m) -> O (c {cfgCsvPath = Just str}) m)
     "FILE")
    "File to write CSV summary to"

  , Option [] ["json"]
    (ReqArg (\str (O c m) -> O (c {cfgJsonPath = Just str}) m)
    "FILE")
    "File to write JSON summary to"

  , Option ['o'] ["output"]
    (ReqArg (\str (O c m) -> O (c {cfgReportPath = Just str}) m)
    "FILE")
    "File to write report to"

  , Option [] ["fail-if-faster"]
    (ReqArg (\str (O c m) -> case parsePositivePercents str of
                Just x -> O (c {cfgFailIfFaster = x}) m
                _      -> throw (InvalidArgument "fail-if-faster" str))
      "NUM")
    (unlines
     ["Upper bound acceptable speed up in percents. If a"
     ,"benchmark is unacceptable faster than baseline (see"
     ,"--baseline), it will be reported as failed"])

  , Option [] ["fail-if-slower"]
    (ReqArg (\str (O c m) -> case parsePositivePercents str of
                Just x -> O (c {cfgFailIfSlower = x}) m
                _      -> throw (InvalidArgument "fail-if-slower" str))
      "NUM")
    (unlines
     ["Upper bound acceptable slow down in percents. If a"
     ,"benchmark is unacceptable slower than baseline (see"
     ,"--baseline), it will be reported as failed"])

  , Option ['s'] ["stdev"]
    (ReqArg (\str (O c m) -> case parseNonNegativeParcents str of
                Just x -> O (c {cfgRelStDev = x}) m
                _      -> throw (InvalidArgument "stdev" str))
     "NUM")
    (unlines
     ["Target relative standard deviation of measurement"
     ,"in percents (default: 5)"])

  , Option [] ["time-mode"]
    (ReqArg (\str (O c m) -> case str of
                "cpu"  -> O (c {cfgTimeMode = CpuTime}) m
                "wall" -> O (c {cfgTimeMode = WallTime}) m
                _      -> throw (InvalidArgument "time-mode" str))
    "cpu|wall")
    (unlines
     ["Whether to measure CPU (\"cpu\") time or wall-clock"
     ,"time (\"wall\") (default: cpu)"])

  , Option ['v'] ["verbosity"]
    (ReqArg (\str (O c m) -> case readMaybe str :: Maybe Int of
                Just n | 0 <= n && n <= 3 -> O (c {cfgVerbosity = n}) m
                _ -> throw (InvalidArgument "verbosity" str))
      "INT")
     "Verbosity level (default: 1)"

  , Option ['n'] ["iters"]
    (ReqArg (\str (O c _) -> case readMaybe str :: Maybe Word64 of
                Just n -> O c (DoIter n)
                _      -> throw (InvalidArgument "iters" str))
    "INT")
    "Run benchmarks, don't analyse"

  , Option ['m'] ["match"]
    (let modes = [("glob", Glob)
                 ,("pattern", Pattern)
                 ,("prefix", Prefix)
                 ,("ipattern", IPattern)]
         match str = isPrefixOf str . fst
     in  ReqArg (\str (O c m) -> case find (match str) modes of
                    Just (_, mode) -> O (c {cfgMatch = mode}) m
                    _              -> throw (InvalidArgument "match" str))
         "MODE")
    (unlines
     ["How to match benchmark names (\"prefix\", \"glob\","
     ,"\"pattern\" (substring), or \"ipattern\")"])

  , Option ['l'] ["list"]
    (NoArg (\ (O c _) -> O c DoList))
    "List benchmarks"

  , Option [] ["version"]
    (NoArg (\ (O c _ ) -> O c Version))
    "Show version info"
  ]

parseNonNegativeParcents :: String -> Maybe Double
parseNonNegativeParcents = parsePercentsWith (>= 0)

parsePositivePercents :: String -> Maybe Double
parsePositivePercents = parsePercentsWith (> 0)

parsePercentsWith :: (Double -> Bool) -> String -> Maybe Double
parsePercentsWith test xs = do
  x <- readMaybe xs
  guard (test x)
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

getTimePicoSecs :: TimeMode -> IO Word64
getTimePicoSecs = \case
  CpuTime  -> fromInteger <$> getCPUTime
  WallTime -> round . (1e12 *) <$> getMonotonicTime

picoToSecs :: Word64 -> Double
picoToSecs pico = word64ToDouble pico / 1e12
{-# INLINE picoToSecs #-}


-- ------------------------------------------------------------------------
-- Getting GC info
-- ------------------------------------------------------------------------

getAllocsAndCopied :: Bool -> IO (Word64, Word64, Word64)
getAllocsAndCopied has_gc_stats =
#if MIN_VERSION_base(4,10,0)
  if not has_gc_stats then pure (0, 0, 0) else
    (\s -> (allocated_bytes s, copied_bytes s, max_mem_in_use_bytes s))
    <$> getRTSStats
#elif MIN_VERSION_base(4,6,0)
  if not has_gc_stats then pure (0, 0, 0) else
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

data Measurement = Measurement
  { measIters  :: !Word64 -- ^ number of iterations
  , measTime   :: !Word64 -- ^ time in picoseconds
  , measAllocs :: !Word64 -- ^ allocations in bytes
  , measCopied :: !Word64 -- ^ copied bytes
  , measMaxMem :: !Word64 -- ^ max memory in use
  }

-- | Measurement paired with end time.
data Measured = Measured
  { mdMeas     :: !Measurement
  , _mdEndTIme :: !Word64
  }

instance Semigroup Measured where
  Measured !m1 _ <> Measured !m2 !e2 = Measured m3 e2
    where
      on h g = h (g m1) (g m2)
      add = on (+)
      max_of = on max
      m3 = Measurement { measIters = measIters m2
                       , measTime = add measTime
                       , measAllocs = add measAllocs
                       , measCopied = add measCopied
                       , measMaxMem = max_of measMaxMem
                       }
  {-# INLINE (<>) #-}

data Estimate = Estimate
  { estMean  :: !Measurement
  , estStdev :: !Word64 -- ^ stdev in picoseconds
  }

type OLS = Ranged Word64
type R2 = Ranged Double
type Mean = Ranged Word64
type Stdev = Ranged Word64

data KDE = KDE
  { kdValues :: {-# UNPACK #-} ![Double]
  , kdPDF    :: {-# UNPACK #-} ![Double]
  }

data OutlierEffect
  = Unaffected
  | Slight
  | Moderate
  | Severe
  deriving (Show)

data OutlierVariance = OutlierVariance
  { ovEffect   :: OutlierEffect
  , ovDesc     :: String
  , ovFraction :: !Double
  }

data Summary = Summary
  { smEstimate   :: !Estimate
  , smOLS        :: !OLS
  , smR2         :: !R2
  , smStdev      :: !Stdev
  , smMean       :: !Mean
  , smKDEs       :: KDE
  , smMeasured   :: [Measurement]
  , smOutlierVar :: !OutlierVariance
  }

square :: Num a => a -> a
square x = x * x
{-# INLINE square #-}

predict
  :: Measurement -- ^ time for the previous run
  -> Measurement -- ^ time for the current run
  -> Estimate
predict (Measurement n1 t1 a1 c1 m1) (Measurement n2 t2 a2 c2 m2) = Estimate
  { estMean  = Measurement n1 t (fit a1 a2) (fit c1 c2) (max m1 m2)
  , estStdev = truncate (sqrt d)
  }
  where
    !t = fit t1 t2
    fit x1 x2 = n1 * (x1 `quot` n3 + x2 `quot` n3)
      where
        n3 = n1 + n2
    d = square (word64ToDouble t1 - t') + square (word64ToDouble t2 - r * t')
      where
        r = word64ToDouble n2 / word64ToDouble n1
        t' = word64ToDouble t

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
runLoop :: Semigroup a => Benchmarkable -> Word64 -> (IO () -> IO a) -> IO a
runLoop Benchmarkable{..} n f
  | perRun    = work >>= go (n - 1)
  | otherwise = work
  where
    go 0 result   = pure result
    go !i !result = work >>= go (i - 1) . (<>) result

    count | perRun = 1
          | otherwise = n

    work = do
      e <- allocEnv count
      let clean = cleanEnv count e
          run = runRepeatedly e count
      clean `seq` run `seq` evaluate (rnf e)
      f run `finally` clean
    {-# INLINE work #-}
{-# INLINE runLoop #-}

measure :: MEnv -> Word64 -> Benchmarkable -> IO Measured
measure MEnv{meConfig=cfg, meHasGCStats=gc} num b =
  runLoop b num $ \act -> do
    let getTimePicoSecs' = getTimePicoSecs (cfgTimeMode cfg)

    performMinorGC
    (start_allocs, start_copied, start_max_mem) <- getAllocsAndCopied gc
    start_time <- getTimePicoSecs'
    act
    end_time <- getTimePicoSecs'
    performMinorGC
    (end_allocs, end_copied, end_max_mem) <- getAllocsAndCopied gc

    let meas = Measurement
          { measIters = num
          , measTime = end_time - start_time
          , measAllocs = end_allocs - start_allocs
          , measCopied = end_copied - start_copied
          , measMaxMem = max end_max_mem start_max_mem
          }

    pure $ Measured meas end_time

measureUntil :: MEnv -> Benchmarkable -> IO Summary
measureUntil menv@MEnv{meConfig=Config{..}} b
  | is_once = fmap (measToSummary . mdMeas) (measure menv 1 b)
  | otherwise = init_and_go
  where
    is_once = isInfinite cfgRelStDev && 0 < cfgRelStDev

    -- See 'Criterion.Measurement.{squish,series}' in the package
    -- 'criterion-measurement'.
    series = squish (unfoldr f 2)
      where
        squish = foldr g []
          where g x xs = x : dropWhile (== x) xs
        f k = Just (truncate l, l)
          where l = k * 1.05 :: Double

    init_and_go = do
      performGC
      start_time <- getTimePicoSecs cfgTimeMode
      Measured m0 _ <- measure menv 1 b
      debugStr' menv $ formatMeasurement m0
      go series start_time m0 $ Acc
        { acStdevs = []
        , acMeasurements = [m0]
        , acCount = if threshold < measTime m0 then 1 else 0
        }

    go [] _ _ _ = error "measureUntil.go: empty series"
    go (!n:ns) !start_time m1 !acc = do
      Measured m2 end_time <- measure menv n b
      debugStr' menv $ formatMeasurement m2
      let est@(Estimate measN stdevN) = predictPerturbed m1 m2
          !is_stdev_in_target_range =
            stdevN < truncate (cfgRelStDev * word64ToDouble (measTime measN))
          !is_timeout_soon = case cfgTimeout of
            Timeout micros ->
              let next_end = end_time + measTime m2*2 + 30*oneMillisecond
              in  micros * 1000000 < next_end - start_time
            _ -> False
          !count | threshold < measTime m2 = acCount acc + 1
                 | otherwise = acCount acc
          !acc' = acc { acStdevs = (stdevN `quot` n) : acStdevs acc
                      , acMeasurements = m2 : acMeasurements acc
                      , acCount = count
                      }
      warnOnTooLongBenchmark cfgTimeout start_time end_time
      -- Need at least 4 long enough measurements to get IQR while
      -- computing KDE. Later the measurement data are filtered in the
      -- 'summarize' function.
      if 4 <= acCount acc' &&
         (is_stdev_in_target_range ||
          is_timeout_soon)
        then pure $ summarize acc' est
        else go ns start_time m2 acc'

measToSummary :: Measurement -> Summary
measToSummary m@(Measurement {measTime=t}) =
  Summary { smEstimate = Estimate m 0
          , smOLS = toRanged t
          , smR2 = toRanged 1
          , smStdev = toRanged 0
          , smMean =  toRanged t
          , smKDEs = KDE [] []
          , smMeasured = []
          , smOutlierVar = OutlierVariance Unaffected "no" 0
          }
{-# INLINABLE measToSummary #-}

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

-- According to the UNPACK section of the ghc users guide, the two
-- lists fields in Acc are Sum types, thus -funbox-strict-fields does
-- not unpack them. Explicitly telling GHC to unpack the list fields,
-- since the fields does not perform expensive computations.

data Acc = Acc
  { acStdevs       :: {-# UNPACK #-} ![Word64]
  , acMeasurements :: {-# UNPACK #-} ![Measurement]
  , acCount        :: !Word64
  }

-- XXX: Order of fields make difference in performance?
--
-- data Acc = Acc
--   { acCount        :: !Word64
--   , acMeasurements :: {-# UNPACK #-} ![Measurement]
--   , acStdevs       :: {-# UNPACK #-} ![Word64]
--   }

-- | 30 milliseconds in picosecond.
threshold :: Word64
threshold = 30000000000
{-# INLINE threshold #-}

summarize :: Acc -> Estimate -> Summary
summarize acc (Estimate measN stdevN) = Summary
  { smEstimate = Estimate meas sd_scaled
  , smOLS = ols
  , smR2 = rsq
  , smStdev = Ranged sd_min sd_all_w64 sd_max
  , smMean = mean_r
  , smKDEs = kde
  , smMeasured = measured
  , smOutlierVar = ov
  }
  where
    meas = scale measN
    measured = reverse (acMeasurements acc)

    -- Filtering out measurements with too short total duration, since
    -- those data are considered imprecise and unreliable. See
    -- 'Criterion.Analysis.analyseSample'.
    times = [ measTime m `quot` measIters m
            | m <- measured, threshold < measTime m ]
    !len = fromIntegral (acCount acc)

    !mean_all = sum [word64ToDouble t | t <- times] / len
    (mean_min, mean_max) = minMax times
    mean_r = Ranged mean_min (ceiling mean_all) mean_max

    !sd_all = computeSSD len mean_all times
    !sd_all_w64 = ceiling sd_all
    !sd_scaled = stdevN `quot` measIters measN
    (sd_min, sd_max) = minMax (sd_all_w64 : sd_scaled : acStdevs acc)

    (ols, rsq) = regress sd_all xys
    xys = [ (word64ToDouble (measIters m), word64ToDouble (measTime m))
          | m <- measured ]

    (kde, ov) = kdeAndOutlierVariance mean_r sd_all len times
{-# INLINE summarize #-}

scale :: Measurement -> Measurement
scale (Measurement n t a c m) = Measurement n t' a' c' m
  where
    t' = t `quot` n
    a' = a `quot` n
    c' = c `quot` n
{-# INLINE scale #-}

minMax :: [Word64] -> (Word64, Word64)
minMax = foldr f z
  where
    f x (amin, amax) = (min amin x, max amax x)
    z = (maxBound, minBound)
{-# INLINABLE minMax #-}


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
regress :: Double             -- ^ Sample standard deviation
        -> [(Double, Double)] -- ^ Pair of x and y values
        -> (OLS, R2)
regress ssd xs_and_ys = (ols, r2)
  where
    ols = fmap ceiling (ci95 sample_size ssd a)
    r2 = Ranged (fr2 (-1)) (fr2 0) (fr2 1)

    -- means and sample size
    (x_mean, y_mean, sample_size) = (sum_x / n, sum_y / n, n)
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
computeSSD :: Double   -- ^ Length of the list of values
           -> Double   -- ^ Mean
           -> [Word64] -- ^ List of values
           -> Double
computeSSD !n !mean xs =
  sqrt (sum [square (word64ToDouble x - mean) | x <- xs] / (n-1))
{-# INLINABLE computeSSD #-}

-- | Compute 95% confidence interval from sample standard deviation.
ci95 :: Double -- ^ Number of samples.
     -> Double -- ^ Sample standard deviation.
     -> Double -- ^ The point value.
     -> Ranged Double
ci95 n ssd x = Ranged (x-w) x (x+w)
  where
    !w = (ssd / sqrt n) * 1.96
{-# INLINABLE ci95 #-}

-- | Compute kernel density estimation and outlier variance effect.
kdeAndOutlierVariance :: Ranged Word64 -- ^ Range to get min and max
                      -> Double        -- ^ Sample standard deviation
                      -> Double        -- ^ Length of the list
                      -> [Word64]      -- ^ The list containing values
                      -> (KDE, OutlierVariance)
kdeAndOutlierVariance !mean_w64 !s !n xs_w64 = (kde, ov)
  where
    kde = KDE {kdValues=values, kdPDF=density}

    -- Dividing 120% of the range to 128 points.
    values = enumFromThenTo lo' (lo'+delta) hi'
      where
        delta = (hi' - lo') / 127
        lo' = lo - r/10
        hi' = hi + r/10
        r = hi - lo
        Ranged lo _ hi = fmap picoToSecs mean_w64

    -- Using simple Gaussian kernel function and Silverman's rule of
    -- thumb for bandwidth.
    density = [sum [k ((x-xi)/h) | xi<-xs] / (n*h) | x<-values]
      where
        k u = exp (-(u*u/2)) / sqrt (2*pi)
        !h = 0.9 * min s_in_seconds s'_in_seconds * (n ** (-0.2))

    -- Comparing sample standard deviation to pseudo standard
    -- deviation calculated from IQR. See
    -- 'Criterion.Analysis.outlierVariance'.
    ov = OutlierVariance effect desc frac
      where
        (effect, desc) | frac < 0.01 = (Unaffected, "no")
                       | frac < 0.1  = (Slight,     "a slight")
                       | frac < 0.5  = (Moderate,   "a moderate")
                       | otherwise   = (Severe,     "a severe")
        frac = 1 - min 1 (s'_in_seconds / s_in_seconds)

    s_in_seconds = s / 1e12
    s'_in_seconds = iqr / 1.349
      where
        iqr = q3 - q1
        q3 = xs !! ceiling (n * 0.75)
        q1 = xs !! truncate (n * 0.25)
    xs = sort $ map picoToSecs xs_w64
{-# INLINABLE kdeAndOutlierVariance #-}


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

nf' :: (b -> c) -> (a -> b) -> a -> Word64 -> IO ()
nf' frc = benchLoop SPEC
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
{-# NOINLINE nf' #-}

whnf' :: (a -> b) -> a -> Word64 -> IO ()
whnf' = go SPEC
  where
    -- See the comment in `nf'' for explicit `f' and `x'.
    go !_ f x n
      | n == 0 = pure ()
      | otherwise = do
          _ <- evaluate (f x)
          go SPEC f x (n - 1)
{-# NOINLINE whnf' #-}

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
