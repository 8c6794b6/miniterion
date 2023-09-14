module Main (main) where

-- base
import           Control.Exception  (catch, fromException, throwIO)
import           Data.Functor       (void)
import           System.Environment (withArgs)
import           System.Exit        (ExitCode (..), exitFailure, exitSuccess)
import           System.IO.Error    (isDoesNotExistError)

-- directory
import           System.Directory   (removeFile)

-- tasty
import           Test.Tasty         hiding (defaultMain)
import qualified Test.Tasty         as Tasty

-- tasty-hunit
import           Test.Tasty.HUnit

-- Internal
import           Miniterion


-- ------------------------------------------------------------------------
-- Main
-- ------------------------------------------------------------------------

main :: IO ()
main = Tasty.defaultMain $
  testGroup "All"
  [ benchmarkable
  , options
  , skipping
  , substr
  , glob
  , csv
  , timelimit
  ]


-- ------------------------------------------------------------------------
-- Test trees
-- ------------------------------------------------------------------------

benchmarkable :: TestTree
benchmarkable = testGroup "benchmarkable"
  [ testCase "fib" $
    defaultMain
    [ bgroup "fib-nf"
      [ bench "4" (nf fib 4)
      , bench "8" (nf fib 8) ]
    , bgroup "fib-whnf"
      [ bench "4" (whnf fib 4)
      , bench "8" (whnf fib 8) ]]

  , testCase "wcIO" $
    defaultMain
    [ bgroup "wcIO"
      [ bench "nfIO" (nfIO (wcIO miniterionDotCabal))
      , bench "whnfIO" (whnfIO (wcIO miniterionDotCabal))
      , bench "nfAppIO" (nfAppIO wcIO miniterionDotCabal)
      , bench "whnfAppIO" (whnfAppIO wcIO miniterionDotCabal) ]]

  , testGroup "env"
    [ testCase "wc with env" $
      defaultMain
      [ env (readFile miniterionDotCabal) $ \contents ->
          bench "wc" (nf wc contents) ]]

  , testGroup "perBatchEnv"
    [ testCase "wc with perBatchEnv" $
      defaultMain
      [ bench "wc" $
        perBatchEnv
        (\_ -> readFile miniterionDotCabal)
        (pure . wc)
      ]
    ]

  , testGroup "perRunEnv"
    [ testCase "wc with perRunEnv" $
      withArgs ["--stdev", "20"] $
      defaultMain
      [ bench "wc" $
        perRunEnv
        (readFile miniterionDotCabal)
        (pure . wc)
      ]
    , testCase "perRunEnv with time limit" $
      withArgs ["-L2", "-s1e-9"] $
      defaultMain
      [ bench "fib" $
        perRunEnv
        (pure 32)
        (pure . fib)
      ]
    ]
  , testGroup "interactive"
    [ testCase "simple function" $
      benchmark (nf not True)
    ]
  ]

options :: TestTree
options = testGroup "options"
  [ testCase "help with long option" $
    withArgs ["--help"] emptyMain

  , testCase "help with short option" $
    withArgs ["-h"] emptyMain

  , testCase "show version info" $
    withArgs ["--version"] emptyMain

  , testCase "listing names with long option" $
    withArgs ["--list"] benchFib4

  , testCase "listing names with short option" $
    withArgs ["-l"] benchFib4

  , testCase "listing name of benchmark using env" $
    withArgs ["--list"] benchWithEnv

  , testCase "listing name of benchmark using env and pat" $
    shouldExitFailure $ withArgs ["--list"] benchWithEnvAndPat

  , testCase "listing name of benchmark using env and irrefultable pat" $
    withArgs ["--list"] benchWithEnvAndIrrPat

  , testCase "stdev option" $
    withArgs ["--stdev", "20"] benchFib4

  , testCase "short stdev option" $
    withArgs ["-s", "20"] benchFib4

  , testCase "infinit stdev" $
    withArgs ["--stdev", "Infinity"] benchFib4

  , testCase "invalid stdev arg" $
    shouldExitFailure $ withArgs ["--stdev", "foo"] emptyMain

  , testCase "missing stdev arg" $
    shouldExitFailure $ withArgs ["--stdev"] emptyMain

  , testCase "cpu clock for time-mode option" $
    withArgs ["--time-mode", "cpu"] benchFib4

  , testCase "wall clock for time-mode option" $
    withArgs ["--time-mode", "wall"] benchFib4

  , testCase "invalid time-mode option" $
    shouldExitFailure $ withArgs ["--time-mode", "blah"] benchFib4

  , testCase "invalid timeout option" $
    shouldExitFailure $ withArgs ["--time-limit", "foo"] benchFib4

  , testCase "verbosity 0" $
    withArgs ["--verbosity", "0"] benchFib4

  , testCase "verbosity 1" $
    withArgs ["-v", "1"] benchFib4

  , testCase "verbosity 2" $
    withArgs ["-v2"] benchFib4

  , testCase "invalid verbosity" $
    shouldExitFailure $ withArgs ["--verbosity", "foo"] benchFib4

  , testCase "out of range verbosity" $
    shouldExitFailure $ withArgs ["--verbosity", "100"] benchFib4

  , testCase "non existing option" $
    shouldExitFailure $ withArgs ["--no-such-option"] emptyMain
  ]

skipping :: TestTree
skipping = testGroup "skipping"
  [ testCase "selecting benchmarks" $
    withArgs ["2"] benchNesting

  , testCase "selecting benchmarks, skipping group" $
    withArgs ["c.1.A"] benchNesting

  , testCase "no matching benchmark" $
    withArgs ["no-matching-benchmark"] benchNesting

  , testCase "selecting under env, strict" $
    shouldExitFailure $
    withArgs ["fiba"] benchNestingEnvStrict

  , testCase "selecting under env, strict, under group" $
    shouldExitFailure $
    withArgs ["fiba"] benchNestingEnvStrict_grouped

  , testCase "selecting under env" $
    withArgs ["a"] benchForMatch
  ]

benchNestingEnvStrict_grouped :: IO ()
benchNestingEnvStrict_grouped =
  defaultMain
  [ bgroup "a"
    [ bgroup "1" [s, p]
    , bgroup "2" [s, p] ]
  , bgroup "b"
    [ env (pure (8, ())) $ \(a, _) ->
        bench "fiba" (nf fib a)
    ]
  ]

substr :: TestTree
substr = testGroup "substr"
  [ testCase "substring match (case sensitive)" $
    substr_test ["--match", "pattern", "oob"] "foobar"

  , testCase "substring match (case insensitive)" $
    substr_test ["-m", "ipattern", "oOB"] "foobar"

  , testCase "prefix match" $
    substr_test ["-m", "prefix", "foo"] "foobar"

  , testCase "invalid match mode" $
    shouldExitFailure $
    withArgs ["-m", "no_such_mode"] $
    defaultMain
    [ bench "foo" (nf fib 8) ]
  ]
  where
    substr_test args str =
      shouldExitFailure $
      withArgs args $
      defaultMain
      [ bench "don't match me" (nfIO exit)
      , bench str (nfIO (exitFailure :: IO ()))
      , bench "don't match me either" (nfIO exit)
      ]
    exit :: IO ()
    exit = exitSuccess

glob :: TestTree
glob = testGroup "glob"
  [ testCase "simple pattern" $
    glob_test "foo" "foo"

  , testCase "pattern with '?'" $
    glob_test "f??" "foo"

  , testCase "pattern with '*'" $
    glob_test "*foo" "foo"

  , testCase "pattern with '*' at the end" $
    glob_test "*f*" "foo"

  , testCase "escaping with '\\'" $
    glob_test "*foo\\?" "foo?"

  , testCase "escape after '*'" $
    glob_test "*foo*\\?" "foo foo foo?"

  , testCase "repeated stars" $
    glob_test "*fo**" "foo"

  , testCase "simple bracket" $
    glob_test "[abcdef]oo" "foo"

  , testCase "bracket with range" $
    glob_test "[a-z]oo" "foo"

  , testCase "bracket with negation" $
    glob_test "[!z]oo" "foo"

  , testCase "bracket with negation and range" $
    glob_test "[!a-d]oo" "foo"

  , testCase "bracket with escapes" $
    glob_test "[\\!\\-][\\!\\-]oo" "!-oo"

  , testCase "char class starting with `]'" $
    glob_test "[]f]oo" "]oo"

  , testCase "char class ending with `-'" $
    glob_test "[]-]oo" "-oo"

  , testCase "char class `[!]a-]'" $
    glob_test "[!]a-]oo" "foo"

  , testCase "unbalanced bracket" $
    glob_test "[foo" "foo"
  ]
  where
    glob_test pat str =
      shouldExitFailure $
      withArgs ["--match=glob", pat] $
      defaultMain
      [ bench "skip me" (nfIO (exitSuccess :: IO ()))
      , bench str (nfIO (exitFailure :: IO ())) ]

csv :: TestTree
csv = with_csv_cleanup $ testGroup "csv"
  [ testCase writing_slow_csv $
    withArgs ["--csv", "slow.csv"] benchSlowfib

  , after_slow_csv $
    testCase "comparing with baseline" $ do
      withArgs ["--baseline", "slow.csv"] benchFastfib

  , testCase "non-existing baseline" $ do
      shouldExitFailure $ withArgs ["--baseline", "nosuch.csv"] benchFastfib

  , testCase writing_quoted_csv $
      withArgs ["--csv", "quotes.csv", "-L3"] benchQuotes

  , after_quoted_csv $
    testCase "reading baseline containing quotes" $ do
      withArgs ["--baseline", "quotes.csv", "-L3"] benchQuotes

  , testCase writing_fast_csv $ do
      withArgs ["--csv", "fast.csv"] benchFastfib

  , after_fast_csv $
    testCase "fail if slower" $ do
      shouldExitFailure $
        withArgs ["--baseline", "fast.csv", "--fail-if-slower", "10"]
        benchSlowfib

  , after_fast_csv $
    testCase "fail if slower, with match" $ do
      shouldExitFailure $
        withArgs ["--baseline", "fast.csv" ,"--fail-if-slower", "10" ,"fib/16"]
        benchSlowfib

  , testCase "fail if slower, invalid arg" $ do
      shouldExitFailure $ withArgs ["--fail-if-slower", "foo"] benchSlowfib

  , after_slow_csv $
    testCase "fail if faster" $ do
      shouldExitFailure $
        withArgs ["--baseline", "slow.csv", "--fail-if-faster", "10"]
        benchFastfib

  , testCase "fail if faster, invalid arg" $ do
      shouldExitFailure $
        withArgs ["--fail-if-faster", "foo"] benchSlowfib
  ]
  where
    writing_slow_csv = "writing slow.csv"
    writing_fast_csv = "writing fast.csv"
    writing_quoted_csv = "names containing double quotes"
    after_slow_csv = after AllSucceed writing_slow_csv
    after_fast_csv = after AllSucceed writing_fast_csv
    after_quoted_csv = after AllSucceed writing_quoted_csv
    csv_cleanup _ = do
      removeFile "slow.csv"
      removeFile "fast.csv"
      removeFile "quotes.csv"
      `catch` \e -> case fromException e of
        Just ioe | isDoesNotExistError ioe -> pure ()
        _                                  -> throwIO e
    with_csv_cleanup = withResource (pure ()) csv_cleanup . const

timelimit :: TestTree
timelimit = testGroup "timeout"
  [ testCase "time limit, long name" $
    shouldExitFailure $
    withArgs ["--time-limit", "1e-6", "--stdev", "1e-9"] benchFib32

  , testCase "time limit, short name" $
    shouldExitFailure $
    withArgs ["-L", "1e-9", "--stdev", "1e-32"] benchFib32

  , testCase "time limit, return before the limit" $
    withArgs ["-L", "1", "--stdev", "1e-32"] benchFib32

  , testCase "invalid time limit arg" $
    shouldExitFailure $
    withArgs ["--time-limit", "foo"] benchFib32

  ]


-- ------------------------------------------------------------------------
-- Auxiliary
-- ------------------------------------------------------------------------

fib :: Int -> Integer
fib n = if n < 2 then toInteger n else fib (n-1) + fib (n-2)

fastfib :: Int -> Integer
fastfib n = fibs !! n where
  fibs = 0 : 1 : zipWith (+) (tail fibs) fibs

wc :: String -> Int
wc = length . words

wcIO :: FilePath -> IO Int
wcIO = fmap wc . readFile

shouldExitFailure :: IO a -> IO ()
shouldExitFailure act = void (act >> throwIO ExitSuccess) `catch` \e ->
  case fromException e of
    Just (ExitFailure {}) -> pure ()
    _                     -> throwIO e

emptyMain :: IO ()
emptyMain = defaultMain []

miniterionDotCabal :: FilePath
miniterionDotCabal = "miniterion.cabal"

benchFib4 :: IO ()
benchFib4 =
  defaultMain
  [ bgroup "fib"
    [ bench "4" (nf fib 4) ]]

benchWithEnv :: IO ()
benchWithEnv =
  defaultMain
  [ bgroup "a"
    [ bench "fibnf" (nf fib 8)
    , bench "fibwhnf" (whnf fib 8) ]
  , env (readFile miniterionDotCabal) $ \contents ->
      bgroup "b"
      [ bench "wcnf" (nf wc contents)
      , bench "wcwhnf" (whnf wc contents) ]]

benchWithEnvAndPat :: IO ()
benchWithEnvAndPat =
  defaultMain
  [ env (pure (3, 4)) $ \ (a, b) ->
      bgroup "fib"
      [ bench "a" (nf fib a)
      , bench "b" (nf fib b) ]]

benchWithEnvAndIrrPat :: IO ()
benchWithEnvAndIrrPat =
  defaultMain
  [ env (pure (3, 4)) $ \ ~(a, b) ->
      bgroup "fib"
      [ bench "a" (nf fib a)
      , bench "b" (nf fib b) ]]

s, p :: Benchmark
s = bench "succ" (nf (succ :: Int -> Int) 1)
p = bench "pred" (nf (pred :: Int -> Int) 1)

benchNesting :: IO ()
benchNesting =
  defaultMain
  [ bgroup "a" [s, p]
  , bgroup "b"
    [ bgroup "1" [s, p]
    , bgroup "2" [s, p] ]
  , bgroup "c"
    [ bgroup "1"
      [ bgroup "A" [s, p] ]
    , bgroup "2"
      [ bgroup "B" [s, p] ]]]

benchNestingEnvStrict :: IO ()
benchNestingEnvStrict =
  defaultMain
  [ bgroup "a"
    [ bgroup "1" [s, p]
    , bgroup "2" [s, p] ]
  , env (pure (1, 2)) $ \ (a, b) ->
      bgroup "b"
      [ bench "fiba" (nf fib a)
      , bench "fibb" (nf fib b) ]]

benchForMatch :: IO ()
benchForMatch =
  defaultMain
  [ bgroup "a"
    [ bgroup "a1" [s, p]
    , bgroup "a2" [s, p] ]
  , env (pure ()) $ \_ ->
      bgroup "b" [s, p] ]

benchSlowfib :: IO ()
benchSlowfib =
  defaultMain
  [ bgroup "fib"
    [ bench "4" (nf fib 4)
    , bench "8" (nf fib 8)
    , bench "16" (nf fib 16) ]]

benchFib32 :: IO ()
benchFib32 =
  defaultMain
  [ bgroup "fib"
    [ bench "32" (nf fib 32) ]]

benchFastfib :: IO ()
benchFastfib =
  defaultMain
  [ bgroup "fib"
    [ bench "4" (nf fib 4)
    , bench "8" (nf fastfib 8)
    , bench "16" (nf fastfib 16) ]]

benchQuotes :: IO ()
benchQuotes =
  defaultMain
  [ bgroup "group \"one\""
    [ bgroup "a" [s, p]
    , bgroup  "b" [s, p] ]
  , bgroup "group two"
    [ bench "\"a\"" (nf fromEnum 'a')
    , bench "\"b\"" (nf fromEnum 'z')]
  , bgroup "group three"
    [ bench "'\"'" (nf fromEnum '"')
    , bench "\"'\"" (nf fromEnum '\'')]
  ]
