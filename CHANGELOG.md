# Revision history for miniterion

## Unreleased

* Show time from OLS linear regression, R², and range values in
  benchmark results.

* Modify CSV format to follow the template used in Criterion.

* Discard measurement results when the total duration is shorter than
  threshold.

* Modify timeout detection to refer to the benchmark start and end
  time instead of the sum of the measurement durations.

* Support brace expressions in glob.

* Modify the output of ``-v2`` flag to show divided time.

* Perform minor GC after running target function to update RTSStats.

* Add `--output` (`-o` for short) option to write HTML report.

* Add `--json` option to write JSON summary.

* Add `--iters` option to run benchmarks without analysis.

* Silence the outputs in tests.

## 0.1.1.1 -- 2024-05-29

* Suppress warning messages in ghc 9.10.

## 0.1.1.0 -- 2023-09-20

* Update version bounds of ``deepseq``.

* Reorder exported entities.

* Add "dev" flag for internal development of miniterion.

* Some documentation updates.

## 0.1.0.0 -- 2023-09-15

* Initial release.
