# Revision history for miniterion

## Unreleased

* Show R², median, and min/max values in benchmark results.

* Modify CSV format to follow the template used in Criterion.

* Discared measurement results with its total duration shorter than
  threshold when initializing batch benchmarks.

* Modify time out detection to refer benchmark start and end time
  instead of adding up measurement durations.

* Modify output with ``-v2`` flag to show divided time.

* Perform minor GC after running target function to update RTSStats.

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
