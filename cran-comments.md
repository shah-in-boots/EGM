## R CMD check results

0 errors | 0 warnings | 0 notes

## Release summary

Version 0.3.0 is a breaking release.

* The plotting layer (`ggm()` and its themes) is removed and `ggplot2` leaves
  Imports. `EGM` is the signal back end; plotting moves to a companion package.
* `add_annotation()` resolves a channel given as a lead name to its signal
  number. It previously could never match a name, and a name that slipped
  through was written to disk as channel 0. `write_annotation()` now refuses
  such a table.
* Annotation channel numbering is settled at the reader: a file counting
  signals from 0 is renumbered as it is read and restored as it is written.
* Windowing strategies are objects (`by_rhythm()`, `by_beat()`, `by_pwave()`),
  the fibrillatory-wave analysis follows its cited methods, and a
  vectorcardiogram is added. NEWS.md lists every renamed or removed function.

## Test environments

* local: macOS (darwin, R 4.6.1)
* GitHub Actions:
  - windows-latest (R release)
  - macOS-latest (R release)
  - ubuntu-latest (R devel, release, oldrel)

## Downstream dependencies

There are currently no downstream dependencies for this package.
