# EGM (development version)

* **WFDB fidelity fixes** in the native C++ reader/writer:
    * Fixed a format 8 (8-bit first difference) round-trip bug where the writer
      primed its difference accumulator at 0 while the reader primed it at the
      header initial value, causing sample 0 to be double-counted on read-back.
    * Annotation `chan` and `num` fields are now treated as *persistent* (they
      carry forward to subsequent annotations until changed), matching the WFDB
      specification; `subtype` and `aux` remain per-annotation. This fixes
      reading of standard WFDB annotation files that record these fields only
      when they change. The writer now emits `chan`/`num` records only on change.
    * `write_wfdb()` now computes the WFDB signal checksum from the data being
      written, so files validate cleanly with standard WFDB tools (e.g. `rdsamp`).
    * Fixed missing-value (`NA`) detection for the ADC gain, which previously
      used an equality comparison against `NA_REAL` (always false for a NaN).
* Added regression tests for format 8/80 round-trips, checksum computation, and
  persistent annotation field handling.

# EGM 0.2.0

This release includes major improvements to WFDB functionality and package structure.

* **Native WFDB implementation**: Added C++ implementation for reading and writing WFDB files, removing the system dependency on external WFDB libraries

* **Annotation improvements**: Enhanced annotation handling with new helper functions and improved frequency handling

* **Signal format**: Updated signal format to accept integer data types for improved memory efficiency and compatibility

* **Documentation**: Added new `annotation-guide` vignette and expanded `wfdb-guide` vignette with detailed examples

* **Breaking changes**: Removed `segmentation.R` functions and related vignette (functionality may be restored in future releases)

* **Code quality**: Extensive refactoring and documentation improvements across the package, including detailed inline comments

# EGM 0.1.1

This is an updated release to *CRAN* for this package.
The changes to this version are as follows.

* Update to `ggm()` function to allow for appropriate theming for dark and light color themes (removes issues with importing colors)

* Update to code and testing suite to assess compatibility with the `{ggplot2}` __v4.0.0__ release

* Addition of a series of functions for F wave extraction and analysis

* Addition of rudimentary functions for windowing signal based on surface ECG

* New function to read in Prucka (CardioLab) for EP study recordings with the `read_prucka()` function

# EGM 0.1.0

This is the first *CRAN* release for this package. 
The initial version contains key features for working with intracardiac electrograms (EGM) and surface electrocardiograms (ECG), visualizing signals, and working with annotations stored in the `WFDB` format.
A single, major class is introduced here.

- `egm` objects contain signal data and meta-data with specific dispatch methods, and are composed of three internal classes
	- a `signal_table` that contains the raw signal data
	- a `header_table` that contains meta-data about the signal data
	- an `annotation_table` that identifies samples and labels them with specific annotations

Additional I/O features are introduced to work with data stored in the `WFDB` format:

- `read_wfdb()` reads in `WFDB` data and returns an `egm` object
- `write_wfdb()` writes an `egm` object to a `WFDB`-compatible format

This first version also allows working with ECG and EGM data using:

- `read_muse()` for ECG data (GE MUSE, v9)
- `read_lspro()` for EGM data (LabSystem Pro)

	
