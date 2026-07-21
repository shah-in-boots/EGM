# EGM 0.2.0

This release includes major improvements to WFDB functionality and package structure.

* **Window management suite**: New functions for managing `windowed` signal
  collections. `median_window()` collapses windowed beats into a single median
  `EGM` template; `pad_window()` zero-pads windows to a common length, optionally
  anchoring a fiducial such as the QRS peak; `resample_window()` (and a new
  `resample_frequency` argument to `window()`) up- or down-samples windows to a
  common rate while preserving duration; and `normalize_window()` stretches whole
  windows onto a fixed length.

* **Landmark templates**: New S7 `landmark` and `template` classes provide a
  small, validated data model for fiducial templates. `learn_template()` learns
  landmark phase positions from annotated EGM examples; manual templates are
  constructed directly with positioned `landmark()` objects and `template()`.
  `warp_window()` accepts a template and aligns all channels using explicit
  missing, ambiguous, and crossed-landmark policies, strict channel precedence,
  non-zero sample-coordinate support, and phase-warp provenance. This adds a
  dependency on the `S7` package.

* **Bug fix**: `print()`/`format()` for `windowed` objects now report the
  window method correctly (previously read a non-existent attribute and printed
  a blank).

* **Time-based WFDB ranges**: WFDB readers now accept time values for
  `begin` and `end`, plus numeric or compact character durations for `interval`.
  Read windows are consistently half-open and clamp at the end of the study.

* **Consistent helper names**: `get_signal()` replaces `extract_signal()`, and
  internal helpers no longer use a leading dot.

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

	
