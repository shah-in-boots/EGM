# EGM (development version)

## Vectorcardiography

* **`vectorcardiogram()` and `atrial_vectorcardiogram()` are new.** Both
  reconstruct the orthogonal X, Y, Z leads from the 12-lead ECG with the Kors
  regression transformation (Kors et al., 1990) and cut the result at the
  annotated wave boundaries: the ventricular (QRS) loop and the atrial (P) loop
  respectively. Kors' matrix is preferred to the inverse Dower matrix, which
  reproduces the recorded Frank leads less closely and yields derived measures
  with less prognostic power (Man et al., 2011; Kück et al., 2018).

  Each returns the loop as a `data.table` of `beat`, `sample`, `X`, `Y`, `Z`
  alongside its standard descriptors — peak and mean spatial vector magnitude,
  azimuth and elevation of the peak vector, enclosed planar area, and planarity.
  `beats = "median"` gives the signal-averaged loop used to characterise atrial
  conduction (Havmöller et al., 2007); `beats = "all"` gives one loop per beat
  and preserves the beat-to-beat variability that a signal average removes
  (Tachmatzidis et al., 2022).

  Segmentation reuses `get_windows()` and `median_window()`, so wave boundaries
  come from the record's own delineation annotations. A record without them is an
  error rather than a guess, as is a record whose annotations span several
  channels without a guiding `channel`.

## The ECG class

* **`as_ECG()` now extracts the surface ECG rather than relabelling the whole
  record** (**breaking**). An electrophysiology study records surface and
  intracardiac channels side by side; `as_ECG()` keeps the recognised surface
  leads, renames them canonically, and reports the channels it dropped. A record
  with no surface leads is an error: an intracardiac channel cannot stand in for
  a surface lead, and the analyses gated on this class would otherwise return a
  number that looks reasonable and is not.

* **Surface-only analyses share one gate.** `extract_f_waves()`,
  `vectorcardiogram()` and `atrial_vectorcardiogram()` all coerce their input
  through `as_ECG()` and then check the leads they need. Fibrillatory wave
  extraction accepts any surface lead set, since it reads each lead more or less
  independently; the vectorcardiograms require all eight leads of the Kors
  transformation, which is a fixed linear combination with no substitute for a
  missing one.

* **`extract_f_waves()` loses its `.force_all` argument** (**breaking**). It
  existed to analyse intracardiac channels, which is the result the class now
  exists to prevent. A requested `lead` is resolved canonically, so `"aVR"` and
  `"AVR"` both name the same channel, and an intracardiac one is rejected by
  name.

## Fibrillatory wave analysis

* **`extract_f_waves()` now cancels the ventricular signal spatiotemporally**
  (**breaking**). Cancellation previously ran one lead at a time, re-detecting
  QRS positions in each lead and rebuilding every beat from a low-rank SVD of
  that lead's own beats. A single-lead template cannot absorb the beat-to-beat
  rotation of the heart's electrical axis, so what it left behind was periodic
  at the heart rate and deposited energy on heart-rate harmonics — inside the
  4–9 Hz band the analysis reads. All leads now share one set of QRS positions,
  and each beat is fitted by least squares to a combination of the median
  templates from every lead (Stridh & Sörnmo, 2001).

  On the bundled `muse-af` record the old method returned a dominant rate of
  262 fpm with all 12 leads sitting on a heart-rate harmonic; the new one
  returns 443 fpm with none.

* **Every spectral feature now arrives with a contamination diagnostic.**
  `harmonic_index`, `on_harmonic`, and `cancellation_residual` are returned per
  lead. `dominant_rate` must not be used without conditioning on `on_harmonic`:
  a contaminated estimate is not noisy but precise, wrong, and highly
  reproducible, so validating the feature by test–retest reliability selects the
  artifact. See `?f_wave_diagnostics`.

* **Aberrant beats are identified by QRS morphology, not RR interval**
  (**breaking**). The previous rule flagged a beat when its RR interval deviated
  by more than 40% from the median. In atrial fibrillation the RR interval is
  irregular by definition, so it fired on normally conducted beats in the exact
  rhythm the function targets — 21% of beats on the bundled AF record. Beats are
  now scored by correlation against the median template.

* **Beat groups are never subtracted to zero.** A group small enough for a
  low-rank model to reconstruct exactly was subtracted to an identically zero
  residual, and a lone aberrant beat had its window replaced by linear
  interpolation across 377 ms. Both deleted the atrial signal outright, and both
  were silent. The model rank is now capped below the group size, a minimum
  group size is enforced, and no window is ever blanked or interpolated across.

* **Entropy is computed on a decimated signal, and sample entropy is available.**
  Approximate entropy is O(n²) and ran at 1000 Hz, taking about 18 s per lead —
  over 99% of the runtime, and roughly 216 s for a 12-lead ECG. The atrial
  signal is now decimated to `entropy_rate` (default 50 Hz) first. A full
  12-lead analysis takes about 0.2 s. `calculate_sample_entropy()` is new and
  preferred: approximate entropy counts self-matches, which biases it toward
  regularity and makes it depend on record length.

* **The default entropy tolerance changed from 3.5 to 0.2 standard deviations**
  (**breaking**). At 3.5 SD nearly every pair of vectors counts as a match, which
  drove the statistic toward zero regardless of input — 0.0024 on the bundled AF
  record, against 1.01 at the conventional tolerance. `m` now defaults to 2.

* **Dominant frequency is estimated from a Welch periodogram**, optionally
  pooled across leads. A single raw periodogram is an inconsistent estimator: its
  variance does not fall as the record lengthens, so the argmax over a several-Hz
  band is unstable on a 10 s record. `calculate_welch_spectrum()` is exported.

* **Amplitude is measured in the TQ segments**, where the ventricles are
  electrically silent, rather than over the whole record. Whatever cancellation
  fails to remove is concentrated at the QRS, so a whole-record RMS scored
  records with poor cancellation as having large f waves. TQ boundaries come from
  an `ecgpuwave`-style annotation when one is attached, and from a fixed
  exclusion window otherwise. `f_ratio` normalises by the QRS excursion in the
  same lead, which cancels the thoracic transfer function to first order and
  makes amplitudes comparable between patients.

* **New features and arguments.** `calculate_organization_index()` reports the
  share of 2.5–15 Hz power at the dominant frequency and its first harmonic.
  `spatial_dispersion` gives the coefficient of variation of f-wave amplitude
  across leads. The search band is exposed through `band` (default `c(4, 10)`);
  the previous 4–9 Hz window excluded slow and drug-modified flutter.
  `cancel_ventricular_signal()` is exported for use on a bare multi-lead list.

* **`extract_f_waves()` returns an `f_wave_analysis` object** (**breaking**),
  holding a `features` table with one row per lead and a one-row `record` table,
  rather than a nested list. Its `...` is now actually forwarded; it was
  previously declared and silently discarded, so no argument could reach any
  analysis function.

* `extract_f_waves()` warns when the record does not look like atrial
  fibrillation. In sinus rhythm there is no fibrillatory wave to find, so the
  estimator returns whatever is largest in the band.

* Internal `remove_ventricular_signal()` now requires `frequency` rather than
  defaulting to 1000 Hz, which silently produced QRS detection filters designed
  for the wrong Nyquist at any other sampling rate. Analysis runs at the record's
  native rate; the previous upsample to 1000 Hz added no information.

* Surface lead matching no longer strips the letter `s` from lead names. The
  pattern `[_\s-]` matched a literal `s` rather than whitespace in R's default
  regex engine.

## Windowing

* **`window()` is now `get_windows()`** (**breaking**). The old name masked
  `stats::window()`, which is a real S3 generic, so attaching EGM broke
  `window()` for every `ts` object in the session. Extending that generic was not
  an option: `stats::window()` returns one subset of a series, whereas this
  returns a collection of segments matching a search. The class `windowed` is
  likewise now `windows`, with `is_windowed()` becoming `is_window_set()` (named
  to avoid confusion with the common operating-system predicate `is_windows()`)
  and the constructor `windowed()` becoming `new_windows()` (**not** `windows()`,
  which would mask `grDevices::windows()` on Windows builds of R).

* **Windowing strategies**: How to find windows is now described by a strategy
  object rather than by arguments on the entry point. `by_rhythm()` replaces
  `window_by_rhythm()` and takes the rhythm arguments under shorter names
  (`onset`, `offset`, `reference`, `channel`), validating them at construction.
  Previously these travelled through `...` and a misspelled argument was silently
  discarded; it is now an error. Strategies are values, so one specification can
  be reused across every record in a study:

  ```r
  get_windows(ecg, by = "rhythm")               # defaults
  get_windows(ecg, by = by_rhythm(channel = 2)) # explicit

  woi <- by_rhythm(channel = 2)
  lapply(records, get_windows, by = woi)
  ```

  Adding a strategy no longer widens a shared argument list; each gets its own
  constructor and its own help page.

* **`standardize_windows()` has been removed**, merged into `normalize_window()`,
  which gains its `align_feature` and `channel_criteria` arguments. The two were
  the same engine with alignment switched on or off. Callers moving across should
  note that `normalize_window()`'s defaults win: `preserve_amplitude = FALSE`
  (was `TRUE`) and `preserve_class = TRUE` (was `FALSE`).

* **`lapply.windowed()` has been removed.** It was never reachable — `base::lapply`
  is not an S3 generic, so the method could not dispatch. New `map_windows()`
  does what it was meant to: applies a function across a collection, rebuilding a
  `windows` object when every result is an `EGM` and returning a plain list
  otherwise.

* **Window provenance survives the pipeline.** The `window_method` attribute was
  overwritten by every transform, so a padded collection no longer knew it came
  from rhythm windowing. It is replaced by `method` (the extraction strategy, set
  once) and `history` (every step applied, in order, e.g.
  `c("rhythm", "padded", "normalized")`). The redundant `window_count` attribute
  is gone; it was always `length(x)`. `print()` now shows the history chain.

* **Internal**: the 2000-line `R/window.R` is split into `R/windows-class.R`,
  `R/windows-extract.R`, `R/windows-transform.R`, and `R/features.R` (the
  annotation-criteria helpers shared with the template code).

* **Resampling is now a pipe stage**: New `change_frequency()` converts an `EGM`,
  a `windows` collection, a list of `EGM` objects, or a bare numeric lead from
  one sampling frequency to another, preserving duration and moving every
  annotator onto the new grid. Both `from` and `to` must be stated; for objects
  carrying a header, the declared `from` is checked against the recorded rate.
  Four methods are offered - `linear` (default), `spline`, `step`, and
  anti-aliased `polyphase` - and down-sampling with an interpolating method now
  low-pass filters first (`anti_alias = TRUE`) so that content above the new
  Nyquist frequency is not folded back into the band. New `frequency()` methods
  report the sampling rate of an `EGM` or `windows` object.

  This replaces `resample_window()` and the `resample_frequency` argument of
  `window()` (now `get_windows()`), both of which have been **removed**, along
  with the internal `upsample_signal()` used by the f-wave pipeline. Rate changes
  are now expressed
  as their own step, for example
  `read_wfdb(...) |> change_frequency(from = 250, to = 500) |> get_windows()`.

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

	
