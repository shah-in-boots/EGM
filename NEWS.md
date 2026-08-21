# EGM (development version)

## Breaking

* `read_muse()` sets `ADC_gain` from `<LeadAmplitudeUnits>`, 1000 for
  microvolts. Earlier conversions read 5× too large in physical units; the
  `.dat` was always right, fix the `.hea` gain field. Bundled `muse-sinus`,
  `muse-af`, `ecg`, `ecg-sinus` corrected.
* Annotations spanning more than one channel need a `channel`, and one they do
  not carry is an error. Everywhere; see `?channels`.
* `extract_f_waves()` gains `channel` and no longer pools across channels;
  loses `.force_all`; returns an `f_wave_analysis` object; forwards `...`.
  Cancellation is spatiotemporal across leads (Stridh & Sörnmo, 2001); aberrant
  beats scored by QRS correlation, not RR deviation; amplitude measured in TQ
  segments; entropy tolerance 0.2 SD (was 3.5), `m` 2, band `c(4, 10)`.
* A record with no usable sampling rate is an error, not `NA`.
* `change_frequency()` takes `to` first; `from` defaults to the recorded rate
  and is checked against it.
* `as_ECG()` extracts the surface leads and errors where there are none.
* `window()` is `get_windows()`; `windowed` is `windows`; `is_windowed()` is
  `is_window_set()`; `windowed()` is `new_windows()`.
* Strategies are objects: `by_rhythm()` replaces `window_by_rhythm()`, and
  validates at construction.
* `median_window()` returns the fiducials that produced the beat, under
  `$annotation` — read with `get_annotation()`.
* `pad_window()` pads with `NA`, not `0`.
* `vectorcardiogram()` drops and counts an undelineated beat rather than
  failing the record.
* Removed: `standardize_windows()` (merged into `normalize_window()`, whose
  defaults win), `resample_window()`, `resample_frequency`, `lapply.windowed()`.

## New

* `vectorcardiogram()`, `atrial_vectorcardiogram()` — Kors transform to X, Y, Z,
  cut at the wave boundaries; returns `loop`, `components`, GEH.
  `beats = "median"` or `"all"`. Exported `kors` dataset.
* `by_beat()` — fixed span around a fiducial, equal lengths, nothing to pad.
* `by_pwave()` — P onset to QRS onset. `by_rhythm()` gains `reject_overlap`.
* `baseline_window()` — removes each window's isoelectric offset. Do it before
  reducing.
* `pad_window(pad_value = "edge")` — extends the nearest observed sample.
* `change_frequency()` — `linear`, `spline`, `step`, `polyphase`; anti-aliases
  on downsampling. `map_windows()` replaces `lapply.windowed()`.
* `signal_units()` — digital or physical, carried through every transform.
  `write_wfdb()` refuses a signal contradicting its `units`.
* `window_dropped()` — candidates a strategy did not return, by reason.
* `channel_zero()`, `read_wfdb(channel_zero = "signal")` — declares a file
  numbering channels `0 .. nsig-1`; default stays global.
* `ecg_leads()`, `lead_factor()` — display order and canonical lead names.
* `label_waves()` exported — wave identity is the peak symbol inside each
  `(`…`)`, not the WFDB `number` column.
* `record$rr_regular`, with a warning — cancellation eats a phase-locked atrial
  signal, so flutter reads as fibrillation.
* Per-lead `harmonic_index`, `on_harmonic`, `cancellation_residual`. Never read
  `dominant_rate` without `on_harmonic`.
* `calculate_sample_entropy()` (preferred), `calculate_welch_spectrum()`,
  `calculate_organization_index()`, `spatial_dispersion`,
  `cancel_ventricular_signal()`.

## Updates

* Leads order `I, II, III, AVR, AVL, AVF, V1`–`V6` as displayed; ordered lead
  and catheter factors had alphabetical levels.
* `channel_criteria` superseded by `channel`, taking a number, a name (`"II"`),
  or `list(channel = )`. Warns once per session.
* A heart rate outside 20–300 bpm warns, as does a record not looking like AF.
* Windows cut from an `ECG` are `ECG`s. Provenance is `method` plus `history`.

## Bugs

* Every `print()` method dispatches again; the `S7::method(print, ...)`
  assignments are wrapped in `local()`.
* `frequency()` gains a `header_table` method; it reported 1 Hz for a 500 Hz
  record.
* Beat groups are never subtracted to zero or interpolated across.

# EGM 0.2.0

* Native C++ WFDB reader and writer — no external WFDB installation needed.
* Window suite: `median_window()`, `pad_window()`, `resample_window()`,
  `normalize_window()`.
* S7 `landmark` and `template` classes. `learn_template()` fits landmark phases
  from annotated examples; `warp_window()` aligns to a template with explicit
  missing, ambiguous and crossed-landmark policies. Adds an `S7` dependency.
* WFDB readers take times for `begin`/`end` and durations for `interval`.
  Ranges are half-open and clamp at the end of the study.
* `get_signal()` replaces `extract_signal()`; internal helpers lose the leading
  dot.
* `signal_table` accepts integer data.
* New `annotation-guide` vignette, expanded `wfdb-guide`.
* Removed `segmentation.R` and its vignette.
* Bug: `print()`/`format()` on `windowed` read a non-existent attribute and
  printed a blank window method.

# EGM 0.1.1

Second *CRAN* release.

* `ggm()` themes correctly for dark and light, without importing colors.
* Compatible with `{ggplot2}` v4.0.0.
* F wave extraction and analysis.
* Rudimentary windowing of signal by surface ECG.
* `read_prucka()` for CardioLab EP study recordings.

# EGM 0.1.0

First *CRAN* release — intracardiac electrograms and surface ECGs, their
visualization, and `WFDB` annotations.

* `egm` objects hold signal and metadata across three classes: `signal_table`
  (raw signal), `header_table` (metadata), `annotation_table` (labelled
  samples).
* `read_wfdb()`, `write_wfdb()`.
* `read_muse()` for GE MUSE v9 ECGs, `read_lspro()` for LabSystem Pro EGMs.
