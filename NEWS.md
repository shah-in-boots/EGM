# EGM (development version)

## Breaking

* `read_muse()` sets `ADC_gain` from `<LeadAmplitudeUnits>`, 1000 for
  microvolts. Earlier conversions read 5× too large in physical units; the
  `.dat` was always right, fix the `.hea` gain field. Bundled `muse-sinus`,
  `muse-af`, `ecg`, `ecg-sinus` corrected.
* Annotations spanning more than one channel need a `channel`, and one they do
  not carry is an error. Everywhere; see `?channels`.
* `extract_f_waves()` gains `channel` and no longer pools across channels;
  loses `.force_all`; returns an `f_wave_analysis` object. Cancellation is
  spatiotemporal across leads (Stridh & Sörnmo, 2001); aberrant beats scored by
  QRS correlation, not RR deviation; amplitude measured in TQ segments; entropy
  tolerance 0.2 SD (was 3.5), `m` 2, band `c(4, 10)`.
* **`f_amplitude` is now the root-mean-square amplitude, not the peak-to-peak
  one.** Peak-to-peak is a maximum over its segment, so it grows with the
  segment's length — on white noise its expectation rises 58% between a
  20-sample and a 400-sample window. TQ segments are as long as the RR interval
  allows, which is irregular within an AF record by definition and shorter at
  higher rates between patients, so `f_amplitude_p2p` and `f_ratio` carry a
  heart-rate confound by construction. Both columns are still returned, and
  `normalize = "qrs"` still points `f_amplitude` at `f_ratio`. Both are now
  measured within each TQ segment and reduced by the median across segments;
  `f_amplitude_rms` was a single figure pooled over every TQ sample at once,
  which one noisy segment could carry.
* **`entropy_rate` defaults to 256 Hz, not 50.** Alcaraz et al. (2010) tuned
  sample entropy for atrial fibrillation organisation specifically and found
  classification degraded below 256 Hz. The old default was chosen from the
  fibrillatory bandwidth and the O(n²) cost, which is a compute argument rather
  than a validity one. Batches get slower; pass `entropy_rate = 50` back to
  restore the previous behaviour and its caveat.
* **`calculate_organization_index()` now matches its published definition.** It
  sums the dominant peak with its first four harmonics — `n_harmonics = 4`, as
  Everett et al. and An et al. do — over a `c(0.5, 50)` Hz denominator, clipped
  below Nyquist. It previously summed one harmonic over 2.5–15 Hz, which is not
  anyone's definition and which additionally dropped the harmonic entirely once
  the dominant frequency passed 7.5 Hz, inside the 4–10 Hz range the estimator
  searches. Values fall: `muse-af` lead V1 goes 0.175 → 0.134, almost all of it
  from the wider denominator. They are still not on the published scale — An et
  al. report a median of 0.33 in V1 — and `?calculate_organization_index` now
  says so with the numbers.
* **`harmonic_index` is renamed `harmonic_overlap`.** It is the dominant
  frequency divided by the heart rate, and it is not a published quantity: the
  concern it encodes is established (Ng & Goldberger) but no paper defines this
  ratio, and calling it an index implied otherwise. Same value, same
  `on_harmonic` flag.
* **`record$spatial_dispersion` is removed.** The coefficient of variation of
  f-wave amplitude across leads was invented here and has no published
  counterpart; the spatial variability of the surface f-wave *is* studied, but
  through a principal-component decomposition across leads rather than a CV
  (Meo et al., IEEE Trans Biomed Eng 2013;60(1):20-27). Four of the twelve leads
  are exact linear combinations of two others, which a CV over leads ignores.
* Removed `cancel_method = "adaptive_svd"` and the single-lead helpers behind it
  (`remove_ventricular_signal()`, the ICA path, the Savitzky–Golay smoother).
  The documentation already said not to use it. `"average_beat"` remains as the
  comparison baseline.
* `calculate_approximate_entropy()` loses `implementation`; the R path was a
  slower copy of the C++ one and now lives in the tests, where it checks the C++
  against the definition rather than against itself.
* `extract_f_waves()` and `analyze_atrial_signal()` no longer take `...`.
  Both documented it as unused, so a misspelled argument was silently swallowed.
  `min_beats` and `aberrancy_threshold` are now named arguments of
  `extract_f_waves()`.
* `calculate_welch_spectrum()`, `calculate_sample_entropy()` and
  `calculate_approximate_entropy()` error on a non-finite sample rather than
  dropping it. Dropping joins two stretches that were not adjacent: for a
  spectrum that shifts the whole time axis, and for an entropy it is worse,
  since the statistic *is* the relationship between neighbouring samples and the
  spliced pairs get compared as though they were contiguous.
* `extract_f_waves()` errors where a record carries more than one usable
  annotator instead of reading the first. The `channel` column catches one
  per-lead convention; the other — a file per lead, as LUDB writes — leaves
  `chan` at 0 in every file, so the tables are indistinguishable by channel and
  taking the first silently takes a lead. Read the record with the single
  annotator you mean.
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
* `?extract_f_waves` no longer claims pooling across twelve leads gives twelve
  leads' worth of variance reduction. Four of the twelve are exact linear
  combinations of I and II and the atrial signal is close to a single dipole:
  the correlation matrix of the twelve cancelled leads on `muse-af` has a
  participation ratio of 1.7, so the reduction is about 1.3-fold rather than
  3.5-fold. Pooling is still the default.
* `?calculate_organization_index` says how it differs from Everett et al. — one
  harmonic rather than four, a power rather than a magnitude spectrum, a surface
  lead rather than an interatrial electrogram — and that its values are
  therefore not comparable with published thresholds.
* `?cancel_ventricular_signal` says that the per-beat time shift and time
  scaling of Stridh & Sörnmo are not implemented, so `"spatiotemporal"` is the
  spatial half of the published method.
* `?detect_QRS` says the threshold is a single static one, not the adaptive pair
  with searchback that Pan & Tompkins describe.
* **`?f_wave_diagnostics` now records the provenance of every output** — which
  are published and computed as published (`dominant_rate`, `sample_entropy`,
  `f_amplitude_p2p`, `rr_cv`), which are adaptations of a published quantity
  (`organization_index`, `cancellation_residual`), which are specific to this
  package with no published definition (`harmonic_overlap`, `on_harmonic`,
  `f_ratio`), and which thresholds are package operating points rather than
  published cut-points (`af_like`, `rr_regular`). Citations added throughout: Li
  et al. for coarse-versus-fine f-wave amplitude, Tateno & Glass for the RR
  coefficient of variation, Alcaraz & Rieta and Alcaraz, Sörnmo & Rieta for the
  ventricular residue that `cancellation_residual` is a variant of, and Everett
  et al., An et al. and Stavrakis et al. for the organisation index.
* `f_ratio` is documented as specific to this package: dividing the f-wave
  amplitude by the QRS excursion to cancel the thoracic transfer function is a
  stated rationale, not a validated or published one.
* `on_harmonic` is documented as a sensitive screen rather than a specific test.
  The harmonic index of an honest peak is a ratio of two unrelated numbers, and
  integers are spaced one apart, so a tolerance of 0.15 lands on one 30% of the
  time by arithmetic alone — simulated at 0.301 across 4–10 Hz and 60–160 bpm,
  and flat across rate. Excluding every flagged record discards about a third of
  the good ones too, so it is to be read beside `cancellation_residual`. The
  behaviour is unchanged; what changed is that the documentation now says what
  the flag can and cannot do.

## Bugs

* Every `print()` method dispatches again; the `S7::method(print, ...)`
  assignments are wrapped in `local()`.
* `frequency()` gains a `header_table` method; it reported 1 Hz for a 500 Hz
  record.
* Beat groups are never subtracted to zero or interpolated across.
* **The rhythm warnings survive `verbose = FALSE`.** `verbose` gated them along
  with the progress message, so the two warnings that say the record is not
  fibrillating and that cancellation may have taken the atrial signal with it
  both vanished in exactly the batch setting they exist for.
* **TQ segments are read from a channel-resolved annotation.** The guiding
  `channel` reached the beat positions but not the segment boundaries, so a
  per-lead annotator gave twelve overlapping copies of every segment and
  `tq_fraction` came back greater than one. Every amplitude feature was measured
  over the duplicates.
* **`tq_segments()` identifies waves with `label_waves()`**, positionally from
  the peak symbol each bracket pair encloses, rather than from the WFDB `number`
  column. Any annotator leaving `number` at zero — the bundled `ecg-sinus.ann`
  among them — fell silently to the crude fixed exclusion window while still
  reporting `amplitude_window = "tq"`.
* **`detect_QRS()` returns local maxima.** Both halves of the peak test compared
  a sample against a neighbour in the same direction, so every sample on a
  rising limb was flagged and the refractory loop returned the first of them:
  a threshold crossing, ahead of the beat by an amount that moved with the
  signal. `refine_qrs_positions()` searches ±100 ms, was ±60 ms, which is wider
  than the 75 ms integration lag it is now undoing.
* **Cancellation no longer steps at the edge of a beat window.** Dividing the
  accumulated ventricular estimate by the accumulated weight cancelled the taper
  exactly wherever one window covered a sample, so subtraction switched on at
  its full fitted value: on the bundled `muse-af` record, 3 to 9% of the
  ventricular estimate's own peak, a median of 46 digital units against 0.3 with
  the taper kept, once per beat. No returned feature was seen to move — the
  harmonic share of 2.5–15 Hz atrial power shifts by 0.0005 on `muse-af`, and by
  as little on a synthetic record slow enough that no two beat windows overlap —
  so this is a correctness fix rather than one with a demonstrated effect.
* `extract_f_waves()` keeps annotation channel `0` alongside the requested
  channel only where the table means it globally. On a table declared
  `channel_zero = "signal"` there is no global channel — `0` is a lead like any
  other — so keeping it pooled two leads' fiducials, which is the doubling the
  `channel` guard exists to prevent.
* `f_characteristics = "organization"` returns an organisation index. It was
  computed only when `"dominant_frequency"` was also asked for, so asking for it
  alone returned a table with no such column and said nothing.
* `print()` on an `f_wave_analysis` no longer errors on a record with fewer than
  three beats, where `af_like` is `NA`.
* The QRS excursion behind `f_ratio` is measured before the bandpass. The
  passband stops at 30 Hz and the QRS carries energy above it, so the divisor
  was 4–37% small across the leads of the bundled records, median 14%, by an
  amount that depends on the lead and the QRS width.
* `extract_f_waves()` needs a `channel` for a multi-channel annotation even when
  `qrs_loc` is supplied, which the argument previously said it did not. The beat
  positions come from `qrs_loc`, but the TQ segments are still read from the
  annotations.

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
