# EGM (development version)

## Per-lead annotations

An annotator run once per lead writes twelve independent copies of every
fiducial, separated only by the `channel` column. Every entry point that consumed
annotations resolved that differently — some warned, one threw a message that did
not name the fix, and one silently reported twelve times as many beats as the
record contained. They now resolve it the same way, documented once in
`?channels`.

* **Annotations spanning more than one channel are an error where no `channel`
  is given** (**breaking**). This holds in `get_windows()`, `learn_template()`,
  `extract_f_waves()`, `vectorcardiogram()`, `median_window()`, `pad_window()`,
  `normalize_window()` and `warp_window()`. `get_windows()` previously warned and
  returned anyway, which is the worst of both: the warning disappears in a batch
  job and leaves a well-formed `windows` object that flows happily into
  everything downstream. On a record with 19 QRS complexes it returned 7 windows
  without a channel and 10 with one.

* **A `channel` that the annotations do not carry is an error.** The numbering
  convention belongs to the annotator that wrote the file — WFDB's own tools
  number channels from 0, others from 1 — so the check is what turns a silent
  off-by-one lead into a message naming the channels that exist.

* **`extract_f_waves()` gains a `channel` argument, and no longer pools QRS
  annotations across channels** (**breaking**). Pooled, a 10-second AF record
  reported 73 beats rather than 13, a heart rate of 15,000 bpm, an RR coefficient
  of variation of 2.19 rather than 0.21, and all 12 leads flagged `on_harmonic`
  rather than none — inverting the harmonic-contamination verdict with no error
  and no warning.

* **A heart rate outside 20–300 bpm is warned about.** It is not a rhythm, it is
  a counting error, and it is invisible in every feature the caller goes on to
  read. The guard is independent of the channel fix and would have caught it.

* **`channel_criteria` is superseded by `channel`.** The old name invited a
  criteria list — the shape `onset` and `offset` take a few arguments away —
  while accepting only a scalar, and the neighbouring functions called the same
  thing `channel`. All of them now take `channel`, which accepts a channel
  number, a channel name (`"II"`), or a `list(channel = ...)` wrapper.
  `channel_criteria` still works, and warns once per session.

* **`learn_template()` names the cause of an ambiguous landmark.** The message
  was `Landmark 'P_onset' matched 2 annotations in example 1`, which reports the
  symptom; the count rising with the number of leads is the clue, and it only
  reads as a clue to someone who already knows. It now reads `matched 12
  annotations across 12 channels (1, ..., 12) in example 1, which is one fiducial
  per lead rather than several fiducials; set `channel` to choose a guiding
  lead`.

* **`label_waves()` is exported.** It infers wave identity from the peak symbol
  enclosed by each `(`…`)` pair, and that single fact decides whether an
  annotator is usable: a file leaving the WFDB `number` column at zero throughout
  looks disqualifying and is not. The contract is now stated in `?label_waves`
  and `?annotators` rather than reachable only by reading the source with `:::`.

## Signal units

* **`read_wfdb()` and `read_signal()` label the units they return.** Read them
  back with the new `signal_units()`. `read_wfdb()` has taken a `units` argument
  for some time, but nothing on the returned object said which units were in
  hand, and digital and physical values differ by the ADC gain — 200 in a great
  many records — with no way to tell them apart from the numbers. The label is
  carried through windowing, padding, medians, normalization, warping and
  resampling.

* **`write_wfdb()` refuses to write a signal whose label contradicts its `units`
  argument.** Writing physical values as digital rescales every sample by the
  gain and leaves no trace in the file.

* **`read_wfdb()` matches its `units` argument.** It passed the unmatched default
  through to `read_signal()`, which worked, but an invalid value was not caught
  where it was written.

## Sampling frequency

* **`frequency()` works on a `header_table`.** Without a method it fell through
  to `stats::frequency()`, which answers `1` for any object with no `tsp`
  attribute — so a 500 Hz record reported 1 Hz, which is wrong in a way nothing
  downstream can catch.

* **A record with no usable sampling rate is an error rather than `NA`**
  (**breaking**). An `NA` rate is never a recoverable state: it propagates into
  every interval, heart rate and duration, while the analyses that do not divide
  by it — vectorcardiography among them — go on looking healthy, so the failure
  is invisible in aggregate.

* **`change_frequency()` takes `to` first and defaults `from` to the recorded
  rate** (**breaking**). `change_frequency(ecg, 500)` reads as "resample to 500
  Hz" and now means it; it previously bound 500 to `from` and failed for a
  missing `to`, at the far end of a batch job rather than at the call site. The
  source rate is already on the header, and stating it is now an assertion: a
  disagreement is an error. A bare `numeric` lead still requires `from`, having
  no header to read.

  Calls that passed both positionally must be updated. Where the object carries
  a header the old order now raises the rate-disagreement error rather than
  rescaling by the wrong ratio.

* **The annotation rescaling is documented.** `change_frequency()` rescales the
  annotations it carries, which is correct and not obvious; a second copy read
  separately from disk is left on the original grid, and mixing the two halves
  every interval measured from them.

## Bug fixes

* **Every `print()` method in the package now dispatches.** `S7::method(print,
  cls) <- f` is a replacement call, so at the top level of a package it left a
  copy of `print` in the namespace; each `S3method(print, ...)` directive in
  `NAMESPACE` then registered against that copy rather than `base::print`, and
  printing an `EGM`, a `windows` collection or any of the tables produced a wall
  of raw list output. The three S7 method assignments are now wrapped in
  `local()`.

## Vectorcardiography

* **`vectorcardiogram()` and `atrial_vectorcardiogram()` are new.** Both
  reconstruct the orthogonal X, Y, Z leads from the 12-lead ECG with the Kors
  regression transformation (Kors et al., 1990) and cut the result at the
  annotated wave boundaries: the ventricular (QRS) loop and the atrial (P) loop
  respectively. Kors' matrix is preferred to the inverse Dower matrix, which
  reproduces the recorded Frank leads less closely and yields derived measures
  with less prognostic power (Man et al., 2011; Kück et al., 2018).

  Each returns a plain `list` of the `loop` — a `data.table` of `beat`,
  `sample`, `X`, `Y`, `Z` — and the `components` extracted from it: the peak and
  mean spatial vectors with their azimuth and elevation, the planar area the
  loop encloses, and how far it departs from a plane. `beats = "median"` gives
  the signal-averaged loop used to characterise atrial conduction (Havmöller et
  al., 2007); `beats = "all"` gives one loop per beat and preserves the
  beat-to-beat variability a signal average removes (Tachmatzidis et al., 2022).

  `vectorcardiogram()` additionally returns the global electric heterogeneity
  (GEH) components — spatial QRS-T angle in its peak and mean forms, the spatial
  ventricular gradient, and the sum absolute QRST integral (Waks et al., 2016).
  These describe the discordance between depolarization and repolarization and
  so need the T wave delineated as well; where it is not, they are `NA` rather
  than absent.

  The unit of analysis is one beat, so both accept a whole record, a single
  windowed beat, or a median beat — windowing is `get_windows()` and reduction
  is `median_window()`, and an object that already holds one beat passes through
  each unchanged:

  ```r
  ecg |> get_windows() |> median_window(align_feature = "N") |> vectorcardiogram()
  ```

  Wave boundaries come from the record's own delineation annotations. A record
  without them is an error rather than a guess, as is a record whose annotations
  span several channels without a guiding `channel`.

  `?vectorcardiogram` now tabulates each component's units and says which are
  scale-free. The angles, `planarity` and the organisation measures are;
  `magnitude_*`, `area` and `sai_qrst` inherit the signal's units, so two columns
  of the same table behave differently under a change of gain.

* **`kors` is a new exported dataset**, the 3 by 8 regression matrix itself, so
  it can be inspected and used directly rather than being buried in the function
  that applies it.

* **Beats that do not reach the result are counted on it, and no longer
  announced.** `window_dropped()` reads back two reasons from a
  `vectorcardiogram()` result: `incomplete_span`, beats too near an end of the
  record for the fixed window to be cut, and `no_delineation`, beats the
  annotator did not mark the wave in. The count had been reported only through
  `message()`, which goes nowhere on a background worker — across a 14,000-record
  batch not one of these notices was seen — and it fired on essentially every
  record, since a 1000 ms window overhangs at least one end of a ten-second
  strip. This is what `window_dropped()` already did for windowing strategies.

* **An undelineated beat costs that beat rather than the whole record**
  (**breaking**, for the better). `vectorcardiogram(beats = "all")` raised `No
  complete QRS wave in this beat` and abandoned the record when any single beat
  lacked a landmark; a missing wave boundary is common enough that one beat
  regularly took twelve good ones with it. Such beats are now dropped and
  counted. A record with no traceable beat at all is still an error.

## The ECG class

* **The twelve leads are ordered as they are displayed.** They were held as
  `I, II, III, AVF, AVL, AVR, V1`–`V6`, which puts the augmented limb leads in
  alphabetical order rather than the AHA/ACCF/HRS display sequence (Kligfield et
  al., 2007); it is now `I, II, III, AVR, AVL, AVF, V1`–`V6`, the order every
  ECG cart prints and every PhysioNet twelve-lead database is written in.

  Worse, the lead list was built with `factor(..., ordered = TRUE)` and no
  `levels =`, so the levels came out **alphabetical** — `AVF < AVL < AVR < I <
  II < III` — and every catheter list with them, most visibly `DD 1-2 < DD 11-12
  < DD 13-14 < … < DD 9-10`. Each was a factor that claimed to be ordered and
  ordered by nothing. Levels are now the sequence as written, for the surface
  leads and for every catheter.

  Two things follow. Plot facets are laid out in the display sequence regardless
  of what order the record stores its channels in. And `read_muse()` returns its
  columns in that sequence — the values are unchanged and each lead keeps its
  own samples, but a record converted from MUSE XML now writes its `.hea`
  channels in a different order than one converted before this change.

* **`ecg_leads()` and `lead_factor()` are new.** `ecg_leads()` returns the twelve
  leads as an ordered factor, `order = "cabrera"` giving the recognised
  alternative sequence. `lead_factor()` puts arbitrary lead labels onto that
  order, canonicalising them on the way, so `aVR`, `av r` and `AV-R` all sort as
  `AVR`:

  ```r
  features$lead <- lead_factor(features$lead)   # then sort, facet, or split
  ```

  Between them they are the one place the display order is written down. A label
  that is not a surface lead becomes `NA` rather than being dropped, so it stays
  visible in whatever it was going to be plotted or sorted into.

* **Lead order in a record still follows the record.** `as_ECG()` renames leads
  canonically but deliberately does not reorder them: annotation `channel`
  indices address signal columns by position, so permuting the columns would
  repoint every per-lead fiducial at a different lead, silently, since the
  indices stay valid. Renumbering them is not available either — whether an
  annotator counted from zero or from one is a property of the file, and channel
  `0` is also how a fiducial says it belongs to no lead in particular. Index by
  name, and order at the point of use. `?ECG` and `?as_ECG` say so.

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

* **A regular ventricular response is flagged, because cancellation cannot be
  trusted on one.** `record$rr_regular` is new, and a warning goes with it.

  What a cancellation template holds is whatever repeats at a fixed phase to the
  QRS. In fibrillation the atrial signal has no such phase, which is why the
  method works. In flutter conducting at a fixed ratio it does, so the flutter
  wave is built into the template and subtracted along with the QRST. On a
  synthetic 12-lead record carrying a 5 Hz flutter wave, the fraction surviving
  cancellation is 7% at 2:1, 13% at 3:1 and 16% at 4:1, against 80% when the same
  fixed rate puts a non-integer number of atrial cycles in each RR interval. The
  organisation index falls with it, from 0.95 uncancelled to 0.19–0.27 — the
  range fibrillation occupies. A cohort compared on `organization_index` will not
  separate flutter from fibrillation, and the failure looks like a null result.

  Nothing in the fit reports this: the template models the beat *better* for
  having absorbed the atrial wave, so `cancellation_residual` is small and
  reassuring. Regularity of the ventricular response is what reports it, and it
  is deliberately **not** silenced by `rhythm = "flutter"` — that is the case it
  exists for. `af_like` and `rr_regular` answer different questions and are now
  read separately. Both are covered by a synthetic flutter case in the test
  suite. This is a property of template subtraction, not of this implementation;
  `"average_beat"` shares it and `"adaptive_svd"` is worse. Where flutter is the
  question, read the atrial wave between QRS complexes rather than from a
  cancelled signal. See the cancellation section of `?extract_f_waves`.

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

* **`by_pwave()` is a new windowing strategy.** It cuts the atrial portion of
  each beat, from the P onset to either the QRS onset (default) or the P offset.
  Isolating the P wave is what makes atrial morphology modellable, since the QRS
  is an order of magnitude taller and otherwise absorbs the variance in any basis
  expansion fitted over a whole beat. Ending at the QRS onset keeps the
  isoelectric PR segment, which costs nothing and avoids truncating the P wave
  on the least reliably placed of the two fiducials.

  This was already expressible through `by_rhythm()`, but only because any
  `rhythm` other than `"sinus"` skipped the default-filling branch. That is now
  documented as the extension point it is, and `by_rhythm()` gains
  `reject_overlap` so the behaviour that was tied to the string `"sinus"` can be
  asked for by name.

* **Candidate beats a strategy did not return are counted on the collection.**
  Read them with the new `window_dropped()`. `by_beat()` reports
  `incomplete_span`; `by_rhythm()` reports `no_offset`, `no_reference` and
  `overlapping`. The count was previously reported only to the console, which is
  nowhere on a background worker — and the drop rate across a study is exactly
  what an audit needs.

  `by_beat()` no longer *also* announces its drops. A fixed span overhangs at
  least one end of a short strip almost every time, so the notice fired on
  essentially every record and said nothing actionable. Printing the collection
  shows the count.

* **`baseline_window()` is new.** It subtracts each window's own isoelectric
  level from every lead. Nothing else in the chain does: reading, cutting,
  padding, warping and reducing all preserve whatever DC offset the recording
  carried, and that offset is not small next to a P wave. It dominates anything
  that goes looking for variance — a principal components analysis over a set of
  median beats returns a constant vertical shift of the whole window as its
  leading components, which is invisible in a scree table and obvious the moment
  a component is plotted as a waveform.

  The isoelectric segment is named by `reference`: a fiducial (the `width`
  milliseconds before it, which anchored on a wave onset is the PR or TP
  segment), `"start"`, `"end"`, or a numeric level. The level is a median rather
  than a mean, and each lead gets its own. Correct before reducing, which also
  removes the beat-to-beat wander that would otherwise smear the median:

  ```r
  get_windows(ecg, by = by_beat(channel = 2)) |>
    baseline_window(reference = "(", channel = 2) |>
    median_window()
  ```

  `?pad_window` and `?median_window` now say in their return values that the
  offset is preserved and point here.

* **`median_window()` returns the fiducials that produced the beat**
  (**breaking**). It previously discarded them on the grounds that a median of
  many beats has no single set — but it does, in the same sense the signal does:
  each fiducial's position is the median of its positions across the aligned
  windows. Annotations are matched between windows by channel, type, and rank
  within that pair, so the first QRS onset of one beat lines up with the first of
  every other rather than with whichever bracket sorts alongside it. A fiducial
  most windows do not carry is dropped. The header gains a `median_info` string
  recording how many windows went into the beat, and loses the `window_info`
  string naming the single source window it is no longer from.

  They live where every `EGM` keeps its annotations: under `$annotation`, which
  is a *named list* of `annotation_table`s — one per annotator — and not a table.
  Read them with `get_annotation()`, which unwraps the single-annotator case.
  `nrow(beat$annotation)` is `NULL` here for the same reason it is `NULL` on a
  record straight from `read_wfdb()`, which reads as though the fiducials were
  lost; `?median_window` now says so in its return value.

* **`by_beat()` is a new windowing strategy.** It cuts the same span of signal
  around every occurrence of a fiducial, so every window is the same length by
  construction. That is what a representative beat needs: reducing ragged windows
  means padding them onto a common grid first, and a padded sample is a
  fabricated one. Cutting a fixed span out of the continuous recording leaves
  nothing to pad, which is how the standard representative beat is derived
  (Kligfield et al., 2007). Beats too near either end of the record for the full
  span are dropped rather than truncated, and how many is reported.

  `vectorcardiogram()` and `atrial_vectorcardiogram()` use it, so their median
  beat no longer contains any fabricated sample. On the bundled AF record, where
  T-offset wanders by 158 ms and only 57% of a padded grid was backed by every
  beat, the GEH components are now computable at all.

* **`median_window()` matches fiducials by wave identity and by rank counted
  outward from `align_feature`.** A fixed span reaches into the neighbouring
  beats, and which of their fiducials fall inside varies with the rate, so rank
  counted from the window start named a different fiducial in each window — on
  the AF record it placed the T onset before the QRS it belonged to, and the
  spatial QRS-T angle came back `NA`.

* **`pad_window()` pads with `NA` rather than `0`** (**breaking**). Zero is a
  fabricated observation: it states that the potential at those samples was zero,
  and it drags `median_window()` toward the origin wherever the windows do not
  all reach. On the bundled sinus record that shrank the P-loop area by 9%
  against a median built from real signal; peak-vector measures were unaffected,
  since the peak sits mid-beat where every window contributes. Beats windowed
  from raw rhythm are ragged in proportion to how much the rhythm varies — on
  the bundled AF record only 57% of the padded grid is backed by every beat.
  Pass `pad_value = 0` where a downstream step cannot carry missing values.

  Where padding can be avoided entirely, it now is: see `by_beat()`.

* **`pad_window(pad_value = "edge")` extends the nearest observed sample
  outward.** `NA` is the right default and stays it, but it means a padded
  collection cannot go straight into a matrix method, and the obvious handling is
  the wrong one: where the padded length exceeds the median wave span — as it
  must, or nothing would be padded — *every* window carries some `NA`, so
  dropping incomplete cases empties the matrix rather than trimming it. On a
  250-sample P-wave representation against a median P span of 81, that is 0 rows
  out of 7,830. Edge extension is the physiologically sensible fill for an
  isoelectric segment, so it is offered rather than left to be reinvented.
  `?pad_window` now sets out what each of the three fills claims.

  `pad_value` is also validated now. A mistyped string became `NA` through
  `as.numeric()` and passed for the default.

* **Windows cut from an `ECG` are `ECG`s.** The class was previously lost at
  extraction, so a windowed beat could not satisfy an analysis gated on it.
  `get_windows()`, `pad_window()`, `normalize_window()`, `warp_window()` and
  `median_window()` now carry it, which is what lets a beat be piped into
  `vectorcardiogram()`. Windows of a record that is not a surface ECG are
  unaffected.

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

	
