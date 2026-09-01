#' Extract fibrillatory wave features from a surface ECG
#'
#' @description `extract_f_waves()` isolates atrial activity from a surface ECG
#'   by cancelling the ventricular (QRST) contribution, then summarises the
#'   residual fibrillatory signal in each lead.
#'
#' @details
#'
#' # Cancellation
#'
#' The default `cancel_method = "spatiotemporal"` implements the *spatial* half
#' of Stridh and Sornmo (2001). A single set of QRS positions is shared by every
#' lead, and a template beat is formed for each lead by taking the *median*
#' across beats. Each individual beat, in each lead, is then fitted by least
#' squares to a linear combination of the templates from *all* leads plus an
#' intercept, and the fit is subtracted.
#'
#' The published method estimates a per-beat time shift and time scaling of the
#' template as well, which is what the *temporal* in its name refers to. That
#' step is not implemented here. Its absence shows up as a larger
#' `cancellation_residual` on records whose QRS width varies beat to beat.
#'
#' The extra degrees of freedom matter. A single-lead template cannot absorb the
#' beat-to-beat rotation of the heart's electrical axis caused by respiration and
#' by varying ventricular filling, which is the dominant reason a template fails
#' to fit its own beat. What it leaves behind is periodic at the heart rate, so
#' it deposits energy on heart-rate harmonics -- inside the very band this
#' function reads. Use [f_wave_diagnostics] to check whether that has happened.
#'
#' ## What cancellation cannot separate
#'
#' A template built by stacking beats keeps whatever repeats at a fixed phase to
#' the QRS. That is the ventricular complex, and it is also atrial activity in a
#' rhythm whose AV conduction ratio is fixed -- typical atrial flutter. The
#' method has no way to tell the two apart, so the flutter wave joins the
#' template and is subtracted with the QRST.
#'
#' The size of the effect is not marginal. On a synthetic 12-lead record
#' carrying a 5 Hz (300/min) flutter wave, the fraction of that wave surviving
#' cancellation is:
#'
#' | Ventricular response | Atrial cycles per RR | Surviving | Organisation index |
#' |---|---|---|---|
#' | 2:1, fixed | 2.00 | 7% | 0.19 |
#' | 3:1, fixed | 3.00 | 13% | 0.27 |
#' | 4:1, fixed | 4.00 | 16% | 0.21 |
#' | fixed, non-integer ratio | 2.50 | 80% | 0.94 |
#' | irregular (AF-like) | -- | 57% | 0.58 |
#'
#' The uncancelled signal scores 0.95. So a flutter record conducting at a fixed
#' ratio comes back with an organisation index in the range fibrillation
#' occupies, and a dominant rate describing whatever survived rather than the
#' flutter. **A cohort compared on `organization_index` will not separate flutter
#' from fibrillation, and the failure looks like a null result rather than an
#' artefact.**
#'
#' Nothing in the fit reports this. The template models the beat *better* for
#' having absorbed the atrial wave, so `cancellation_residual` is small and
#' reassuring. What does report it is the ventricular response being regular:
#' `record$rr_regular` is `TRUE` when RR CV is below 0.05, and a warning is
#' raised. It is deliberately not silenced by `rhythm = "flutter"`, since that is
#' the case it exists for.
#'
#' There is no cancellation method here that avoids this -- `"average_beat"`
#' shares the assumption. Where flutter is the question, read the atrial wave
#' from a segment between QRS complexes rather than from a cancelled signal.
#'
#' # Interpretation
#'
#' Every spectral feature is returned alongside `on_harmonic`. A contaminated
#' dominant frequency is not noisy, it is precise and wrong, and it is highly
#' reproducible because heart rate is highly reproducible within a patient.
#' `dominant_rate` must not be used without conditioning on `on_harmonic`.
#'
#' Ten seconds is a short record for this measurement. Even with correct
#' cancellation a material fraction of atrial fibrillation records will carry a
#' contaminated rate.
#'
#' The output is meaningful only in atrial fibrillation or flutter. In sinus
#' rhythm there is no fibrillatory wave to find, so the estimator returns
#' whatever is largest in the band. When `rhythm` is not supplied, irregularity
#' is inferred from the RR series and a warning is issued if the record does not
#' look like atrial fibrillation.
#'
#' @param object An object of class `EGM` or of subclass [ECG]. An `EGM` from an
#'   electrophysiology study is reduced to its surface leads first (see
#'   [as_ECG()]); a record with no surface leads is an error, since a
#'   fibrillatory wave cannot be read from an intracardiac channel.
#'
#' @param lead Optional. A character vector of leads to analyse. If `NULL`
#'   (default), all available surface leads are processed. Cancellation always
#'   uses every available surface lead regardless of this argument, since the
#'   spatiotemporal fit draws on all of them.
#'
#' @param qrs_loc Optional integer vector of QRS sample positions, at the
#'   record's own sampling frequency. If `NULL` (default), positions are taken
#'   from the object's annotation table when one is attached, and otherwise
#'   detected from a multi-lead composite.
#'
#' @param channel The lead whose beat annotations give the QRS positions, given
#'   as a channel number or name. Required when the annotations span more than
#'   one channel: pooled across twelve leads they report twelve times as many
#'   beats as the record holds, and every rhythm measure derived from them
#'   follows. See the channels section.
#'
#'   Supplying `qrs_loc` does not make this unnecessary. The beat positions come
#'   from `qrs_loc` then, but the TQ segments are still read from annotations,
#'   and pooled across leads they overlap and cover the record several times
#'   over. A record whose annotations span more than one channel is an error
#'   either way.
#'
#' @param cancel_method Ventricular cancellation method. Either
#'   `"spatiotemporal"` (default) or `"average_beat"`, the single-lead Slocum
#'   predecessor kept as the comparison baseline.
#'
#' @param min_beats,aberrancy_threshold Passed to
#'   [cancel_ventricular_signal()]. See there for what each is guarding.
#'
#' @param f_characteristics Character vector of features to compute. Any of
#'   `"amplitude"`, `"dominant_frequency"`, `"organization"`, `"sample_entropy"`,
#'   `"approximate_entropy"`.
#'
#' @param amplitude_window Where amplitude is measured. `"tq"` (default)
#'   restricts measurement to the TQ segments, where the ventricles are
#'   electrically silent. `"all"` uses the whole record, which confounds
#'   fibrillatory amplitude with cancellation error, since whatever cancellation
#'   fails to remove is concentrated at the QRS.
#'
#' @param normalize Amplitude normalisation. `"none"` (default) leaves
#'   `f_amplitude` as `f_amplitude_rms`, in raw signal units. `"qrs"` points it
#'   at `f_ratio` instead, which divides the peak-to-peak amplitude by the QRS
#'   excursion in the same lead. The intent is to cancel the thoracic transfer
#'   function to first order -- the ventricular signal crosses the same tissue,
#'   so it carries the same attenuation -- and so make amplitudes comparable
#'   *between* patients of different body habitus. Every amplitude column is
#'   always returned, and this argument only sets which one `f_amplitude` refers
#'   to.
#'
#'   `f_ratio` is specific to this package. Fibrillatory wave amplitude itself
#'   is well established, and Li et al. tied a coarse f-wave -- 1 mm or more in
#'   V1 -- to left atrial appendage dysfunction and thrombus. Normalising it by
#'   the QRS is not published, and the reasoning above is the whole of the case
#'   for it.
#'
#'   RMS is the default because peak-to-peak is a maximum over the segment and
#'   so grows with the segment's length: on white noise its expectation rises
#'   58% between a 20-sample and a 400-sample window. TQ segments are as long as
#'   the RR interval lets them be, which is irregular within an AF record by
#'   definition and shorter at higher rates between patients, so
#'   `f_amplitude_p2p` and `f_ratio` both carry a heart-rate confound that
#'   `f_amplitude_rms` does not.
#'
#' @param band Numeric length-2 vector giving the frequency band searched for the
#'   dominant fibrillatory frequency, in Hz. Default `c(4, 10)`. Sustained atrial
#'   fibrillation is usually quoted at 4-10 Hz. Typical atrial flutter is
#'   4-5.5 Hz, and slow or drug-modified flutter falls below 4 Hz, so widen this
#'   when flutter is expected.
#'
#' @param entropy_rate Sampling rate in Hz to which the atrial signal is
#'   decimated before entropy is computed, or `NULL` to compute at the record's
#'   own rate. Default 256.
#'
#'   Entropy is O(n^2), so this is the argument that decides how long a batch
#'   takes. It is not free to lower it: Alcaraz et al. (2010) tuned sample
#'   entropy for this exact task and found classification degraded below 256 Hz,
#'   which is why that is the default rather than a rate chosen from the
#'   fibrillatory bandwidth. A lower value returns a number that is cheaper and
#'   outside the range the parameter set was validated on.
#'
#' @param pooled_spectrum Logical. If `TRUE` (default), the dominant frequency is
#'   taken from a spectrum pooled across all analysed leads, each normalised to
#'   unit power in 2.5-15 Hz before averaging. Normalising before pooling stops
#'   one high-amplitude lead dictating the peak.
#'
#'   Pooling helps, but by much less than the lead count suggests, and it is
#'   worth knowing by how much. Four of the twelve leads are exact linear
#'   combinations of I and II, so at most eight can be independent; and the
#'   atrial signal is close to a single dipole, so fewer are. On the bundled
#'   `muse-af` record the correlation matrix of the twelve cancelled leads has a
#'   participation ratio of 1.7, making the variance reduction about 1.3-fold
#'   rather than the 3.5-fold that twelve independent leads would give.
#'
#' @param rhythm Optional character string naming the rhythm, e.g. `"af"`,
#'   `"flutter"`, `"sinus"`. Used only to decide whether to warn. If `NULL`
#'   (default), rhythm is inferred from RR irregularity.
#'
#' @param tol Numeric tolerance for the harmonic test. A peak is flagged
#'   `on_harmonic` when its harmonic index falls within `tol` of an integer.
#'   Default 0.15.
#'
#'   This is a sensitive screen and not a specific test, and the difference is
#'   large enough to plan around. The harmonic index of an uncontaminated peak
#'   is a ratio of two unrelated numbers, so with integers spaced one apart a
#'   tolerance of 0.15 lands on one 30% of the time by arithmetic alone --
#'   simulated at 0.301 for a dominant frequency uniform on 4-10 Hz and a heart
#'   rate uniform on 60-160 bpm, and flat across rate. Excluding every flagged
#'   record therefore discards roughly a third of the good ones too. Read it
#'   with `cancellation_residual`, which says whether there was residual for the
#'   peak to have come from.
#'
#' @param keep_signal Logical. If `TRUE`, the cancelled atrial signals are
#'   returned alongside the features. Default `FALSE`, since retaining them
#'   across a large batch is expensive.
#'
#' @param verbose Logical. If `TRUE` (default), report which leads are analysed.
#'   This controls the progress message only. The rhythm warnings are raised
#'   either way: batch processing is where `verbose = FALSE` gets set, and it is
#'   also where an unnoticed flutter record does the most damage.
#'
#' @inheritSection channels Guiding channel
#'
#' @return An object of class `f_wave_analysis`, a list with:
#'
#'   \describe{
#'     \item{`features`}{A `data.table` with one row per lead, holding
#'       `f_amplitude_p2p`, `f_amplitude_rms`, `qrs_amplitude`, `f_ratio`,
#'       `dominant_rate`, `organization_index`, `sample_entropy`, and the
#'       per-lead diagnostics `harmonic_overlap`, `on_harmonic`,
#'       `cancellation_residual`, and `tq_fraction`.}
#'     \item{`record`}{A one-row `data.table` of record-level values, including
#'       `n_beats_cancelled`, `n_beats_skipped`, `n_beats_aberrant`,
#'       `heart_rate`, `rr_cv`, `af_like`, and
#'       `rr_regular` - the flag that the cancellation may have taken the atrial
#'       signal with it, described in the cancellation section.}
#'     \item{`signal`}{The cancelled atrial signals, when `keep_signal = TRUE`.}
#'   }
#'
#' @references
#'
#' Stridh M, Sornmo L. Spatiotemporal QRST cancellation techniques for analysis
#' of atrial fibrillation. *IEEE Transactions on Biomedical Engineering*.
#' 2001;48(1):105-111. \doi{10.1109/10.900266}
#'
#' Slocum J, Sahakian A, Swiryn S. Diagnosis of atrial fibrillation from surface
#' electrocardiograms based on computer-detected atrial activity. *Journal of
#' Electrocardiology*. 1992;25(1):1-8. \doi{10.1016/0022-0736(92)90123-h}
#'
#' Bollmann A, Husser D, Mainardi L, et al. Analysis of surface
#' electrocardiograms in atrial fibrillation: techniques, research, and clinical
#' applications. *Europace*. 2006;8(11):911-926. \doi{10.1093/europace/eul113}
#'
#' Ng J, Goldberger JJ. Understanding and interpreting dominant frequency
#' analysis of AF electrograms. *Journal of Cardiovascular Electrophysiology*.
#' 2007;18(6):680-685. \doi{10.1111/j.1540-8167.2007.00832.x}
#'
#' Lankveld TAR, Zeemering S, Crijns HJGM, Schotten U. The ECG as a tool to
#' determine atrial fibrillation complexity. *Heart*. 2014;100(14):1077-1084.
#' \doi{10.1136/heartjnl-2013-305149}
#'
#' Alcaraz R, Abasolo D, Hornero R, Rieta JJ. Optimized assessment of atrial
#' fibrillation organization through suitable parameters of sample entropy.
#' *Annual International Conference of the IEEE Engineering in Medicine and
#' Biology Society*. 2010;2010:118-121. \doi{10.1109/IEMBS.2010.5627169}
#'
#' @seealso [cancel_ventricular_signal()], [calculate_dominant_frequency()],
#'   [calculate_sample_entropy()], [calculate_organization_index()]
#'
#' @examples
#' \dontrun{
#' af <- read_wfdb("muse-af", system.file("extdata", package = "EGM"))
#' res <- extract_f_waves(af)
#'
#' # Never read the rate without the diagnostics beside it
#' res$features[, .(lead, dominant_rate, on_harmonic, cancellation_residual)]
#' }
#'
#' @export
extract_f_waves <- function(
  object,
  lead = NULL,
  qrs_loc = NULL,
  channel = NULL,
  cancel_method = c("spatiotemporal", "average_beat"),
  f_characteristics = c(
    "amplitude",
    "dominant_frequency",
    "organization",
    "sample_entropy"
  ),
  amplitude_window = c("tq", "all"),
  normalize = c("none", "qrs"),
  band = c(4, 10),
  entropy_rate = 256,
  pooled_spectrum = TRUE,
  rhythm = NULL,
  tol = 0.15,
  min_beats = 3L,
  aberrancy_threshold = 0.9,
  keep_signal = FALSE,
  verbose = TRUE
) {
  # Surface leads only, and at least one of them. Cancellation and every feature
  # below assume a body-surface potential; on an intracardiac channel they return
  # a number rather than an error, which is the failure worth preventing.
  object <- require_ECG(object, what = "Fibrillatory wave extraction")

  channel <- valid_channel(channel)
  cancel_method <- match.arg(cancel_method)
  amplitude_window <- match.arg(amplitude_window)
  normalize <- match.arg(normalize)

  valid_characteristics <- c(
    "amplitude",
    "dominant_frequency",
    "organization",
    "sample_entropy",
    "approximate_entropy"
  )
  if (!all(f_characteristics %in% valid_characteristics)) {
    stop(
      "Invalid characteristic specified. Choose from: ",
      paste(valid_characteristics, collapse = ", ")
    )
  }

  band <- as.numeric(band)
  if (length(band) != 2 || anyNA(band) || band[1] >= band[2] || band[1] < 0) {
    stop("`band` must be an increasing, non-negative numeric pair, e.g. c(4, 10)")
  }

  frequency <- stats::frequency(object)

  if (band[2] >= frequency / 2) {
    stop(
      "`band` upper bound (",
      band[2],
      " Hz) must be below the Nyquist frequency (",
      frequency / 2,
      " Hz)"
    )
  }

  # Leads that carry the cancellation. The spatiotemporal fit draws on every
  # surface lead available, which is separate from the leads the caller wants
  # features for.
  cancel_leads <- names(object$signal)[-1]

  if (!is.null(lead)) {
    # Requested leads are resolved canonically, so "aVR" and "AVR" both name the
    # column that `require_ECG()` produced.
    requested <- surface_leads(lead)
    unusable <- setdiff(lead, unname(requested))
    if (length(unusable) > 0) {
      stop(
        "Not a surface ECG lead: ",
        paste(unusable, collapse = ", "),
        ". Fibrillatory waves cannot be read from an intracardiac channel."
      )
    }
    missing_leads <- setdiff(names(requested), cancel_leads)
    if (length(missing_leads) > 0) {
      stop(
        "Specified lead not found in the signal data: ",
        paste(missing_leads, collapse = ", ")
      )
    }
    report_leads <- names(requested)
  } else {
    report_leads <- cancel_leads
  }

  if (verbose) {
    message(
      "Cancelling ventricular signal across ",
      length(cancel_leads),
      " leads (",
      cancel_method,
      "); reporting features for ",
      length(report_leads),
      ": ",
      paste(report_leads, collapse = ", ")
    )
  }

  # Bandpass at the record's own rate. The analysis band is at or below 15 Hz,
  # so upsampling adds no information while quadrupling the cost of entropy.
  raw <- lapply(cancel_leads, function(l) as.numeric(object$signal[[l]]))
  names(raw) <- cancel_leads
  filtered <- lapply(raw, filter_bandpass, frequency = frequency)

  # One annotation table for the whole analysis, resolved to a single lead
  # before anything reads it. Beat positions and TQ segments both come from
  # annotations and both have to come from the *same* lead: a per-lead annotator
  # writes a complete set of fiducials for each of them, so a pooled table holds
  # twelve records' worth of beats and twelve overlapping copies of every TQ
  # segment. That used to be guarded here and not in `tq_segments()`.
  #
  # Only where something will actually read it, though: with `qrs_loc` given and
  # amplitude measured over the whole record, nothing consults the annotations
  # and demanding a `channel` for them would be a guard on nothing.
  needs_annotation <- is.null(qrs_loc) ||
    (amplitude_window == "tq" && "amplitude" %in% f_characteristics)
  ann <- if (needs_annotation) resolve_fwave_annotation(object, channel) else NULL

  # A single QRS set shared by every lead
  if (is.null(qrs_loc)) {
    qrs_loc <- shared_qrs_positions(ann, filtered, frequency)
  }
  qrs_loc <- sort(unique(as.integer(qrs_loc)))
  qrs_loc <- qrs_loc[qrs_loc >= 1 & qrs_loc <= length(filtered[[1]])]

  rhythm_info <- rhythm_summary(qrs_loc, frequency, rhythm)

  # Two ways a record defeats these features, and they need saying apart. An
  # organised rhythm has no fibrillatory activity to measure. A *regular* one
  # additionally defeats the cancellation, which is the trap: the fit looks good,
  # `cancellation_residual` is small, and the numbers are wrong anyway.
  # Deliberately not gated on `verbose`, which reports progress. A batch run
  # sets `verbose = FALSE`, and a batch run is exactly where an unnoticed
  # flutter record does the most damage.
  if (!isTRUE(rhythm_info$af_like)) {
    warning(
      "This record does not look like atrial fibrillation (RR CV = ",
      signif(rhythm_info$rr_cv, 2),
      "). Fibrillatory features are only meaningful in AF or flutter; ",
      "in sinus rhythm the estimator returns whatever is largest in the band.",
      call. = FALSE
    )
  }
  if (isTRUE(rhythm_info$rr_regular)) {
    warning(
      "The ventricular response is regular (RR CV = ",
      signif(rhythm_info$rr_cv, 2),
      "), so atrial activity holding a fixed phase to the QRS - flutter ",
      "conducting at a fixed ratio - enters the cancellation template and is ",
      "subtracted with the ventricular complex. `organization_index` then ",
      "collapses toward the fibrillation range and `dominant_rate` describes ",
      "whatever survived. See the cancellation section of ?extract_f_waves.",
      call. = FALSE
    )
  }

  cancelled <- cancel_ventricular_signal(
    filtered,
    frequency = frequency,
    qrs_loc = qrs_loc,
    method = cancel_method,
    min_beats = min_beats,
    aberrancy_threshold = aberrancy_threshold
  )

  atrial <- cancelled$atrial
  qrs_loc <- cancelled$qrs_loc

  features <- lapply(report_leads, function(l) {
    analyze_atrial_signal(
      atrial_signal = atrial[[l]],
      frequency = frequency,
      characteristics = f_characteristics,
      original_signal = filtered[[l]],
      raw_signal = raw[[l]],
      qrs_loc = qrs_loc,
      annotation = ann,
      amplitude_window = amplitude_window,
      band = band,
      entropy_rate = entropy_rate,
      tol = tol
    )
  })
  names(features) <- report_leads

  # The dominant frequency is estimated once for the record and shared. Pooling
  # buys less than the lead count suggests -- the twelve leads carry about 1.7
  # independent signals between them -- but a single lead on a ten second record
  # gives only four Welch segments, so it is still worth having.
  if (pooled_spectrum && "dominant_frequency" %in% f_characteristics) {
    pooled <- pooled_dominant_frequency(
      atrial[report_leads],
      frequency = frequency,
      band = band
    )
    median_rr <- rhythm_info$median_rr
    for (l in report_leads) {
      features[[l]]$dominant_rate <- pooled$dominant_frequency * 60
      features[[l]]$harmonic_overlap <- pooled$dominant_frequency *
        median_rr /
        frequency
      features[[l]]$on_harmonic <- harmonic_flag(
        features[[l]]$harmonic_overlap,
        tol
      )
      if ("organization" %in% f_characteristics) {
        features[[l]]$organization_index <- calculate_organization_index(
          atrial[[l]],
          frequency = frequency,
          dominant_frequency = pooled$dominant_frequency
        )
      }
    }
  }

  feature_table <- assemble_feature_table(features, report_leads, normalize)

  record_table <- data.table::data.table(
    n_leads = length(report_leads),
    frequency = frequency,
    n_beats = length(qrs_loc),
    n_beats_cancelled = cancelled$n_beats_cancelled,
    n_beats_skipped = cancelled$n_beats_skipped,
    n_beats_aberrant = cancelled$n_beats_aberrant,
    cancel_method = cancel_method,
    heart_rate = rhythm_info$heart_rate,
    rr_cv = rhythm_info$rr_cv,
    rr_rmssd = rhythm_info$rr_rmssd,
    af_like = rhythm_info$af_like,
    rr_regular = rhythm_info$rr_regular,
    rhythm = rhythm_info$rhythm
  )

  out <- list(
    features = feature_table,
    record = record_table,
    signal = if (keep_signal) atrial[report_leads] else NULL
  )
  class(out) <- c("f_wave_analysis", "list")
  out
}

#' @export
print.f_wave_analysis <- function(x, ...) {
  cat("<f_wave_analysis>\n")
  if (nrow(x$record) == 0) {
    cat("  no leads analysed\n")
    return(invisible(x))
  }
  cat(
    "  ",
    x$record$n_leads,
    " leads, ",
    x$record$n_beats,
    " beats, ",
    x$record$frequency,
    " Hz, ",
    x$record$cancel_method,
    " cancellation\n",
    sep = ""
  )
  cat(
    "  heart rate ",
    round(x$record$heart_rate),
    " bpm, RR CV ",
    signif(x$record$rr_cv, 2),
    if (isTRUE(x$record$af_like)) " (AF-like)" else " (NOT AF-like)",
    "\n",
    sep = ""
  )
  if (isTRUE(x$record$rr_regular)) {
    cat(
      "  regular ventricular response -- cancellation may have removed atrial",
      " activity locked to the QRS\n",
      sep = ""
    )
  }
  if (!is.null(x$features$on_harmonic)) {
    n_contaminated <- sum(x$features$on_harmonic, na.rm = TRUE)
    cat(
      "  ",
      n_contaminated,
      "/",
      nrow(x$features),
      " leads flagged on_harmonic",
      if (n_contaminated > 0) " -- dominant_rate is unreliable there" else "",
      "\n",
      sep = ""
    )
  }
  print(x$features)
  invisible(x)
}

#' Apply bandpass filter (0.5-30 Hz by default)
#'
#' @param signal Numeric vector
#' @param frequency Sampling frequency in Hz
#' @param low,high Passband edges in Hz
#'
#' @noRd
filter_bandpass <- function(signal, frequency, low = 0.5, high = 30) {
  nyquist_freq <- frequency / 2

  # A record sampled slowly enough that the nominal passband reaches Nyquist
  # would otherwise produce an invalid filter
  high <- min(high, nyquist_freq * 0.95)
  if (low >= high) {
    stop(
      "Bandpass edges are invalid for a ",
      frequency,
      " Hz record: low = ",
      low,
      ", high = ",
      high
    )
  }

  bf <- signal::butter(3, c(low / nyquist_freq, high / nyquist_freq), type = "pass")
  as.numeric(signal::filtfilt(bf, signal))
}

# QRS positions ----

#' Derive one set of QRS positions for the whole record
#'
#' Detecting independently in each lead puts every lead's beat stack on its own
#' grid; on a 12-lead record the positions can disagree by tens of milliseconds
#' and even on the beat count. The spatiotemporal fit needs a single shared set,
#' so annotations are preferred and detection otherwise runs on a multi-lead
#' composite.
#'
#' @noRd
shared_qrs_positions <- function(ann, filtered, frequency) {
  if (!is.null(ann)) {
    # Beat labels in the WFDB convention: N, L, R, B, A, a, J, S, V, r, F, e,
    # j, n, E, P, f, Q are all beat annotations
    beat_types <- c(
      "N", "L", "R", "B", "A", "a", "J", "S", "V",
      "r", "F", "e", "j", "n", "E", "P", "f", "Q"
    )
    beats <- ann$sample[ann$type %in% beat_types]
    if (length(beats) >= 3) {
      return(as.integer(beats))
    }
  }

  # Composite detection signal. Each lead is scaled to unit variance first so
  # that one high-amplitude lead does not decide where the beats are.
  scaled <- lapply(filtered, function(x) {
    s <- stats::sd(x)
    if (!is.finite(s) || s == 0) rep(0, length(x)) else x / s
  })
  composite <- sqrt(Reduce(`+`, lapply(scaled, function(x) x^2)) / length(scaled))

  loc <- detect_QRS(composite, frequency)
  refine_qrs_positions(loc, composite, frequency)
}

#' Settle on one annotation table for the whole analysis
#'
#' Both the QRS positions and the TQ segments are read from annotations, and
#' both have to be read from the same lead. An annotator run once per lead
#' writes a complete set of fiducials for each, separated only by the `channel`
#' column, so a pooled table reports twelve times as many beats as the record
#' holds and twelve overlapping copies of every TQ segment -- the latter shows
#' up as a `tq_fraction` greater than one.
#'
#' @noRd
resolve_fwave_annotation <- function(object, channel = NULL) {
  ann <- resolve_annotation(object$annotation)
  if (is.null(ann)) {
    return(NULL)
  }

  channel <- resolve_annotation_channel(
    ann,
    resolve_channel_spec(object, channel),
    what = "Fibrillatory wave extraction"
  )
  if (!is.null(channel) && "channel" %in% colnames(ann)) {
    # Channel 0 rides along as the global channel, but only where the table
    # means it that way. A table declared `channel_zero = "signal"` numbers its
    # leads `0 .. nsig-1`, so 0 is a lead like any other and keeping it pools
    # two leads' fiducials -- the doubling this guard exists to prevent.
    keep <- as.integer(channel)
    if (!identical(channel_zero(ann), "signal")) {
      keep <- c(keep, 0L)
    }
    ann <- ann[ann$channel %in% keep, , drop = FALSE]
  }

  ann
}

#' Pull a usable annotation table out of whatever shape is attached
#'
#' An `EGM` object carries annotations as a named list keyed by annotator, but a
#' bare `annotation_table` is also accepted. Several usable annotators is an
#' error rather than a choice made here: the file-per-lead convention leaves
#' `chan` at 0 in every file, so the tables are indistinguishable by channel and
#' picking one silently picks a lead.
#'
#' @noRd
resolve_annotation <- function(ann) {
  if (is.null(ann) || length(ann) == 0) {
    return(NULL)
  }

  usable <- function(x) {
    is.data.frame(x) &&
      nrow(x) > 0 &&
      all(c("sample", "type") %in% names(x))
  }

  if (usable(ann)) {
    return(as.data.frame(ann))
  }

  if (!is.list(ann)) {
    return(NULL)
  }

  candidates <- Filter(usable, ann)
  if (length(candidates) == 0) {
    return(NULL)
  }

  # Several annotators is the other per-lead convention, and it is the one the
  # `channel` column cannot catch: a file-per-lead annotator such as LUDB's
  # carries the lead in the file extension and leaves `chan` at 0 throughout,
  # so every table looks global and picking one silently picks a lead. Which
  # one changes the beat positions, the TQ boundaries and every amplitude.
  if (length(candidates) > 1) {
    stop(
      "This record carries ", length(candidates), " usable annotators (",
      paste(names(candidates), collapse = ", "),
      "), and which one is read decides the beat positions and the TQ ",
      "segments. Read the record with the single annotator you mean, or ",
      "supply `qrs_loc` and an annotation-free `amplitude_window`.",
      call. = FALSE
    )
  }

  as.data.frame(candidates[[1]])
}

#' Snap detected positions onto the local energy maximum
#'
#' Pan-Tompkins reports the peak of the integration window, which lags the true
#' R peak by roughly half that window. Left uncorrected, the misalignment smears
#' the template across beats and weakens every subsequent fit.
#'
#' `search_ms` has to exceed that lag or the true peak is outside the window and
#' the refinement latches onto whatever else is in range. The integration window
#' is 150 ms, so the lag is about 75 ms; 100 ms leaves margin without reaching a
#' neighbouring QRS at any plausible rate.
#'
#' @noRd
refine_qrs_positions <- function(qrs_loc, reference, frequency, search_ms = 100) {
  if (length(qrs_loc) == 0) {
    return(integer(0))
  }
  half <- max(1L, as.integer(round(search_ms * frequency / 1000)))
  n <- length(reference)

  refined <- vapply(
    qrs_loc,
    function(p) {
      lo <- max(1L, as.integer(p) - half)
      hi <- min(n, as.integer(p) + half)
      lo + which.max(abs(reference[lo:hi])) - 1L
    },
    integer(1)
  )

  sort(unique(refined))
}

#' Summarise rhythm regularity from the RR series
#' @noRd
rhythm_summary <- function(qrs_loc, frequency, rhythm = NULL) {
  rr <- diff(qrs_loc)

  if (length(rr) < 2) {
    return(list(
      median_rr = NA_real_,
      heart_rate = NA_real_,
      rr_cv = NA_real_,
      rr_rmssd = NA_real_,
      af_like = NA,
      rr_regular = NA,
      rhythm = if (is.null(rhythm)) NA_character_ else rhythm
    ))
  }

  median_rr <- stats::median(rr)
  rr_cv <- stats::sd(rr) / mean(rr)
  rr_rmssd <- sqrt(mean(diff(rr)^2)) / mean(rr)
  heart_rate <- 60 * frequency / median_rr

  # A rate outside these bounds is not a rhythm, it is a counting error - most
  # often beat annotations pooled across leads, or a sampling frequency that is
  # not the one the positions were measured at. Neither is visible in any
  # feature the caller reads afterwards, so it is said here.
  if (is.finite(heart_rate) && (heart_rate < 20 || heart_rate > 300)) {
    warning(
      "Implausible heart rate (",
      signif(heart_rate, 4),
      " bpm) from ",
      length(qrs_loc),
      " QRS positions at ",
      frequency,
      " Hz. Check that the beat annotations belong to one lead - pooled across ",
      "a per-lead annotator they count every beat once per lead - and that the ",
      "positions are at the record's own sampling frequency."
    )
  }

  af_like <- if (!is.null(rhythm)) {
    tolower(rhythm) %in% c("af", "afib", "atrial fibrillation", "flutter", "aflutter")
  } else {
    # Irregularly irregular ventricular response. The coefficient of variation
    # of the RR interval is Tateno and Glass's atrial fibrillation detector
    # (Med Biol Eng Comput. 2001;39(6):664-671), which they report at 86.6%
    # sensitivity and 84.3% specificity; the normalised RMSSD is the companion
    # index used through the RR-irregularity detection literature.
    #
    # These three thresholds -- 0.12 and 0.10 here, 0.05 for `rr_regular` below
    # -- are not from either. They were chosen here, deliberately permissive, to
    # decide whether to warn rather than to diagnose anything.
    isTRUE(rr_cv >= 0.12 && rr_rmssd >= 0.10)
  }

  # Read separately from `af_like`, and never overridden by the `rhythm` label,
  # because it answers a different question: not "is there fibrillatory activity
  # to measure" but "will the canceller leave it there". Regular RR means atrial
  # activity can hold a fixed phase to the QRS, and a template built by
  # stacking beats absorbs anything that does. Labelling a record "flutter" is
  # precisely when this bites, so it must not be what silences it.
  rr_regular <- isTRUE(rr_cv < 0.05)

  list(
    median_rr = median_rr,
    heart_rate = heart_rate,
    rr_cv = rr_cv,
    rr_rmssd = rr_rmssd,
    af_like = af_like,
    rr_regular = rr_regular,
    rhythm = if (is.null(rhythm)) NA_character_ else rhythm
  )
}

# Cancellation ----

#' Cancel the ventricular contribution from a multi-lead ECG
#'
#' @description Removes the QRST complex from each lead so that the atrial
#'   signal can be measured. Three methods are available.
#'
#' @details
#'
#' `"spatiotemporal"` fits each beat, in each lead, to a linear combination of
#' the median beat template from *every* lead plus an intercept, and subtracts
#' the fit (Stridh and Sornmo, 2001). The cross-lead terms give the model enough
#' freedom to absorb beat-to-beat rotation of the electrical axis, which a
#' single-lead template cannot represent.
#'
#' `"average_beat"` subtracts each lead's own median template, scaled by a single
#' least-squares coefficient. This is the Slocum average-beat predecessor and is
#' provided for comparison.
#'
#' Beats are never blanked or interpolated across. A beat that the template
#' models badly is still fitted and subtracted, and the quality of that fit is
#' reported through `cancellation_residual` rather than being hidden by
#' replacing the segment with a straight line.
#'
#' All three methods share one assumption, and it is worth stating plainly: what
#' the template holds is whatever repeats at a fixed phase to the QRS. In atrial
#' fibrillation the atrial signal has no such phase, which is why the method
#' works. In atrial flutter conducting at a fixed ratio it does, so the flutter
#' wave is built into the template and subtracted along with the QRST -- around
#' 90% of it, on a synthetic 2:1 record. The fit is *better* for having absorbed
#' it, so `cancellation_residual` does not report the loss. See the cancellation
#' section of [extract_f_waves()], which flags the condition through
#' `record$rr_regular`.
#'
#' @param signals A named list of numeric vectors, one per lead, already
#'   bandpass filtered and all of the same length.
#' @param frequency Sampling frequency in Hz. Required.
#' @param qrs_loc Integer vector of QRS sample positions shared by all leads.
#'   Detected from a composite if `NULL`.
#' @param method Either `"spatiotemporal"` or `"average_beat"`.
#' @param min_beats Minimum number of beats required before any template-based
#'   subtraction is attempted. Default 3. Below this an "average" beat is
#'   effectively a copy of a single beat, and subtracting it returns zero.
#' @param aberrancy_threshold Correlation below which a beat is called
#'   morphologically aberrant and excluded from template construction. Default
#'   0.9. Aberrancy is judged on QRS morphology, never on RR interval: in atrial
#'   fibrillation the RR interval is irregular by definition, so an RR-based
#'   criterion fires on normally conducted beats in the exact rhythm this
#'   function targets.
#'
#' @return A list with `atrial` (named list of cancelled signals), `qrs_loc`,
#'   `n_beats_cancelled`, `n_beats_skipped`, and `n_beats_aberrant`.
#'
#' @references
#' Stridh M, Sornmo L. Spatiotemporal QRST cancellation techniques for analysis
#' of atrial fibrillation. *IEEE Transactions on Biomedical Engineering*.
#' 2001;48(1):105-111. \doi{10.1109/10.900266}
#'
#' @export
cancel_ventricular_signal <- function(
  signals,
  frequency,
  qrs_loc = NULL,
  method = c("spatiotemporal", "average_beat"),
  min_beats = 3L,
  aberrancy_threshold = 0.9
) {
  method <- match.arg(method)

  if (missing(frequency) || is.null(frequency)) {
    stop("`frequency` is required and must be the signal's sampling rate in Hz")
  }

  if (is.numeric(signals)) {
    signals <- list(signal = as.numeric(signals))
  }
  if (is.null(names(signals))) {
    names(signals) <- paste0("lead", seq_along(signals))
  }

  lengths_ok <- length(unique(vapply(signals, length, integer(1)))) == 1L
  if (!lengths_ok) {
    stop("All leads must be the same length")
  }

  n <- length(signals[[1]])

  if (is.null(qrs_loc)) {
    scaled <- lapply(signals, function(x) {
      s <- stats::sd(x)
      if (!is.finite(s) || s == 0) rep(0, length(x)) else x / s
    })
    composite <- sqrt(Reduce(`+`, lapply(scaled, function(x) x^2)) / length(scaled))
    qrs_loc <- refine_qrs_positions(
      detect_QRS(composite, frequency),
      composite,
      frequency
    )
  }
  qrs_loc <- sort(unique(as.integer(qrs_loc)))

  if (length(qrs_loc) < min_beats) {
    warning(
      "Only ",
      length(qrs_loc),
      " beats detected; ",
      min_beats,
      " are required for cancellation. Returning the original signals."
    )
    return(list(
      atrial = signals,
      qrs_loc = qrs_loc,
      n_beats_cancelled = 0L,
      n_beats_skipped = length(qrs_loc),
      n_beats_aberrant = NA_integer_
    ))
  }

  # Beat window. Wide enough to hold the whole QRST, but bounded so that a slow
  # rhythm does not produce an absurdly long template.
  rr <- diff(qrs_loc)
  median_rr <- if (length(rr) > 0) stats::median(rr) else frequency
  pre <- as.integer(round(0.20 * frequency))
  post <- as.integer(round(min(
    0.50 * frequency,
    max(0.25 * frequency, 0.65 * median_rr)
  )))
  width <- pre + post + 1L

  if (width >= n) {
    warning("Record is too short to build a beat window. Returning originals.")
    return(list(
      atrial = signals,
      qrs_loc = qrs_loc,
      n_beats_cancelled = 0L,
      n_beats_skipped = length(qrs_loc),
      n_beats_aberrant = NA_integer_
    ))
  }

  # Only beats whose full window lies inside the record can be stacked. The rest
  # are counted as skipped rather than being padded with zeros, which would
  # otherwise drag the template toward baseline.
  usable <- qrs_loc[(qrs_loc - pre) >= 1L & (qrs_loc + post) <= n]
  n_skipped <- length(qrs_loc) - length(usable)

  if (length(usable) < min_beats) {
    warning(
      "Only ",
      length(usable),
      " beats have a complete window; ",
      min_beats,
      " are required. Returning the original signals."
    )
    return(list(
      atrial = signals,
      qrs_loc = qrs_loc,
      n_beats_cancelled = 0L,
      n_beats_skipped = length(qrs_loc),
      n_beats_aberrant = NA_integer_
    ))
  }

  lead_names <- names(signals)
  windows <- lapply(usable, function(p) seq.int(p - pre, p + post))

  # Beat stack: for each lead, a width x n_beats matrix
  stacks <- lapply(signals, function(x) {
    vapply(windows, function(w) x[w], numeric(width))
  })

  # Row-wise median across beats, per lead. Median rather than mean so that one
  # ectopic or noisy beat does not deform the template that every other beat is
  # then fitted against.
  templates <- vapply(
    stacks,
    function(m) apply(m, 1, stats::median),
    numeric(width)
  )
  templates <- matrix(templates, nrow = width, dimnames = list(NULL, lead_names))

  # Morphology-based aberrancy, judged on the QRS proper rather than the whole
  # window, and pooled across leads.
  qrs_rows <- seq.int(
    max(1L, pre + 1L - as.integer(round(0.06 * frequency))),
    min(width, pre + 1L + as.integer(round(0.06 * frequency)))
  )
  beat_corr <- vapply(
    seq_along(usable),
    function(i) {
      cors <- vapply(
        lead_names,
        function(l) {
          a <- stacks[[l]][qrs_rows, i]
          b <- templates[qrs_rows, l]
          if (stats::sd(a) == 0 || stats::sd(b) == 0) {
            return(NA_real_)
          }
          stats::cor(a, b)
        },
        numeric(1)
      )
      stats::median(cors, na.rm = TRUE)
    },
    numeric(1)
  )
  aberrant <- !is.na(beat_corr) & beat_corr < aberrancy_threshold
  n_aberrant <- sum(aberrant)

  # Rebuild the template from morphologically normal beats only, provided
  # enough of them remain.
  if (n_aberrant > 0 && sum(!aberrant) >= min_beats) {
    templates <- vapply(
      stacks,
      function(m) apply(m[, !aberrant, drop = FALSE], 1, stats::median),
      numeric(width)
    )
    templates <- matrix(templates, nrow = width, dimnames = list(NULL, lead_names))
  }

  if (method == "average_beat") {
    # Each lead sees only its own template, which is the point of the
    # comparison: no cross-lead degrees of freedom.
    design_for <- function(l) cbind(1, templates[, l, drop = FALSE])
  } else {
    design_for <- function(l) cbind(1, templates)
  }

  # Guard against rank deficiency. Derived-lead exports and flat leads make the
  # template columns linearly dependent, and a record should degrade to the
  # well-conditioned columns rather than fail outright.
  qr_cache <- list()
  for (l in lead_names) {
    X <- design_for(l)
    qx <- qr(X)
    if (qx$rank < ncol(X)) {
      keep <- sort(qx$pivot[seq_len(qx$rank)])
      X <- X[, keep, drop = FALSE]
      qx <- qr(X)
    }
    qr_cache[[l]] <- qx
  }

  # The ventricular estimate is accumulated into a full-length buffer with a
  # tapered weight. When RR is short the windows of consecutive beats overlap,
  # and accumulating this way subtracts the overlap once with a smooth crossfade
  # instead of subtracting it twice or leaving a step discontinuity.
  taper <- tukey_window(width, ramp = 0.10)
  # `pmax(weight, 1)` and not `weight`: dividing by the weight itself cancels
  # the taper exactly wherever only one window covers a sample, so subtraction
  # would step from the full fitted value to zero at the outer edge of every
  # covered region. On the bundled `muse-af` record subtraction switched on at 3
  # to 9 percent of the ventricular estimate's own peak -- a median of 46
  # digital units, against 0.3 with the taper kept -- once per beat.
  #
  # What that costs is not measurable here: the harmonic share of 2.5-15 Hz
  # atrial power moves by 0.0005 on `muse-af`, and by as little on a synthetic
  # record slow enough that no two beat windows overlap. The step is removed
  # because a discontinuity the method never intended is not something to leave
  # in a signal that is about to be read spectrally, not because any returned
  # feature was seen to move.
  #
  # The cost is that the outermost tenth of an unoverlapped window has only
  # part of its ventricular estimate removed. That is the far tail of the beat,
  # 200 ms before the R peak and 500 ms after it, where the QRST is near
  # baseline anyway -- and a ramp there is a smaller error than a step.

  ventricular <- lapply(lead_names, function(l) numeric(n))
  names(ventricular) <- lead_names
  weight <- numeric(n)

  for (i in seq_along(usable)) {
    w <- windows[[i]]
    for (l in lead_names) {
      y <- stacks[[l]][, i]
      fitted <- qr.fitted(qr_cache[[l]], y)
      ventricular[[l]][w] <- ventricular[[l]][w] + taper * fitted
    }
    weight[w] <- weight[w] + taper
  }

  scale <- pmax(weight, 1)
  atrial <- lapply(lead_names, function(l) {
    signals[[l]] - ventricular[[l]] / scale
  })
  names(atrial) <- lead_names

  list(
    atrial = atrial,
    qrs_loc = qrs_loc,
    n_beats_cancelled = length(usable),
    n_beats_skipped = n_skipped,
    n_beats_aberrant = as.integer(n_aberrant)
  )
}

#' Tukey (tapered cosine) window
#' @noRd
tukey_window <- function(n, ramp = 0.1) {
  if (n < 3) {
    return(rep(1, n))
  }
  w <- rep(1, n)
  k <- max(1L, as.integer(floor(ramp * n)))
  if (k >= 1) {
    ramp_up <- 0.5 * (1 - cos(pi * seq_len(k) / (k + 1)))
    w[seq_len(k)] <- ramp_up
    w[seq.int(n - k + 1, n)] <- rev(ramp_up)
  }
  w
}

# Atrial signal analysis ----

#' Analyse an isolated atrial signal
#'
#' @param atrial_signal Numeric vector of the cancelled (atrial) signal
#' @param frequency Sampling frequency in Hz
#' @param characteristics Character vector of features to compute
#' @param original_signal The uncancelled, filtered signal for the same lead,
#'   which is what the cancellation residual and the harmonic evidence are
#'   measured against.
#' @param raw_signal The same lead before bandpass filtering, used for the QRS
#'   excursion. Falls back to `original_signal`. The passband reaches only
#'   30 Hz, and the QRS carries energy above it, so a filtered excursion is
#'   small by 4-37% across the leads of the bundled records, median 14%, and by
#'   how much depends on the lead and the QRS width -- which would put a
#'   morphology-dependent factor into `f_ratio`, the one amplitude measure whose
#'   whole purpose is comparability between patients.
#' @param qrs_loc Integer vector of QRS positions
#' @param annotation Optional annotation table used to locate TQ segments. It
#'   must already be resolved to a single lead; see [extract_f_waves()].
#' @param amplitude_window Either `"tq"` or `"all"`
#' @param band Numeric length-2 frequency band in Hz
#' @param entropy_rate Rate in Hz to decimate to before computing entropy
#' @param tol Tolerance for the harmonic coincidence test
#'
#' @return A named list of features
#' @export
analyze_atrial_signal <- function(
  atrial_signal,
  frequency,
  characteristics = c(
    "amplitude",
    "dominant_frequency",
    "organization",
    "sample_entropy"
  ),
  original_signal = NULL,
  raw_signal = original_signal,
  qrs_loc = NULL,
  annotation = NULL,
  amplitude_window = c("tq", "all"),
  band = c(4, 10),
  entropy_rate = 256,
  tol = 0.15
) {
  if (!is.numeric(atrial_signal) || !is.numeric(frequency)) {
    stop("atrial_signal must be numeric and frequency must be a number")
  }
  amplitude_window <- match.arg(amplitude_window)

  atrial_signal <- as.numeric(atrial_signal)
  results <- list()

  if ("amplitude" %in% characteristics) {
    amp <- amplitude_features(
      atrial_signal,
      raw_signal = raw_signal,
      frequency = frequency,
      qrs_loc = qrs_loc,
      annotation = annotation,
      window = amplitude_window
    )
    results <- c(results, amp)
  }

  if ("dominant_frequency" %in% characteristics) {
    df <- calculate_dominant_frequency(
      atrial_signal,
      frequency = frequency,
      f_min = band[1],
      f_max = band[2]
    )
    results$dominant_rate <- df * 60

    # The harmonic index is the diagnostic that separates a fibrillatory peak
    # from ventricular residual: residual energy sits on integer multiples of
    # the heart rate, so the index comes out an integer. Read it as a screen and
    # not a test -- see `harmonic_flag()` for why.
    if (!is.null(qrs_loc) && length(qrs_loc) >= 2) {
      median_rr <- stats::median(diff(qrs_loc))
      results$harmonic_overlap <- df * median_rr / frequency
      results$on_harmonic <- harmonic_flag(results$harmonic_overlap, tol)
    } else {
      results$harmonic_overlap <- NA_real_
      results$on_harmonic <- NA
    }

  }

  # Organisation is a share of band power at the dominant frequency, so it needs
  # one -- but asking for it is enough. It used to be computed only inside the
  # branch above, so `f_characteristics = "organization"` on its own returned a
  # table with no organisation column in it and said nothing.
  if ("organization" %in% characteristics) {
    results$organization_index <- calculate_organization_index(
      atrial_signal,
      frequency = frequency,
      dominant_frequency = if ("dominant_frequency" %in% characteristics) {
        df
      } else {
        calculate_dominant_frequency(
          atrial_signal,
          frequency = frequency,
          f_min = band[1],
          f_max = band[2]
        )
      }
    )
  }

  if ("sample_entropy" %in% characteristics) {
    results$sample_entropy <- calculate_sample_entropy(
      decimate_for_entropy(atrial_signal, frequency, entropy_rate)
    )
  }

  if ("approximate_entropy" %in% characteristics) {
    results$approximate_entropy <- calculate_approximate_entropy(
      decimate_for_entropy(atrial_signal, frequency, entropy_rate)
    )
  }

  if (!is.null(original_signal) && !is.null(qrs_loc)) {
    results$cancellation_residual <- cancellation_residual(
      atrial_signal,
      original_signal,
      qrs_loc,
      frequency
    )
  }

  results
}

#' Refuse a series with holes in it
#'
#' Dropping the non-finite samples closes the gap they leave and joins two
#' stretches that were not adjacent. For a spectral estimate that shifts the
#' whole time axis; for an entropy it is worse, since the statistic *is* the
#' relationship between neighbouring samples and the spliced pairs are compared
#' as though they were contiguous. Both return a plausible number either way.
#'
#' @noRd
refuse_holes <- function(x, what) {
  n_bad <- sum(!is.finite(x))
  if (n_bad > 0) {
    stop(
      what,
      " cannot be computed on a series holding ",
      n_bad,
      " non-finite samples. Removing them would join stretches that are not ",
      "adjacent and the embedded vectors spanning the join would be compared ",
      "as though they were; interpolate or trim the record first.",
      call. = FALSE
    )
  }
  x
}

#' Decimate ahead of an O(n^2) entropy calculation
#' @noRd
decimate_for_entropy <- function(x, frequency, entropy_rate) {
  if (is.null(entropy_rate) || !is.finite(entropy_rate) || entropy_rate >= frequency) {
    return(x)
  }
  as.numeric(change_frequency(as.numeric(x), from = frequency, to = entropy_rate))
}

#' Flag a spectral peak that sits on a heart-rate harmonic
#'
#' A screen, not a test, and the difference matters. Ventricular residual
#' deposits its energy on integer multiples of the heart rate, so a contaminated
#' peak coincides with one. But so does an honest peak, often: the harmonic
#' overlap is a ratio of two unrelated numbers and integers are spaced one
#' apart, so a tolerance of 0.15 lands on one about three times in ten by
#' arithmetic alone. The flag is therefore sensitive and not specific, and it
#' belongs beside `cancellation_residual` rather than on its own.
#'
#' Neither `harmonic_overlap` nor this flag is a published quantity. See
#' [f_wave_diagnostics] for what they are and are not.
#'
#' @noRd
harmonic_flag <- function(harmonic_overlap, tol = 0.15) {
  if (is.null(harmonic_overlap) || !is.finite(harmonic_overlap)) {
    return(NA)
  }
  abs(harmonic_overlap - round(harmonic_overlap)) < tol
}

# Amplitude ----

#' Locate TQ segments, where the ventricles are electrically silent
#'
#' A TQ segment runs from a T offset to the next QRS onset, so the wave each
#' bracket belongs to has to be known. That comes from [label_waves()], which
#' reads it positionally from the peak symbol each `(`...`)` pair encloses --
#' not from the WFDB `number` column, which most annotators leave at zero
#' throughout. Reading `number` meant those files fell silently to the fixed
#' exclusion window below while still reporting `amplitude_window = "tq"`.
#'
#' The annotation must already be resolved to one lead; pooled across a
#' per-lead annotator every segment appears once per lead and `tq_fraction`
#' comes back greater than one.
#'
#' @noRd
tq_segments <- function(n, frequency, qrs_loc = NULL, annotation = NULL) {
  from_annotation <- NULL
  ann <- resolve_annotation(annotation)

  if (!is.null(ann)) {
    if (all(c("sample", "type") %in% names(ann))) {
      labelled <- label_waves(ann)
      t_off <- labelled$sample[labelled$type == ")" & labelled$wave %in% "T"]
      qrs_on <- labelled$sample[labelled$type == "(" & labelled$wave %in% "QRS"]

      if (length(t_off) > 0 && length(qrs_on) > 0) {
        segs <- lapply(t_off, function(s) {
          nxt <- qrs_on[qrs_on > s]
          if (length(nxt) == 0) {
            return(NULL)
          }
          e <- min(nxt) - 1L
          if (e - s < 2) {
            return(NULL)
          }
          c(max(1L, as.integer(s)), min(n, as.integer(e)))
        })
        segs <- Filter(Negate(is.null), segs)
        if (length(segs) > 0) {
          from_annotation <- segs
        }
      }
    }
  }

  if (!is.null(from_annotation)) {
    return(from_annotation)
  }

  if (is.null(qrs_loc) || length(qrs_loc) < 2) {
    return(list(c(1L, n)))
  }

  # Fallback exclusion: QRS onset through T offset, scaled to the prevailing RR
  rr <- diff(qrs_loc)
  median_rr <- stats::median(rr)
  pre <- as.integer(round(0.10 * frequency))
  post <- as.integer(round(min(0.40 * frequency, 0.55 * median_rr)))

  blocked <- logical(n)
  for (p in qrs_loc) {
    lo <- max(1L, as.integer(p) - pre)
    hi <- min(n, as.integer(p) + post)
    blocked[lo:hi] <- TRUE
  }

  runs <- rle(!blocked)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L
  keep <- runs$values & runs$lengths >= 3L

  if (!any(keep)) {
    return(list())
  }
  Map(c, starts[keep], ends[keep])
}

#' Amplitude of the fibrillatory signal
#'
#' Both a root-mean-square and a peak-to-peak amplitude are returned, and they
#' are not interchangeable. Peak-to-peak is the measure the coarse- versus
#' fine-AF literature uses, but it is a maximum over its segment and so grows
#' with the segment's length: on white noise its expectation rises 58% between
#' a 20-sample and a 400-sample window. TQ segments are as long as the RR
#' interval allows, so peak-to-peak carries a heart-rate confound that the
#' root-mean-square does not. Peak-to-peak is nonetheless the measure with the
#' clinical literature behind it: Li et al. called atrial fibrillation coarse at
#' 1 mm or more in V1 and found left atrial appendage dysfunction and thrombus
#' more common in those patients (Chest. 1995;108(2):359-363,
#' \doi{10.1378/chest.108.2.359}).
#'
#' Both are measured within each segment and then reduced by the median across
#' segments, which is what keeps a single noisy segment from carrying the lead.
#'
#' @noRd
amplitude_features <- function(
  atrial_signal,
  raw_signal,
  frequency,
  qrs_loc,
  annotation,
  window = c("tq", "all")
) {
  window <- match.arg(window)
  n <- length(atrial_signal)

  if (window == "all") {
    segs <- list(c(1L, n))
  } else {
    segs <- tq_segments(n, frequency, qrs_loc, annotation)
  }

  if (length(segs) == 0) {
    return(list(
      f_amplitude_p2p = NA_real_,
      f_amplitude_rms = NA_real_,
      qrs_amplitude = qrs_excursion(raw_signal, qrs_loc, frequency),
      f_ratio = NA_real_,
      tq_fraction = 0
    ))
  }

  idx <- unlist(lapply(segs, function(s) seq.int(s[1], s[2])))
  idx <- idx[idx >= 1 & idx <= n]

  # Peak-to-peak per segment, then the median across segments
  # Both amplitudes are summarised the same way: measured within each segment,
  # then the median across segments. A pooled figure over every TQ sample at
  # once would let one noisy segment carry the lead, and the segments are not
  # exchangeable -- they vary in length with the RR interval.
  per_segment <- function(f) {
    vapply(
      segs,
      function(s) {
        v <- atrial_signal[seq.int(s[1], s[2])]
        if (length(v) < 2 || all(!is.finite(v))) NA_real_ else f(v)
      },
      numeric(1)
    )
  }

  p2p <- per_segment(function(v) diff(range(v, na.rm = TRUE)))
  rms <- per_segment(function(v) sqrt(mean(v^2, na.rm = TRUE)))

  qrs_amp <- qrs_excursion(raw_signal, qrs_loc, frequency)
  p2p_median <- stats::median(p2p, na.rm = TRUE)

  list(
    f_amplitude_p2p = p2p_median,
    f_amplitude_rms = stats::median(rms, na.rm = TRUE),
    qrs_amplitude = qrs_amp,
    # Dividing by the QRS excursion in the same lead is meant to cancel the
    # thoracic transfer function to first order: the ventricular signal
    # traverses the same tissue, so it carries the same attenuation. That
    # matters for comparison between patients, where raw amplitude is dominated
    # by body habitus rather than by atrial physiology.
    #
    # The f-wave amplitude in the numerator is a long-established measure. The
    # ratio is not: it is specific to this package, the reasoning above is its
    # entire justification, and it has not been validated against anything.
    f_ratio = if (is.finite(qrs_amp) && qrs_amp > 0) p2p_median / qrs_amp else NA_real_,
    tq_fraction = length(idx) / n
  )
}

#' Median QRS peak-to-peak excursion, measured on the unfiltered signal
#' @noRd
qrs_excursion <- function(signal, qrs_loc, frequency, half_ms = 60) {
  if (is.null(signal) || is.null(qrs_loc) || length(qrs_loc) == 0) {
    return(NA_real_)
  }
  signal <- as.numeric(signal)
  n <- length(signal)
  half <- as.integer(round(half_ms * frequency / 1000))

  amps <- vapply(
    qrs_loc,
    function(p) {
      lo <- max(1L, as.integer(p) - half)
      hi <- min(n, as.integer(p) + half)
      if (hi - lo < 2) NA_real_ else diff(range(signal[lo:hi], na.rm = TRUE))
    },
    numeric(1)
  )

  stats::median(amps, na.rm = TRUE)
}

#' Residual ventricular energy left behind by cancellation
#'
#' Energy in a window around each QRS after cancellation, as a fraction of the
#' same window before it. Model-free, and independent of any spectral
#' assumption, so it says how well cancellation worked without presuming what
#' the atrial spectrum should look like.
#'
#' This is a variant of the published *ventricular residue*. Alcaraz and Rieta
#' introduced VR to score how much ventricular activity survives in an extracted
#' atrial signal, and Alcaraz, Sornmo and Rieta later recommended the
#' unnormalised form, uVR, as the index to use when characterising extraction
#' performance on real rather than simulated signals. Their VR is scaled so that
#' reported values run above 1 -- 3.16 for average-beat subtraction against 1.73
#' for adaptive singular value cancellation -- while this ratio is an energy
#' fraction bounded below by 0, so the numbers are not interchangeable with
#' theirs and only the ordering carries over.
#'
#' Alcaraz R, Rieta JJ. Adaptive singular value cancelation of ventricular
#' activity in single-lead atrial fibrillation electrocardiograms.
#' Physiological Measurement. 2008;29(12):1351-1369.
#' \doi{10.1088/0967-3334/29/12/001}
#'
#' Alcaraz R, Sornmo L, Rieta JJ. Reference database and performance evaluation
#' of methods for extraction of atrial fibrillatory waves in the ECG.
#' Physiological Measurement. 2019;40(7):075011.
#' \doi{10.1088/1361-6579/ab2b17}
#'
#' @noRd
cancellation_residual <- function(
  atrial_signal,
  original_signal,
  qrs_loc,
  frequency,
  half_ms = 80
) {
  if (is.null(original_signal) || length(qrs_loc) == 0) {
    return(NA_real_)
  }

  n <- length(atrial_signal)
  half <- as.integer(round(half_ms * frequency / 1000))

  idx <- unique(unlist(lapply(qrs_loc, function(p) {
    seq.int(max(1L, as.integer(p) - half), min(n, as.integer(p) + half))
  })))
  idx <- idx[idx >= 1 & idx <= n]

  if (length(idx) == 0) {
    return(NA_real_)
  }

  before <- sum(original_signal[idx]^2, na.rm = TRUE)
  after <- sum(atrial_signal[idx]^2, na.rm = TRUE)

  if (!is.finite(before) || before <= 0) {
    return(NA_real_)
  }
  after / before
}

#' Collapse the per-lead feature lists into one table
#' @noRd
assemble_feature_table <- function(features, leads, normalize = "none") {
  cols <- c(
    "f_amplitude_p2p",
    "f_amplitude_rms",
    "qrs_amplitude",
    "f_ratio",
    "dominant_rate",
    "organization_index",
    "sample_entropy",
    "approximate_entropy",
    "harmonic_overlap",
    "on_harmonic",
    "cancellation_residual",
    "tq_fraction"
  )

  present <- cols[vapply(cols, function(f) {
    any(vapply(features, function(x) !is.null(x[[f]]), logical(1)))
  }, logical(1))]

  dt <- data.table::data.table(lead = leads)
  for (f in present) {
    vals <- lapply(features, function(x) {
      v <- x[[f]]
      if (is.null(v) || length(v) == 0) NA else v[[1]]
    })
    data.table::set(dt, j = f, value = unlist(vals, use.names = FALSE))
  }

  # `f_amplitude` names whichever amplitude the caller asked to work in.
  # Normalisation is never the silent default, since it changes the units. The
  # unnormalised default is the root-mean-square rather than the peak-to-peak,
  # because peak-to-peak grows with the length of the segment it is taken over
  # and TQ segment length is set by the RR interval.
  if (normalize == "qrs" && "f_ratio" %in% names(dt)) {
    data.table::set(dt, j = "f_amplitude", value = dt$f_ratio)
  } else if ("f_amplitude_rms" %in% names(dt)) {
    data.table::set(dt, j = "f_amplitude", value = dt$f_amplitude_rms)
  }

  dt[]
}

# Spectral estimation ----

#' Welch power spectral density estimate
#'
#' @description Averages the periodograms of overlapping, Hann-tapered segments.
#'
#' @details A raw periodogram is an inconsistent estimator: its variance does not
#'   fall as the record lengthens, so on a ten second record the spectrum is very
#'   noisy and an argmax taken over a several-Hz band is correspondingly
#'   unstable. Averaging tapered segments trades frequency resolution for
#'   variance, and zero-padding to a fixed resolution keeps the bin edges from
#'   moving with record length.
#'
#' @param x Numeric vector
#' @param frequency Sampling frequency in Hz
#' @param segment_sec Segment length in seconds. Default 4.
#' @param overlap Fractional overlap between segments. Default 0.5.
#' @param resolution Target frequency resolution in Hz, achieved by zero
#'   padding. Default 0.05.
#'
#' @return A list with `freq` and `spec`
#'
#' @references
#' Welch PD. The use of fast Fourier transform for the estimation of power
#' spectra: a method based on time averaging over short, modified periodograms.
#' *IEEE Transactions on Audio and Electroacoustics*. 1967;15(2):70-73.
#' \doi{10.1109/TAU.1967.1161901}
#'
#' @examples
#' x <- sin(2 * pi * 6 * seq(0, 10, by = 1 / 500)) + rnorm(5001, sd = 0.1)
#' psd <- calculate_welch_spectrum(x, frequency = 500)
#' psd$freq[which.max(psd$spec)]
#'
#' @export
calculate_welch_spectrum <- function(
  x,
  frequency,
  segment_sec = 4,
  overlap = 0.5,
  resolution = 0.05
) {
  x <- as.numeric(x)
  n <- length(x)

  if (n < 8) {
    stop("Signal is too short for a spectral estimate")
  }
  refuse_holes(x, "A spectral estimate")

  nperseg <- min(n, max(8L, as.integer(round(segment_sec * frequency))))
  step <- max(1L, as.integer(round(nperseg * (1 - overlap))))

  starts <- seq.int(1L, n - nperseg + 1L, by = step)
  if (length(starts) == 0) {
    starts <- 1L
  }

  # Hann taper controls leakage from the strong low-frequency content that
  # survives bandpass filtering.
  w <- 0.5 * (1 - cos(2 * pi * (seq_len(nperseg) - 1) / (nperseg - 1)))
  win_power <- sum(w^2)

  nfft <- max(nperseg, 2^ceiling(log2(frequency / resolution)))
  n_out <- floor(nfft / 2) + 1L

  acc <- numeric(n_out)
  for (s in starts) {
    seg <- x[seq.int(s, s + nperseg - 1L)]
    seg <- seg - mean(seg)
    padded <- c(seg * w, numeric(nfft - nperseg))
    p <- Mod(stats::fft(padded)[seq_len(n_out)])^2 / (frequency * win_power)
    acc <- acc + p
  }

  spec <- acc / length(starts)

  # One-sided: fold the power from the negative frequencies onto the positive
  # ones, leaving DC and Nyquist alone
  if (n_out > 2) {
    interior <- seq.int(2L, n_out - as.integer(nfft %% 2 == 0))
    spec[interior] <- 2 * spec[interior]
  }

  list(
    freq = (seq_len(n_out) - 1) * frequency / nfft,
    spec = spec
  )
}

#' Calculate the dominant frequency of a time series
#'
#' @description Returns the frequency carrying the most power within a search
#'   band, estimated from a Welch-averaged periodogram.
#'
#' @param x Numeric vector of the time series
#' @param frequency Sampling frequency of the signal in Hz
#' @param f_min Lower edge of the search band in Hz. Default 4.
#' @param f_max Upper edge of the search band in Hz. Default 10. Sustained
#'   atrial fibrillation is usually quoted at 4-10 Hz; typical atrial flutter is
#'   4-5.5 Hz and slow or drug-modified flutter falls below 4 Hz, so widen the
#'   band when flutter is expected.
#' @param ... Passed to [calculate_welch_spectrum()]
#'
#' @return Dominant frequency in Hz. `NA` when the band holds no finite power.
#'
#' @examples
#' x <- sin(2 * pi * 6 * seq(0, 10, by = 1 / 500)) + rnorm(5001, sd = 0.1)
#' calculate_dominant_frequency(x, frequency = 500)
#'
#' @export
calculate_dominant_frequency <- function(x, frequency, f_min = 4, f_max = 10, ...) {
  psd <- calculate_welch_spectrum(x, frequency = frequency, ...)

  idx <- which(psd$freq >= f_min & psd$freq <= f_max)
  if (length(idx) == 0) {
    return(NA_real_)
  }

  power_range <- psd$spec[idx]
  if (!any(is.finite(power_range))) {
    return(NA_real_)
  }

  psd$freq[idx][which.max(power_range)]
}

#' Dominant frequency from a spectrum pooled across leads
#'
#' Each lead is normalised to unit power in 2.5-15 Hz before averaging, so that
#' one high-amplitude lead cannot dictate the peak.
#'
#' @noRd
pooled_dominant_frequency <- function(signals, frequency, band = c(4, 10)) {
  psds <- lapply(signals, function(x) {
    calculate_welch_spectrum(x, frequency = frequency)
  })

  ref <- psds[[1]]$freq
  norm_band <- which(ref >= 2.5 & ref <= 15)

  normalised <- lapply(psds, function(p) {
    total <- sum(p$spec[norm_band], na.rm = TRUE)
    if (!is.finite(total) || total <= 0) NULL else p$spec / total
  })
  normalised <- Filter(Negate(is.null), normalised)

  if (length(normalised) == 0) {
    return(list(dominant_frequency = NA_real_, freq = ref, spec = rep(NA_real_, length(ref))))
  }

  pooled <- Reduce(`+`, normalised) / length(normalised)

  idx <- which(ref >= band[1] & ref <= band[2])
  df <- if (length(idx) == 0 || !any(is.finite(pooled[idx]))) {
    NA_real_
  } else {
    ref[idx][which.max(pooled[idx])]
  }

  list(dominant_frequency = df, freq = ref, spec = pooled)
}

#' Calculate the organisation index of an atrial signal
#'
#' @description The share of spectral power carried by the dominant frequency
#'   and its harmonics.
#'
#' @details A highly organised atrium concentrates its energy in a narrow peak
#'   and its harmonics; a disorganised one spreads it across the spectrum. This
#'   is one of the few f-wave features with a reasonably direct
#'   electrophysiological reading, being related to the number of independent
#'   wavefronts the atrium is holding.
#'
#'   The definition follows the published one: the area under the dominant peak
#'   and its first `n_harmonics` harmonics, over the total area of the spectrum.
#'   Everett et al. introduced it on an interatrial electrogram, taking the
#'   first four harmonic peaks; An et al. give the clearest surface-ECG
#'   implementation, taking the highest peak and its first four harmonics over
#'   the total area from 0 to 50 Hz on a QRST-cancelled lead. The defaults here
#'   are theirs.
#'
#'   # What the value can be compared with
#'
#'   Not a published cut-point, and the gap is not small. An et al. report a
#'   median of 0.33 (IQR 0.27-0.39) in lead V1 of 102 patients; other surface
#'   implementations report 0.26-0.32 and 0.70-0.75. This function returns about
#'   0.13 on the bundled `muse-af` record with the same formula and the same
#'   band, so the remaining difference is in the cancellation, the peak
#'   integration and the cohort rather than in the definition. Published
#'   surface organisation indices are already implementation-dependent enough
#'   that they do not transfer between groups; treat these values as comparable
#'   within a cohort analysed this way and with nothing else.
#'
#'   Two further details are local choices with no published basis. The peak is
#'   integrated over a fixed window rather than out to the shoulders of the peak
#'   itself, and the harmonic windows are 1.5 times as wide as the fundamental's
#'   to allow for the harmonic drifting. On a surface record the harmonic count
#'   barely matters: at a dominant frequency of 7.5 Hz the third harmonic and
#'   above sit where the f-wave carries no energy, and moving from two peaks to
#'   five changes the value on `muse-af` by 0.007.
#'
#' @param x Numeric vector of the atrial signal
#' @param frequency Sampling frequency in Hz
#' @param dominant_frequency Dominant frequency in Hz. Estimated from `x` if
#'   `NULL`.
#' @param n_harmonics Number of harmonics summed with the dominant peak.
#'   Default 4, as in Everett et al. and An et al.
#' @param band Numeric length-2 vector for the total-power reference band.
#'   Default `c(0.5, 50)`, following An et al., and clipped below the Nyquist
#'   frequency. Note that the band cannot put back what the signal does not
#'   carry: an atrial signal bandpassed to 30 Hz contributes nothing between 30
#'   and 50 Hz, so widening the band beyond the passband changes little.
#' @param half_width Half-width in Hz of the window placed on the dominant
#'   frequency. The harmonic windows are 1.5 times as wide. Default 0.5.
#'
#' @return Organisation index between 0 and 1
#'
#' @references
#' Everett TH 4th, Kok LC, Vaughn RH, Moorman JR, Haines DE. Frequency domain
#' algorithm for quantifying atrial fibrillation organization to increase
#' defibrillation efficacy. *IEEE Transactions on Biomedical Engineering*.
#' 2001;48(9):969-978. \doi{10.1109/10.942586}
#'
#' Everett TH 4th, Moorman JR, Kok LC, Akar JG, Haines DE. Assessment of global
#' atrial fibrillation organization to optimize timing of atrial defibrillation.
#' *Circulation*. 2001;103(23):2857-2861.
#' \doi{10.1161/01.CIR.103.23.2857}
#'
#' An K, Li H, Yu C, Zheng Z. Surface electrocardiogram f wave analysis in
#' patients with atrial fibrillation undergoing thoracoscopic epicardial
#' ablation. *Interdisciplinary CardioVascular and Thoracic Surgery*.
#' 2024;38(5):ivae057. \doi{10.1093/icvts/ivae057}
#'
#' Stavrakis S, Dyer JW, Koomson E, et al. Spectral analysis of baseline
#' electrocardiogram during atrial fibrillation predicts response to
#' antiarrhythmic drug therapy in patients with persistent atrial fibrillation.
#' *Journal of Cardiovascular Electrophysiology*. 2016;27(11):1312-1318.
#' \doi{10.1111/jce.13064}
#'
#' @examples
#' x <- sin(2 * pi * 6 * seq(0, 10, by = 1 / 500)) + rnorm(5001, sd = 0.5)
#' calculate_organization_index(x, frequency = 500)
#'
#' @export
calculate_organization_index <- function(
  x,
  frequency,
  dominant_frequency = NULL,
  n_harmonics = 4L,
  band = c(0.5, 50),
  half_width = 0.5
) {
  psd <- calculate_welch_spectrum(x, frequency = frequency)

  if (is.null(dominant_frequency)) {
    dominant_frequency <- calculate_dominant_frequency(x, frequency = frequency)
  }
  if (!is.finite(dominant_frequency)) {
    return(NA_real_)
  }

  # A band reaching past Nyquist is a band reaching past what was recorded
  band <- c(band[1], min(band[2], frequency / 2 * 0.95))

  total_idx <- which(psd$freq >= band[1] & psd$freq <= band[2])
  total <- sum(psd$spec[total_idx], na.rm = TRUE)
  if (!is.finite(total) || total <= 0) {
    return(NA_real_)
  }

  # The dominant peak and its harmonics, each taken once even where two windows
  # overlap, and each confined to the reference band
  windows <- lapply(seq.int(0L, as.integer(n_harmonics)), function(k) {
    centre <- (k + 1L) * dominant_frequency
    hw <- if (k == 0L) half_width else half_width * 1.5
    which(psd$freq >= centre - hw & psd$freq <= centre + hw)
  })
  peak_idx <- intersect(unique(unlist(windows)), total_idx)

  concentrated <- sum(psd$spec[peak_idx], na.rm = TRUE)

  min(1, concentrated / total)
}

# Utility functions ----

#' Detect QRS complexes in ECG signals
#'
#' @description `detect_QRS()` implements a modified Pan-Tompkins algorithm to
#' detect QRS complexes in ECG signals. The function applies a sequence of
#' processing steps including bandpass filtering, differentiation, squaring, and
#' moving window integration to identify R peaks in the signal.
#'
#' @details The Pan-Tompkins algorithm is a widely-used method for QRS detection
#' in ECG signals. This implementation follows these steps:
#'
#' 1. Bandpass filtering (5-15 Hz) to reduce noise and emphasize QRS complexes
#' 2. Differentiation to highlight the steep slopes of QRS complexes
#' 3. Squaring to amplify high-frequency components
#' 4. Moving window integration to consider the overall QRS morphology
#' 5. Thresholding at the mean plus half a standard deviation of the integrated
#' signal to identify peaks
#' 6. Application of a refractory period to prevent multiple detections of the
#' same QRS complex
#'
#' The threshold is a single static one over the whole record, not the pair of
#' adaptive signal and noise thresholds with searchback that Pan and Tompkins
#' describe. That is enough on a clean ten-second strip and will drop beats on a
#' record whose amplitude drifts.
#'
#' Positions are reported at the peak of the integration window, which lags the
#' true R peak by roughly half the integration width. [extract_f_waves()]
#' refines them against the signal before use, and anything else that needs beat
#' alignment should do the same.
#'
#' @param signal Numeric vector representing the ECG signal
#' @param frequency Sampling frequency of the signal in Hz
#' @param window_size Width of the integration window in seconds, default is
#'   0.150 seconds
#'
#' @return Integer vector containing the sample indices of detected QRS
#'   complexes
#'
#' @references Pan, J., & Tompkins, W. J. (1985). A real-time QRS detection
#' algorithm. IEEE Transactions on Biomedical Engineering, (3), 230-236.
#' \doi{10.1109/TBME.1985.325532}
#'
#' @examples
#' \dontrun{
#' ecg_data <- read_wfdb("muse-af", system.file("extdata", package = "EGM"))
#' freq <- attributes(ecg_data$header)$record_line$frequency
#' qrs_locations <- detect_QRS(ecg_data$signal$II, freq)
#' }
#'
#' @export
detect_QRS <- function(signal, frequency, window_size = 0.150) {
  nyquist_freq <- frequency / 2
  low_cutoff <- 5 / nyquist_freq
  high_cutoff <- min(15 / nyquist_freq, 0.95)

  bp_filter <- signal::butter(n = 4, W = c(low_cutoff, high_cutoff), type = "pass")
  filtered_signal <- signal::filtfilt(bp_filter, signal)

  derivative_filter <- c(-1, -2, 0, 2, 1) * (frequency / 8)
  differentiated_signal <- signal::filter(derivative_filter, 1, filtered_signal)

  squared_signal <- differentiated_signal^2

  window_size <- round(window_size * frequency)
  integration_filter <- rep(1 / window_size, window_size)
  integrated_signal <- signal::filter(integration_filter, 1, squared_signal)

  threshold <- mean(integrated_signal, na.rm = TRUE) +
    0.5 * stats::sd(integrated_signal, na.rm = TRUE)

  # A local maximum rises into the sample and falls out of it. Both comparisons
  # used to test for a rise, which flags every sample on a rising limb instead;
  # with the refractory loop below taking the first of each cluster, what came
  # back was the threshold crossing rather than the peak.
  n <- length(integrated_signal)
  rises_into <- c(FALSE, integrated_signal[-n] < integrated_signal[-1])
  falls_out_of <- c(integrated_signal[-1] < integrated_signal[-n], FALSE)
  is_peak <- (integrated_signal > threshold) & rises_into & falls_out_of
  is_peak[is.na(is_peak)] <- FALSE
  peak_indices <- which(is_peak)

  refractory_period <- round(0.200 * frequency)
  final_peak_indices <- integer(0)
  last_peak <- -Inf

  for (idx in peak_indices) {
    if ((idx - last_peak) > refractory_period) {
      final_peak_indices <- c(final_peak_indices, idx)
      last_peak <- idx
    }
  }

  final_peak_indices
}

#' Calculate Sample Entropy of a time series
#'
#' @description Sample entropy measures the irregularity of a time series as the
#'   negative log conditional probability that two sequences similar for `m`
#'   points remain similar at the next point.
#'
#' @details Unlike approximate entropy, sample entropy excludes self-matches.
#'   Counting self-matches biases approximate entropy toward regularity and makes
#'   it depend on record length, which are the two problems sample entropy was
#'   introduced to fix. Prefer this over [calculate_approximate_entropy()],
#'   particularly when record length varies.
#'
#'   The calculation is O(n^2), which is a standing temptation to decimate
#'   further than the measurement tolerates. Alcaraz et al. tuned `m`, `r` and
#'   the sampling rate for atrial fibrillation organisation specifically and
#'   found classification degraded below 256 Hz, with `m` of 1 or 2 and `r`
#'   between 0.1 and 0.25 times the standard deviation. The defaults here sit
#'   inside that, and [extract_f_waves()] decimates to 256 Hz rather than to
#'   something derived from the fibrillatory bandwidth.
#'
#' @param x Numeric vector of the time series
#' @param m Embedding dimension. Default 2.
#' @param r Tolerance. Default `NULL`, which uses 0.2 times the standard
#'   deviation of `x`.
#'
#' @return Sample entropy. `NA` when no matches are found at either length.
#'
#' @references Richman JS, Moorman JR. Physiological time-series analysis using
#'   approximate entropy and sample entropy. *American Journal of Physiology.
#'   Heart and Circulatory Physiology*. 2000;278(6):H2039-H2049.
#'   \doi{10.1152/ajpheart.2000.278.6.H2039}
#'
#'   Alcaraz R, Abasolo D, Hornero R, Rieta JJ. Optimized assessment of atrial
#'   fibrillation organization through suitable parameters of sample entropy.
#'   *Annual International Conference of the IEEE Engineering in Medicine and
#'   Biology Society*. 2010;2010:118-121. \doi{10.1109/IEMBS.2010.5627169}
#'
#' @examples
#' set.seed(123)
#' calculate_sample_entropy(rnorm(500))
#'
#' @export
calculate_sample_entropy <- function(x, m = 2, r = NULL) {
  x <- refuse_holes(as.double(x), "Sample entropy")

  if (length(x) < m + 2) {
    return(NA_real_)
  }
  if (is.null(r)) {
    r <- -1
  }

  calculate_sample_entropy_cpp(x, as.integer(m), as.double(r))
}

#' Calculate Approximate Entropy (ApEn) of a time series
#'
#' @description Computes approximate entropy by the method of Pincus (1991),
#'   comparing vectors embedded in `m` and `m + 1` dimensions and taking the
#'   difference of the resulting correlation integrals.
#'
#' @details Approximate entropy counts self-matches, which biases it toward
#'   regularity and makes it dependent on record length.
#'   [calculate_sample_entropy()] avoids both and is generally preferable.
#'
#'   The tolerance `r` defaults to 0.2 times the standard deviation of `x`. Note
#'   that this default changed: earlier versions used 3.5 times the standard
#'   deviation, which admitted nearly every pair of vectors as a match and drove
#'   the statistic toward zero regardless of the input.
#'
#'   The calculation is O(n^2). Decimate before calling; see
#'   [calculate_sample_entropy()] for how far.
#'
#' @param x Numeric vector of the time series
#' @param m Embedding dimension. Default 2.
#' @param r Tolerance. Default `NULL`, which uses 0.2 times the standard
#'   deviation of `x`.
#'
#' @return Approximate entropy value
#'
#' @references Pincus SM. Approximate entropy as a measure of system complexity.
#'   *Proceedings of the National Academy of Sciences of the USA*.
#'   1991;88(6):2297-2301. \doi{10.1073/pnas.88.6.2297}
#'
#' @examples
#' set.seed(123)
#' calculate_approximate_entropy(rnorm(500))
#'
#' @export
calculate_approximate_entropy <- function(x, m = 2, r = NULL) {
  x <- refuse_holes(as.double(x), "Approximate entropy")

  if (length(x) < m + 2) {
    return(NA_real_)
  }
  # -1 is the flag telling the C++ side to compute the tolerance itself; NULL
  # cannot cross the boundary
  if (is.null(r)) {
    r <- -1
  }

  calculate_approximate_entropy_cpp(x, as.integer(m), as.double(r))
}

#' Diagnostics returned with every fibrillatory estimate, and where they came from
#'
#' @description Not a function. This documents the diagnostic fields that
#'   [extract_f_waves()] returns beside each spectral feature, why they must be
#'   read together with it, and -- for each output of the fibrillatory analysis
#'   -- whether it is a published quantity, an adaptation of one, or specific to
#'   this package.
#'
#' @details
#'
#' # Where each output comes from
#'
#' A number with no citation should be assumed to be wrong until it has one, so
#' the provenance is stated here rather than left to be inferred.
#'
#' **Published, and computed as published.** `dominant_rate` is the atrial
#' fibrillatory rate, reported in fibrillations per minute as the literature
#' does. `sample_entropy` is Richman and Moorman's statistic, with the embedding
#' dimension, tolerance and sampling rate that Alcaraz et al. tuned for this
#' task. `f_amplitude_p2p` is fibrillatory wave amplitude, the measure behind
#' the coarse-versus-fine distinction: Li et al. called atrial fibrillation
#' coarse at a peak-to-peak amplitude of 1 mm or more in V1, and found left
#' atrial appendage dysfunction and thrombus more common in those patients.
#' `rr_cv` is the coefficient of variation of the RR interval, which Tateno and
#' Glass used to detect atrial fibrillation at 86.6% sensitivity and 84.3%
#' specificity.
#'
#' **Adapted from a published quantity.** `organization_index` follows Everett's
#' definition and An's surface implementation but is not on their scale; see
#' [calculate_organization_index()]. `cancellation_residual` is a variant of the
#' published ventricular residue index, on a different normalisation.
#'
#' **Specific to this package, with no published definition.**
#' `harmonic_overlap` and `on_harmonic`, described below, and `f_ratio`, which
#' divides the fibrillatory amplitude by the QRS excursion in the same lead. The
#' f-wave amplitude in the numerator is thoroughly established; dividing it by
#' the QRS is not, and was introduced here to make amplitudes comparable between
#' patients of different body habitus, on the reasoning that the ventricular
#' signal traverses the same thorax and so carries the same attenuation. That is
#' a stated rationale, not a validated one.
#'
#' **Package operating points, not published cut-points.** `af_like` fires at an
#' RR coefficient of variation of 0.12 with a normalised RMSSD of 0.10, and
#' `rr_regular` below 0.05. The indices are the published ones; these three
#' thresholds were chosen here, and exist to decide whether to warn rather than
#' to diagnose anything.
#'
#' # The diagnostic fields
#'
#' \describe{
#'   \item{`harmonic_overlap`}{How many fibrillatory cycles fit into one average
#'     heartbeat: the dominant frequency divided by the heart rate, both in Hz.
#'     Ventricular energy that cancellation failed to remove sits on integer
#'     multiples of the heart rate, so a contaminated peak makes this a whole
#'     number. **This is not a published quantity.** The concern behind it is
#'     established -- Ng and Goldberger set out how a dominant frequency can
#'     report ventricular residual rather than atrial activity -- but no paper
#'     defines this ratio, and the name should not be read as naming something
#'     from the literature. It was called `harmonic_index` in an earlier version,
#'     which implied more than it should have.}
#'   \item{`on_harmonic`}{`TRUE` when `harmonic_overlap` is within `tol` of an
#'     integer. **`dominant_rate` must not be used without conditioning on
#'     this.** A contaminated estimate is precise, wrong, and highly
#'     reproducible, because heart rate is highly reproducible within a patient.
#'     Validating the feature by test-retest reliability will therefore select
#'     the artifact. It is a sensitive screen rather than a specific test: the
#'     overlap of an honest peak is a ratio of two unrelated numbers, so with
#'     integers spaced one apart a tolerance of 0.15 lands on one about three
#'     times in ten by arithmetic alone, and excluding every flagged record
#'     discards roughly a third of the good ones with the bad. Read it beside
#'     `cancellation_residual`; a flagged record with a small residual is
#'     probably a coincidence.}
#'   \item{`cancellation_residual`}{Residual energy in a window around each QRS
#'     as a fraction of that window's energy before cancellation. Model-free, so
#'     it is independent of any assumption about the atrial spectrum. It reports
#'     what cancellation *failed* to remove, never what it removed and should not
#'     have; see `rr_regular` for that.}
#'   \item{`rr_regular`}{`TRUE` when the ventricular response is regular (RR CV
#'     below 0.05). Cancellation subtracts whatever repeats at a fixed phase to
#'     the QRS, so on a regular rhythm it takes the atrial signal with it -
#'     flutter conducting at a fixed ratio loses most of its wave, and its
#'     `organization_index` collapses into the range fibrillation occupies.
#'     `cancellation_residual` is *small* when this happens, because the template
#'     fits better for having absorbed the atrial wave, so this is the only field
#'     that reports it. See the cancellation section of [extract_f_waves()].}
#'   \item{`tq_fraction`}{Share of the record that was electrically silent and
#'     therefore usable for amplitude measurement.}
#'   \item{`n_beats_cancelled`, `n_beats_skipped`, `n_beats_aberrant`}{How much
#'     of the record was actually processed.}
#' }
#'
#' @references
#' Ng J, Goldberger JJ. Understanding and interpreting dominant frequency
#' analysis of AF electrograms. *Journal of Cardiovascular Electrophysiology*.
#' 2007;18(6):680-685. \doi{10.1111/j.1540-8167.2007.00832.x}
#'
#' Li YH, Hwang JJ, Tseng YZ, Kuan P, Lien WP. Clinical significance of
#' fibrillatory wave amplitude. A clue to left atrial appendage function in
#' nonrheumatic atrial fibrillation. *Chest*. 1995;108(2):359-363.
#' \doi{10.1378/chest.108.2.359}
#'
#' Tateno K, Glass L. Automatic detection of atrial fibrillation using the
#' coefficient of variation and density histograms of RR and deltaRR intervals.
#' *Medical & Biological Engineering & Computing*. 2001;39(6):664-671.
#' \doi{10.1007/BF02345439}
#'
#' Alcaraz R, Rieta JJ. Adaptive singular value cancelation of ventricular
#' activity in single-lead atrial fibrillation electrocardiograms.
#' *Physiological Measurement*. 2008;29(12):1351-1369.
#' \doi{10.1088/0967-3334/29/12/001}
#'
#' Alcaraz R, Sornmo L, Rieta JJ. Reference database and performance evaluation
#' of methods for extraction of atrial fibrillatory waves in the ECG.
#' *Physiological Measurement*. 2019;40(7):075011.
#' \doi{10.1088/1361-6579/ab2b17}
#'
#' @name f_wave_diagnostics
NULL
