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
#' The default `cancel_method = "spatiotemporal"` implements the approach of
#' Stridh and Sornmo (2001). A single set of QRS positions is shared by every
#' lead, and a template beat is formed for each lead by taking the *median*
#' across beats. Each individual beat, in each lead, is then fitted by least
#' squares to a linear combination of the templates from *all* leads plus an
#' intercept, and the fit is subtracted.
#'
#' The extra degrees of freedom matter. A single-lead template cannot absorb the
#' beat-to-beat rotation of the heart's electrical axis caused by respiration and
#' by varying ventricular filling, which is the dominant reason a template fails
#' to fit its own beat. What it leaves behind is periodic at the heart rate, so
#' it deposits energy on heart-rate harmonics -- inside the very band this
#' function reads. Use [f_wave_diagnostics] to check whether that has happened.
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
#' @param cancel_method Ventricular cancellation method. One of
#'   `"spatiotemporal"` (default), `"average_beat"`, or `"adaptive_svd"`.
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
#' @param normalize Amplitude normalisation. `"none"` (default) returns raw
#'   signal units. `"qrs"` additionally divides by the QRS excursion in the same
#'   lead, which cancels the thoracic transfer function to first order and makes
#'   amplitudes comparable *between* patients. Both are always returned; this
#'   argument only sets which one `f_amplitude` refers to.
#'
#' @param band Numeric length-2 vector giving the frequency band searched for the
#'   dominant fibrillatory frequency, in Hz. Default `c(4, 10)`. Sustained atrial
#'   fibrillation is usually quoted at 4-10 Hz. Typical atrial flutter is
#'   4-5.5 Hz, and slow or drug-modified flutter falls below 4 Hz, so widen this
#'   when flutter is expected.
#'
#' @param entropy_rate Sampling rate in Hz to which the atrial signal is
#'   decimated before entropy is computed. Default 50. Entropy is O(n^2) and the
#'   fibrillatory band is below 10 Hz, so computing it at the raw rate is both
#'   enormously slower and dominated by the smoothness of the interpolation
#'   between neighbouring samples rather than by the organisation of the rhythm.
#'
#' @param pooled_spectrum Logical. If `TRUE` (default), the dominant frequency is
#'   taken from a spectrum pooled across all analysed leads, each normalised to
#'   unit power in 2.5-15 Hz before averaging. A ten second record yields only
#'   about four Welch segments per lead; twelve leads give roughly forty-eight,
#'   which is the difference between a usable and an unusable variance.
#'   Normalising before pooling stops one high-amplitude lead dictating the peak.
#'
#' @param rhythm Optional character string naming the rhythm, e.g. `"af"`,
#'   `"flutter"`, `"sinus"`. Used only to decide whether to warn. If `NULL`
#'   (default), rhythm is inferred from RR irregularity.
#'
#' @param tol Numeric tolerance for the harmonic test. A peak is flagged
#'   `on_harmonic` when its harmonic index falls within `tol` of an integer.
#'   Default 0.15.
#'
#' @param keep_signal Logical. If `TRUE`, the cancelled atrial signals are
#'   returned alongside the features. Default `FALSE`, since retaining them
#'   across a large batch is expensive.
#'
#' @param verbose Logical. If `TRUE` (default), report which leads are analysed.
#'
#' @param ... Additional arguments passed to the cancellation and analysis
#'   routines.
#'
#' @return An object of class `f_wave_analysis`, a list with:
#'
#'   \describe{
#'     \item{`features`}{A `data.table` with one row per lead, holding
#'       `f_amplitude_p2p`, `f_amplitude_rms`, `qrs_amplitude`, `f_ratio`,
#'       `dominant_rate`, `organization_index`, `sample_entropy`, and the
#'       per-lead diagnostics `harmonic_index`, `on_harmonic`,
#'       `cancellation_residual`, and `tq_fraction`.}
#'     \item{`record`}{A one-row `data.table` of record-level values, including
#'       `n_beats_cancelled`, `n_beats_skipped`, `n_beats_aberrant`,
#'       `spatial_dispersion`, `heart_rate`, `rr_cv`, and `af_like`.}
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
#' @seealso [cancel_ventricular_signal()], [calculate_dominant_frequency()],
#'   [calculate_sample_entropy()], [calculate_organization_index()]
#'
#' @examples
#' \dontrun{
#' af <- read_wfdb("muse-af", system.file("extdata", package = "EGM"))
#' res <- extract_f_waves(af)
#'
#' # Never read the rate without the diagnostic beside it
#' res$features[, .(lead, dominant_rate, on_harmonic, cancellation_residual)]
#' }
#'
#' @export
extract_f_waves <- function(
  object,
  lead = NULL,
  qrs_loc = NULL,
  cancel_method = c("spatiotemporal", "average_beat", "adaptive_svd"),
  f_characteristics = c(
    "amplitude",
    "dominant_frequency",
    "organization",
    "sample_entropy"
  ),
  amplitude_window = c("tq", "all"),
  normalize = c("none", "qrs"),
  band = c(4, 10),
  entropy_rate = 50,
  pooled_spectrum = TRUE,
  rhythm = NULL,
  tol = 0.15,
  keep_signal = FALSE,
  verbose = TRUE,
  ...
) {
  # Surface leads only, and at least one of them. Cancellation and every feature
  # below assume a body-surface potential; on an intracardiac channel they return
  # a number rather than an error, which is the failure worth preventing.
  object <- require_ECG(object, what = "Fibrillatory wave extraction")

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

  frequency <- attributes(object$header)$record_line$frequency
  if (is.null(frequency) || !is.finite(frequency) || frequency <= 0) {
    stop("Could not determine a valid sampling frequency from the header")
  }

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

  # A single QRS set shared by every lead
  if (is.null(qrs_loc)) {
    qrs_loc <- shared_qrs_positions(object, filtered, frequency)
  }
  qrs_loc <- sort(unique(as.integer(qrs_loc)))
  qrs_loc <- qrs_loc[qrs_loc >= 1 & qrs_loc <= length(filtered[[1]])]

  rhythm_info <- rhythm_summary(qrs_loc, frequency, rhythm)
  if (verbose && !rhythm_info$af_like) {
    warning(
      "This record does not look like atrial fibrillation (RR CV = ",
      signif(rhythm_info$rr_cv, 2),
      "). Fibrillatory features are only meaningful in AF or flutter; ",
      "in sinus rhythm the estimator returns whatever is largest in the band."
    )
  }

  cancelled <- cancel_ventricular_signal(
    filtered,
    frequency = frequency,
    qrs_loc = qrs_loc,
    method = cancel_method,
    ...
  )

  atrial <- cancelled$atrial
  qrs_loc <- cancelled$qrs_loc

  features <- lapply(report_leads, function(l) {
    analyze_atrial_signal(
      atrial_signal = atrial[[l]],
      frequency = frequency,
      characteristics = f_characteristics,
      original_signal = filtered[[l]],
      qrs_loc = qrs_loc,
      annotation = object$annotation,
      amplitude_window = amplitude_window,
      band = band,
      entropy_rate = entropy_rate,
      tol = tol,
      ...
    )
  })
  names(features) <- report_leads

  # A pooled spectrum has far more segments behind it than any single lead, so
  # the dominant frequency is estimated once for the record and shared.
  if (pooled_spectrum && "dominant_frequency" %in% f_characteristics) {
    pooled <- pooled_dominant_frequency(
      atrial[report_leads],
      frequency = frequency,
      band = band
    )
    median_rr <- rhythm_info$median_rr
    for (l in report_leads) {
      features[[l]]$dominant_rate <- pooled$dominant_frequency * 60
      features[[l]]$harmonic_index <- pooled$dominant_frequency *
        median_rr /
        frequency
      features[[l]]$on_harmonic <- harmonic_flag(
        features[[l]]$harmonic_index,
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
    rhythm = rhythm_info$rhythm,
    spatial_dispersion = spatial_dispersion(feature_table$f_amplitude_p2p)
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
    if (x$record$af_like) " (AF-like)" else " (NOT AF-like)",
    "\n",
    sep = ""
  )
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
shared_qrs_positions <- function(object, filtered, frequency) {
  ann <- resolve_annotation(object$annotation)

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

#' Pull a usable annotation table out of whatever shape is attached
#'
#' An `EGM` object carries annotations as a named list keyed by annotator, but a
#' bare `annotation_table` is also accepted. Where several are present, the one
#' that types its wave boundaries is preferred, since that is what locates TQ
#' segments.
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

  # An annotator that marks wave onsets and offsets carries strictly more
  # information than one that only marks beats
  has_boundaries <- vapply(
    candidates,
    function(x) "number" %in% names(x) && any(x$type %in% c("(", ")")),
    logical(1)
  )
  if (any(has_boundaries)) {
    return(as.data.frame(candidates[[which(has_boundaries)[1]]]))
  }

  as.data.frame(candidates[[1]])
}

#' Snap detected positions onto the local energy maximum
#'
#' Pan-Tompkins reports the peak of the integration window, which lags the true
#' R peak by roughly half that window. Left uncorrected, the misalignment smears
#' the template across beats and weakens every subsequent fit.
#'
#' @noRd
refine_qrs_positions <- function(qrs_loc, reference, frequency, search_ms = 60) {
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
      rhythm = if (is.null(rhythm)) NA_character_ else rhythm
    ))
  }

  median_rr <- stats::median(rr)
  rr_cv <- stats::sd(rr) / mean(rr)
  rr_rmssd <- sqrt(mean(diff(rr)^2)) / mean(rr)

  af_like <- if (!is.null(rhythm)) {
    tolower(rhythm) %in% c("af", "afib", "atrial fibrillation", "flutter", "aflutter")
  } else {
    # Irregularly irregular ventricular response. The threshold is deliberately
    # permissive; it exists to catch obviously regular records, not to diagnose.
    isTRUE(rr_cv >= 0.12 && rr_rmssd >= 0.10)
  }

  list(
    median_rr = median_rr,
    heart_rate = 60 * frequency / median_rr,
    rr_cv = rr_cv,
    rr_rmssd = rr_rmssd,
    af_like = af_like,
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
#' `"adaptive_svd"` is retained for backward compatibility and operates one lead
#' at a time. It is not recommended: a per-lead template leaves a residual that
#' is periodic at the heart rate, which deposits energy on heart-rate harmonics
#' inside the fibrillatory band.
#'
#' Beats are never blanked or interpolated across. A beat that the template
#' models badly is still fitted and subtracted, and the quality of that fit is
#' reported through `cancellation_residual` rather than being hidden by
#' replacing the segment with a straight line.
#'
#' @param signals A named list of numeric vectors, one per lead, already
#'   bandpass filtered and all of the same length.
#' @param frequency Sampling frequency in Hz. Required.
#' @param qrs_loc Integer vector of QRS sample positions shared by all leads.
#'   Detected from a composite if `NULL`.
#' @param method One of `"spatiotemporal"`, `"average_beat"`, `"adaptive_svd"`.
#' @param min_beats Minimum number of beats required before any template-based
#'   subtraction is attempted. Default 3. Below this an "average" beat is
#'   effectively a copy of a single beat, and subtracting it returns zero.
#' @param aberrancy_threshold Correlation below which a beat is called
#'   morphologically aberrant and excluded from template construction. Default
#'   0.9. Aberrancy is judged on QRS morphology, never on RR interval: in atrial
#'   fibrillation the RR interval is irregular by definition, so an RR-based
#'   criterion fires on normally conducted beats in the exact rhythm this
#'   function targets.
#' @param ... Unused, for method compatibility.
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
  method = c("spatiotemporal", "average_beat", "adaptive_svd"),
  min_beats = 3L,
  aberrancy_threshold = 0.9,
  ...
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

  if (method == "adaptive_svd") {
    atrial <- lapply(signals, function(x) {
      remove_qrs_with_adaptive_svd(x, frequency = frequency, qrs_loc = qrs_loc)
    })
    names(atrial) <- names(signals)
    return(list(
      atrial = atrial,
      qrs_loc = qrs_loc,
      n_beats_cancelled = length(qrs_loc),
      n_beats_skipped = 0L,
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

  covered <- weight > .Machine$double.eps
  atrial <- lapply(lead_names, function(l) {
    v <- ventricular[[l]]
    v[covered] <- v[covered] / weight[covered]
    v[!covered] <- 0
    signals[[l]] - v
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

#' Legacy single-lead ventricular removal
#'
#' @description Retained for backward compatibility. `frequency` is now required
#'   rather than silently defaulting to 1000 Hz, which produced QRS detection
#'   filters designed for the wrong Nyquist and adaptive window sizing off by a
#'   factor of two whenever the signal was not sampled at 1000 Hz.
#'
#' @param signal Numeric vector for a single lead
#' @param frequency Sampling frequency in Hz. Required.
#' @param method Either `"adaptive_svd"` or `"ica"`
#' @param ... Passed through to the underlying method
#'
#' @return Numeric vector with ventricular activity suppressed
#' @noRd
remove_ventricular_signal <- function(signal, frequency, method = "adaptive_svd", ...) {
  if (missing(frequency) || is.null(frequency)) {
    stop(
      "`frequency` is required. It was previously assumed to be 1000 Hz, ",
      "which silently produced wrong results at any other sampling rate."
    )
  }

  if (method == "adaptive_svd") {
    remove_qrs_with_adaptive_svd(signal, frequency = frequency, ...)
  } else if (method == "ica") {
    remove_qrs_with_ica(signal, frequency = frequency, ...)
  } else {
    stop("Unsupported method. Choose 'adaptive_svd' or 'ica'")
  }
}

#' Single-lead adaptive SVD cancellation
#'
#' Kept for backward comparison only. See [cancel_ventricular_signal()] for the
#' reason this is not the default.
#'
#' @noRd
remove_qrs_with_adaptive_svd <- function(
  signal,
  frequency,
  qrs_loc = NULL,
  adaptive_window = TRUE,
  smoothing = TRUE,
  min_group = 3L,
  max_components = 3L,
  ...
) {
  if (missing(frequency) || is.null(frequency)) {
    stop("`frequency` is required")
  }

  signal <- as.numeric(signal)

  if (is.null(qrs_loc)) {
    qrs_loc <- detect_QRS(signal, frequency)
  }
  qrs_loc <- sort(unique(as.integer(qrs_loc)))

  if (length(qrs_loc) < min_group) {
    warning(
      "Insufficient QRS complexes for SVD cancellation. Returning original signal."
    )
    return(signal)
  }

  rr_intervals <- diff(qrs_loc)
  median_rr <- stats::median(rr_intervals)

  if (adaptive_window) {
    base_window_ms <- min(
      500,
      max(250, 60000 / (median_rr / frequency * 1000) * 0.2)
    )
    base_window <- round(base_window_ms * frequency / 1000)
  } else {
    base_window <- round(0.5 * frequency)
  }
  half_window <- floor(base_window / 2)

  process_beat_group(
    signal,
    qrs_loc,
    half_window,
    frequency,
    smoothing,
    min_group = min_group,
    max_components = max_components
  )
}

#' Subtract a low-rank model of a group of beats
#'
#' Two guards matter here. The rank is capped well below the group size: the
#' point is a *low-rank* model of the ventricular complex, so a rank that grows
#' with the number of beats defeats it. Left uncapped, a 95 percent variance rule
#' on a two-beat group selects both components, the reconstruction is exact, and
#' the residual is identically zero -- the atrial signal in that window is
#' deleted outright. The group must also be large enough that its "average" beat
#' is not simply a copy of one beat.
#'
#' @noRd
process_beat_group <- function(
  signal,
  beat_indices,
  half_window,
  frequency,
  smoothing = TRUE,
  min_group = 3L,
  max_components = 3L
) {
  n <- length(signal)

  if (length(beat_indices) < min_group) {
    return(signal)
  }

  usable <- beat_indices[
    (beat_indices - half_window) >= 1L & (beat_indices + half_window) <= n
  ]
  if (length(usable) < min_group) {
    return(signal)
  }

  width <- 2L * half_window + 1L
  windows <- lapply(usable, function(p) seq.int(p - half_window, p + half_window))
  segment_matrix <- t(vapply(windows, function(w) signal[w], numeric(width)))

  svd_result <- tryCatch(svd(segment_matrix), error = function(e) NULL)
  if (is.null(svd_result) || !any(is.finite(svd_result$d)) || sum(svd_result$d) == 0) {
    warning("SVD failed or the segment group is degenerate; leaving it unchanged.")
    return(signal)
  }

  # Cap the rank rather than chasing a variance threshold, and never allow the
  # model to reach full rank, which would reconstruct the group exactly.
  n_components <- max(1L, min(
    as.integer(max_components),
    nrow(segment_matrix) - 1L,
    sum(svd_result$d > .Machine$double.eps * svd_result$d[1])
  ))

  template_matrix <- svd_result$u[, 1:n_components, drop = FALSE] %*%
    diag(svd_result$d[1:n_components], nrow = n_components) %*%
    t(svd_result$v[, 1:n_components, drop = FALSE])

  taper <- tukey_window(width, ramp = 0.10)
  ventricular <- numeric(n)
  weight <- numeric(n)

  for (i in seq_along(usable)) {
    w <- windows[[i]]
    ventricular[w] <- ventricular[w] + taper * template_matrix[i, ]
    weight[w] <- weight[w] + taper
  }

  covered <- weight > .Machine$double.eps
  ventricular[covered] <- ventricular[covered] / weight[covered]
  ventricular[!covered] <- 0

  result <- signal - ventricular

  if (smoothing) {
    result <- smooth_savgol(result, frequency)
  }

  result
}

#' Savitzky-Golay smoothing with a graceful fallback
#' @noRd
smooth_savgol <- function(x, frequency, window_sec = 0.015) {
  smooth_window <- round(window_sec * frequency)
  if (smooth_window <= 2) {
    return(x)
  }
  if (smooth_window %% 2 == 0) {
    smooth_window <- smooth_window + 1
  }
  if (smooth_window <= 3) {
    return(x)
  }

  smoothed <- tryCatch(
    {
      kern <- signal::sgolay(p = 3, n = smooth_window, m = 0)
      as.numeric(signal::filter(kern, x))
    },
    error = function(e) x
  )

  na_indices <- which(is.na(smoothed))
  if (length(na_indices) > 0) {
    smoothed[na_indices] <- x[na_indices]
  }
  smoothed
}

#' Remove QRST using Independent Component Analysis
#'
#' A single-lead signal is delay-embedded to create a pseudo-multichannel
#' matrix, then decomposed with FastICA. Components whose high frequency energy
#' rises sharply inside a window around detected QRS peaks are presumed
#' ventricular and zeroed before back-projection.
#'
#' @noRd
remove_qrs_with_ica <- function(
  signal,
  frequency,
  embedding_dim = 5,
  qrs_loc = NULL,
  threshold = 3,
  ...
) {
  if (missing(frequency) || is.null(frequency)) {
    stop("`frequency` is required")
  }

  signal <- as.numeric(signal)
  N <- length(signal)

  if (!requireNamespace("fastICA", quietly = TRUE)) {
    warning("fastICA not installed, falling back to adaptive SVD.")
    return(remove_qrs_with_adaptive_svd(signal, frequency = frequency, qrs_loc = qrs_loc))
  }

  if (is.null(qrs_loc)) {
    qrs_loc <- detect_QRS(signal, frequency)
  }
  if (length(qrs_loc) < 3L) {
    warning("Too few QRS complexes for ICA, returning original signal.")
    return(signal)
  }

  embedding_dim <- max(3, min(embedding_dim, length(qrs_loc) - 1))

  X <- stats::embed(signal, embedding_dim)
  X <- scale(X, center = TRUE, scale = FALSE)

  methods <- list(
    list(alg = "parallel", fun = "logcosh"),
    list(alg = "deflation", fun = "logcosh"),
    list(alg = "parallel", fun = "exp"),
    list(alg = "deflation", fun = "exp")
  )

  ica_result <- NULL
  for (m in methods) {
    result <- tryCatch(
      fastICA::fastICA(
        X,
        n.comp = embedding_dim,
        alg.typ = m$alg,
        fun = m$fun,
        verbose = FALSE
      ),
      error = function(e) NULL
    )
    if (!is.null(result)) {
      ica_result <- result
      break
    }
  }

  if (is.null(ica_result)) {
    warning("ICA decomposition failed. Falling back to adaptive SVD.")
    return(remove_qrs_with_adaptive_svd(signal, frequency = frequency, qrs_loc = qrs_loc))
  }

  S <- ica_result$S
  A <- ica_result$A

  ny <- frequency / 2
  bf <- signal::butter(3, c(20, min(50, ny * 0.95)) / ny, type = "pass")
  win <- round(0.03 * frequency)

  shift <- embedding_dim - 1L
  keep <- qrs_loc[qrs_loc > win + shift & qrs_loc <= N - win] - shift
  if (length(keep) == 0) {
    return(signal)
  }
  idx_qrs <- unlist(lapply(keep, function(p) (p - win):(p + win)))
  idx_qrs <- idx_qrs[idx_qrs >= 1 & idx_qrs <= nrow(S)]

  total_energy <- apply(S, 2, function(comp) mean(comp^2))
  qrs_energy <- apply(S, 2, function(comp) {
    mean(signal::filtfilt(bf, comp)[idx_qrs]^2)
  })

  qrs_comps <- which(qrs_energy / pmax(total_energy, .Machine$double.eps) > threshold)
  if (length(qrs_comps)) {
    S[, qrs_comps] <- 0
  }

  X_clean <- S %*% t(A)
  X_mean <- rowMeans(X_clean)

  cleaned_signal <- c(rep(X_mean[1L], embedding_dim - 1L), X_mean)
  len_diff <- N - length(cleaned_signal)
  if (len_diff > 0) {
    cleaned_signal <- c(
      cleaned_signal,
      rep(cleaned_signal[length(cleaned_signal)], len_diff)
    )
  }

  # No blanking or spline interpolation across the QRS. Replacing a window with
  # a straight line removes every trace of the atrial signal inside it, which is
  # the opposite of what this function is for.
  cleaned_signal[seq_len(N)]
}

# Atrial signal analysis ----

#' Analyse an isolated atrial signal
#'
#' @param atrial_signal Numeric vector of the cancelled (atrial) signal
#' @param frequency Sampling frequency in Hz
#' @param characteristics Character vector of features to compute
#' @param original_signal The uncancelled, filtered signal for the same lead.
#'   Needed for the QRS excursion used in amplitude normalisation and for the
#'   cancellation residual.
#' @param qrs_loc Integer vector of QRS positions
#' @param annotation Optional annotation table used to locate TQ segments
#' @param amplitude_window Either `"tq"` or `"all"`
#' @param band Numeric length-2 frequency band in Hz
#' @param entropy_rate Rate in Hz to decimate to before computing entropy
#' @param tol Tolerance for the harmonic test
#' @param ... Unused
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
  qrs_loc = NULL,
  annotation = NULL,
  amplitude_window = c("tq", "all"),
  band = c(4, 10),
  entropy_rate = 50,
  tol = 0.15,
  ...
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
      original_signal = original_signal,
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
    # the heart rate, so the index comes out an integer.
    if (!is.null(qrs_loc) && length(qrs_loc) >= 2) {
      median_rr <- stats::median(diff(qrs_loc))
      results$harmonic_index <- df * median_rr / frequency
      results$on_harmonic <- harmonic_flag(results$harmonic_index, tol)
    } else {
      results$harmonic_index <- NA_real_
      results$on_harmonic <- NA
    }

    if ("organization" %in% characteristics) {
      results$organization_index <- calculate_organization_index(
        atrial_signal,
        frequency = frequency,
        dominant_frequency = df
      )
    }
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

#' Decimate ahead of an O(n^2) entropy calculation
#' @noRd
decimate_for_entropy <- function(x, frequency, entropy_rate) {
  if (is.null(entropy_rate) || !is.finite(entropy_rate) || entropy_rate >= frequency) {
    return(x)
  }
  as.numeric(change_frequency(as.numeric(x), from = frequency, to = entropy_rate))
}

#' Flag a spectral peak that sits on a heart-rate harmonic
#' @noRd
harmonic_flag <- function(harmonic_index, tol = 0.15) {
  if (is.null(harmonic_index) || !is.finite(harmonic_index)) {
    return(NA)
  }
  abs(harmonic_index - round(harmonic_index)) < tol
}

# Amplitude ----

#' Locate TQ segments, where the ventricles are electrically silent
#'
#' Prefers an ecgpuwave-style annotation, which types each wave boundary
#' (`number`: 0 = P, 1 = QRS, 2 = T), so a TQ segment runs from a T offset to
#' the next QRS onset. Falls back to a fixed exclusion window around each
#' detected QRS.
#'
#' @noRd
tq_segments <- function(n, frequency, qrs_loc = NULL, annotation = NULL) {
  from_annotation <- NULL
  ann <- resolve_annotation(annotation)

  if (!is.null(ann)) {
    if (all(c("sample", "type", "number") %in% names(ann))) {
      t_off <- ann$sample[ann$type == ")" & ann$number == 2]
      qrs_on <- ann$sample[ann$type == "(" & ann$number == 1]

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
#' @noRd
amplitude_features <- function(
  atrial_signal,
  original_signal,
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
      qrs_amplitude = qrs_excursion(original_signal, qrs_loc, frequency),
      f_ratio = NA_real_,
      tq_fraction = 0
    ))
  }

  idx <- unlist(lapply(segs, function(s) seq.int(s[1], s[2])))
  idx <- idx[idx >= 1 & idx <= n]

  # Peak-to-peak per segment, then the median across segments. This is the
  # measure the coarse- versus fine-AF literature uses.
  p2p <- vapply(
    segs,
    function(s) {
      v <- atrial_signal[seq.int(s[1], s[2])]
      if (length(v) < 2 || all(!is.finite(v))) NA_real_ else diff(range(v, na.rm = TRUE))
    },
    numeric(1)
  )

  qrs_amp <- qrs_excursion(original_signal, qrs_loc, frequency)
  p2p_median <- stats::median(p2p, na.rm = TRUE)

  list(
    f_amplitude_p2p = p2p_median,
    f_amplitude_rms = sqrt(mean(atrial_signal[idx]^2, na.rm = TRUE)),
    qrs_amplitude = qrs_amp,
    # Dividing by the QRS excursion in the same lead cancels the thoracic
    # transfer function to first order, since the ventricular signal traverses
    # the same tissue. That matters for comparison between patients, where raw
    # amplitude is dominated by body habitus rather than by atrial physiology.
    f_ratio = if (is.finite(qrs_amp) && qrs_amp > 0) p2p_median / qrs_amp else NA_real_,
    tq_fraction = length(idx) / n
  )
}

#' Median QRS peak-to-peak excursion, measured on the uncancelled signal
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

#' Coefficient of variation of f-wave amplitude across leads
#'
#' A uniform fibrillatory field and a regionally organised one differ here even
#' when their amplitude in any single lead is identical.
#'
#' @noRd
spatial_dispersion <- function(amplitudes) {
  a <- amplitudes[is.finite(amplitudes)]
  if (length(a) < 2) {
    return(NA_real_)
  }
  m <- mean(a)
  if (m == 0) {
    return(NA_real_)
  }
  stats::sd(a) / m
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
    "harmonic_index",
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
  # Normalisation is never the silent default, since it changes the units.
  if (normalize == "qrs" && "f_ratio" %in% names(dt)) {
    data.table::set(dt, j = "f_amplitude", value = dt$f_ratio)
  } else if ("f_amplitude_p2p" %in% names(dt)) {
    data.table::set(dt, j = "f_amplitude", value = dt$f_amplitude_p2p)
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
  x <- x[is.finite(x)]
  n <- length(x)

  if (n < 8) {
    stop("Signal is too short for a spectral estimate")
  }

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
#' @description The share of 2.5-15 Hz power that sits at the dominant frequency
#'   and its first harmonic.
#'
#' @details A highly organised atrium concentrates its energy in a narrow peak
#'   and its harmonic; a disorganised one spreads it across the band. This is one
#'   of the few f-wave features with a reasonably direct electrophysiological
#'   reading, being related to the number of independent wavefronts the atrium is
#'   holding.
#'
#' @param x Numeric vector of the atrial signal
#' @param frequency Sampling frequency in Hz
#' @param dominant_frequency Dominant frequency in Hz. Estimated from `x` if
#'   `NULL`.
#' @param band Numeric length-2 vector for the total-power reference band.
#'   Default `c(2.5, 15)`.
#' @param half_width Half-width in Hz of the window placed on the dominant
#'   frequency. The harmonic window is 1.5 times as wide. Default 0.5.
#'
#' @return Organisation index between 0 and 1
#'
#' @references
#' Everett TH 4th, Kok LC, Vaughn RH, Moorman JR, Haines DE. Frequency domain
#' algorithm for quantifying atrial fibrillation organization to increase
#' defibrillation efficacy. *IEEE Transactions on Biomedical Engineering*.
#' 2001;48(9):969-978. \doi{10.1109/10.942586}
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
  band = c(2.5, 15),
  half_width = 0.5
) {
  psd <- calculate_welch_spectrum(x, frequency = frequency)

  if (is.null(dominant_frequency)) {
    dominant_frequency <- calculate_dominant_frequency(x, frequency = frequency)
  }
  if (!is.finite(dominant_frequency)) {
    return(NA_real_)
  }

  total_idx <- which(psd$freq >= band[1] & psd$freq <= band[2])
  total <- sum(psd$spec[total_idx], na.rm = TRUE)
  if (!is.finite(total) || total <= 0) {
    return(NA_real_)
  }

  in_window <- function(centre, hw) {
    which(psd$freq >= centre - hw & psd$freq <= centre + hw)
  }

  peak_idx <- in_window(dominant_frequency, half_width)
  harm_idx <- in_window(2 * dominant_frequency, half_width * 1.5)
  harm_idx <- setdiff(harm_idx, peak_idx)
  harm_idx <- intersect(harm_idx, total_idx)
  peak_idx <- intersect(peak_idx, total_idx)

  concentrated <- sum(psd$spec[peak_idx], na.rm = TRUE) +
    sum(psd$spec[harm_idx], na.rm = TRUE)

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
#' 5. Adaptive thresholding to identify peaks
#' 6. Application of a refractory period to prevent multiple detections of the
#' same QRS complex
#'
#' Positions are reported at the peak of the integration window, which lags the
#' true R peak. Where beat alignment matters, refine them against the raw signal
#' before use.
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
  is_peak <- (integrated_signal > threshold) &
    (c(FALSE, integrated_signal[-length(integrated_signal)] < integrated_signal[-1])) &
    (c(integrated_signal[-1] > integrated_signal[-length(integrated_signal)], FALSE))
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
#'   The calculation is O(n^2). Decimate the signal to a rate matched to the
#'   analysis band before calling; for a fibrillatory band below 10 Hz, 50 Hz is
#'   still five times oversampled and is hundreds of times cheaper than the raw
#'   rate.
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
#' @examples
#' set.seed(123)
#' calculate_sample_entropy(rnorm(500))
#'
#' @export
calculate_sample_entropy <- function(x, m = 2, r = NULL) {
  x <- as.double(x)
  x <- x[is.finite(x)]

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
#'   The calculation is O(n^2). Decimate to a rate matched to the analysis band
#'   before calling.
#'
#' @param x Numeric vector of the time series
#' @param m Embedding dimension. Default 2.
#' @param r Tolerance. Default `NULL`, which uses 0.2 times the standard
#'   deviation of `x`.
#' @param implementation Either `"C++"` (default, faster) or `"R"`
#'
#' @return Approximate entropy value
#'
#' @references Pincus SM. Approximate entropy as a measure of system complexity.
#'   *Proceedings of the National Academy of Sciences of the USA*.
#'   1991;88(6):2297-2301. \doi{10.1073/pnas.88.6.2297}
#'
#' @examples
#' set.seed(123)
#' calculate_approximate_entropy(rnorm(500), implementation = "R")
#'
#' @export
calculate_approximate_entropy <- function(x, m = 2, r = NULL, implementation = "C++") {
  x <- as.double(x)
  x <- x[is.finite(x)]

  if (length(x) < m + 2) {
    return(NA_real_)
  }

  if (implementation == "C++") {
    calculate_apen_cpp(x, m, r)
  } else if (implementation == "R") {
    calculate_apen_r(x, m, r)
  } else {
    stop("Invalid method specified. Choose 'R' or 'C++'")
  }
}

#' C++ implementation of approximate entropy
#' @noRd
calculate_apen_cpp <- function(x, m, r) {
  # -1 is the flag telling the C++ side to compute the tolerance itself; NULL
  # cannot cross the boundary
  if (is.null(r)) {
    r <- -1
  }
  calculate_approximate_entropy_cpp(x, as.integer(m), as.double(r))
}

#' R implementation of approximate entropy
#' @noRd
calculate_apen_r <- function(x, m, r) {
  N <- length(x)
  r <- if (is.null(r)) 0.2 * stats::sd(x) else r
  x <- as.vector(x)

  embed_matrix <- function(x, m) {
    matrix(sapply(1:m, function(i) x[i:(N - m + i)]), ncol = m)
  }

  correlation_integral <- function(x, r) {
    N <- nrow(x)
    count <- sapply(1:N, function(i) {
      sum(apply(abs(x - rep(x[i, ], each = nrow(x))), 1, max) <= r)
    })
    sum(log(count / N)) / N
  }

  phi_m <- correlation_integral(embed_matrix(x, m), r)
  phi_m1 <- correlation_integral(embed_matrix(x, m + 1), r)

  phi_m - phi_m1
}

#' Diagnostics returned with every fibrillatory estimate
#'
#' @description Not a function. This documents the diagnostic fields that
#'   [extract_f_waves()] returns beside each spectral feature, and why they must
#'   be read together with it.
#'
#' @details
#'
#' \describe{
#'   \item{`harmonic_index`}{Dominant frequency times the median RR interval. If
#'     the peak is residual ventricular energy rather than atrial activity, it
#'     sits on an integer multiple of the heart rate, so this number comes out
#'     an integer.}
#'   \item{`on_harmonic`}{`TRUE` when `harmonic_index` is within `tol` of an
#'     integer. **`dominant_rate` must not be used without conditioning on
#'     this.** A contaminated estimate is precise, wrong, and highly reproducible,
#'     because heart rate is highly reproducible within a patient. Validating the
#'     feature by test-retest reliability will therefore select the artifact.}
#'   \item{`cancellation_residual`}{Residual energy in a window around each QRS
#'     as a fraction of that window's energy before cancellation. Model-free, so
#'     it is independent of any assumption about the atrial spectrum.}
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
#' @name f_wave_diagnostics
NULL
