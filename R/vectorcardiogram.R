# Vectorcardiography -----------------------------------------------------------

# A vectorcardiogram is the heart's dipole traced through three orthogonal leads.
# Frank's electrode array records it directly and is essentially never placed;
# what is recorded is the 12-lead ECG, from which the orthogonal leads are
# reconstructed by a fixed linear map. Kors' regression matrix is that map.
#
# Only eight leads enter it. III, aVR, aVL and aVF are exact linear combinations
# of I and II, so a matrix that used all twelve would be rank deficient and the
# extra columns would carry no information.
#
# Rows are X (left), Y (inferior), Z (posterior); columns are the source leads.
.kors <- rbind(
  X = c(0.38, -0.07, -0.13, 0.05, -0.01, 0.14, 0.06, 0.54),
  Y = c(-0.07, 0.93, 0.06, -0.02, -0.05, 0.06, -0.17, 0.13),
  Z = c(0.11, -0.23, -0.43, -0.06, -0.14, -0.20, -0.11, 0.31)
)
colnames(.kors) <- c("I", "II", "V1", "V2", "V3", "V4", "V5", "V6")

#' Reconstruct a vectorcardiogram from the 12-lead ECG
#'
#' @description `vectorcardiogram()` traces the ventricular (QRS) loop and
#'   `atrial_vectorcardiogram()` the atrial (P) loop, both by applying the Kors
#'   regression transformation to the surface ECG and cutting the result at the
#'   annotated wave boundaries. Each returns the orthogonal X, Y, Z signal
#'   together with the standard descriptors of the loop it traces.
#'
#' @details
#'
#' # Reconstruction
#'
#' The Kors regression matrix (Kors et al. 1990) maps eight leads - I, II and
#' V1-V6 - onto the orthogonal axes of the Frank system. The remaining four
#' limb leads are exact linear combinations of I and II and carry no independent
#' information, so all eight are required and no substitute exists for a missing
#' one. Kors' matrix is preferred to the inverse Dower matrix: it reproduces the
#' recorded Frank leads more closely and yields derived measures with more
#' prognostic power (Man et al. 2011; Kück et al. 2018).
#'
#' The transformation is linear, so it commutes with windowing: a loop cut from
#' the transformed record and a loop transformed after cutting are the same
#' signal, and the record is therefore transformed once, after segmentation. It
#' does not commute with the median, which is not a linear operator. The median
#' is taken lead by lead first, so `beats = "median"` returns the loop of the
#' median beat rather than the median of the beats' loops.
#'
#' # Segmentation
#'
#' Wave boundaries come from the record's annotations, since neither loop can be
#' delimited without them. Windows run from wave onset to wave offset and must
#' enclose the corresponding peak, using the same machinery as [get_windows()];
#' a record with no delineation annotations is an error rather than a guess.
#'
#' `beats = "median"` collapses the beats to a single loop before transforming,
#' which is the signal-averaged form used to characterise atrial conduction
#' (Havmöller et al. 2007). `beats = "all"` returns one loop per beat, which
#' preserves the beat-to-beat variability that a signal average is designed to
#' remove and that itself carries information about the atrial substrate
#' (Tachmatzidis et al. 2022).
#'
#' # Interpretation
#'
#' The P loop is small - roughly a tenth of the QRS in amplitude - so it is far
#' more sensitive to baseline wander and to the accuracy of the P-onset
#' annotation than the QRS loop is. `magnitude_peak` on the P loop is the spatial
#' P-wave vector magnitude, which falls as the left atrium remodels and which
#' tracks low-voltage area at electroanatomic mapping (Yano et al. 2023).
#'
#' A P loop only exists in an organised atrial rhythm. In atrial fibrillation
#' there is no P wave to delineate, and the atrial signal is characterised
#' instead by [extract_f_waves()].
#'
#' @param object An object of class `EGM` or of subclass [ECG]. An `EGM` from an
#'   electrophysiology study is reduced to its surface leads first (see
#'   [as_ECG()]). All eight leads of the Kors transformation must be present.
#'
#' @param beats Which beats to trace. `"median"` (default) returns a single loop
#'   from the median beat; `"all"` returns one loop per beat.
#'
#' @param channel An optional annotation channel guiding the wave delineation,
#'   passed to [by_rhythm()]. Required when the annotations span more than one
#'   channel, as they do when an annotator has been run per lead.
#'
#' @param baseline Logical. If `TRUE` (default), each loop is referenced to its
#'   own onset, taken as the median of the first 10 ms. Loop orientation is
#'   measured from the origin, so an offset baseline rotates every angle.
#'
#' @return An object of class `vectorcardiogram`, a list with:
#'
#'   \describe{
#'     \item{`loop`}{A `data.table` of the orthogonal signal with columns `beat`,
#'       `sample`, `X`, `Y`, and `Z`, in the units of the source signal.}
#'     \item{`metrics`}{A `data.table` with one row per loop, holding `duration`
#'       (seconds), `magnitude_peak` and `magnitude_mean` (spatial vector
#'       magnitude), `azimuth` and `elevation` (orientation of the peak vector,
#'       in degrees), `area` (planar area enclosed by the loop), and `planarity`
#'       (share of the loop's variance lying in its best-fit plane, 1 being
#'       perfectly planar).}
#'     \item{`wave`}{Either `"QRS"` or `"P"`.}
#'     \item{`beats`}{Either `"median"` or `"all"`.}
#'     \item{`frequency`}{Sampling frequency in Hz.}
#'   }
#'
#' @references
#'
#' Kors JA, van Herpen G, Sittig AC, van Bemmel JH. Reconstruction of the Frank
#' vectorcardiogram from standard electrocardiographic leads: diagnostic
#' comparison of different methods. *European Heart Journal*.
#' 1990;11(12):1083-1092. \doi{10.1093/oxfordjournals.eurheartj.a059647}
#'
#' Man S, Algra AM, Schreurs CA, et al. Influence of the vectorcardiogram
#' synthesis matrix on the power of the electrocardiogram-derived spatial QRS-T
#' angle to predict arrhythmias in patients with ischemic heart disease and
#' systolic left ventricular dysfunction. *Journal of Electrocardiology*.
#' 2011;44(4):410-415. \doi{10.1016/j.jelectrocard.2011.04.007}
#'
#' Kück K, Isaksen JL, Graff C, et al. Spatial QRS-T angle variants for
#' prediction of all-cause mortality. *Journal of Electrocardiology*.
#' 2018;51(5):768-775. \doi{10.1016/j.jelectrocard.2018.05.011}
#'
#' Havmöller R, Carlson J, Holmqvist F, et al. Age-related changes in P wave
#' morphology in healthy subjects. *BMC Cardiovascular Disorders*. 2007;7:22.
#' \doi{10.1186/1471-2261-7-22}
#'
#' Holmqvist F, Platonov PG, Carlson J, et al. Variable interatrial conduction
#' illustrated in a hypertrophic cardiomyopathy population. *Annals of
#' Noninvasive Electrocardiology*. 2007;12(3):227-236.
#' \doi{10.1111/j.1542-474X.2007.00166.x}
#'
#' Yano M, Egami Y, Kawanami S, et al. Ratio of P-wave duration to P-wave
#' amplitude and left atrial remodeling. *The American Journal of Cardiology*.
#' 2023;212:109-117. \doi{10.1016/j.amjcard.2023.11.046}
#'
#' Tachmatzidis D, Tsarouchas A, Mouselimis D, et al. P-wave beat-to-beat
#' analysis to predict atrial fibrillation recurrence after catheter ablation.
#' *Diagnostics*. 2022;12(4):830. \doi{10.3390/diagnostics12040830}
#'
#' @seealso [as_ECG()] for the surface lead contract, [get_windows()] for the
#'   segmentation, [extract_f_waves()] for the atrial signal when there is no P
#'   wave to trace.
#'
#' @examples
#' \dontrun{
#' ecg <- read_wfdb("muse-sinus", system.file("extdata", package = "EGM"),
#'                  annotator = "ecgpuwave")
#'
#' # Ventricular loop of the median beat
#' vectorcardiogram(ecg)
#'
#' # Atrial loop, beat by beat
#' atrial_vectorcardiogram(ecg, beats = "all")$metrics
#' }
#'
#' @name vectorcardiogram
#' @export
vectorcardiogram <- function(
  object,
  beats = c("median", "all"),
  channel = NULL,
  baseline = TRUE
) {
  vcg_from_wave(object, "QRS", match.arg(beats), channel, baseline)
}

#' @rdname vectorcardiogram
#' @export
atrial_vectorcardiogram <- function(
  object,
  beats = c("median", "all"),
  channel = NULL,
  baseline = TRUE
) {
  vcg_from_wave(object, "P", match.arg(beats), channel, baseline)
}

#' Trace the loop of a single wave
#'
#' @description Shared engine behind [vectorcardiogram()] and
#'   [atrial_vectorcardiogram()]. The two differ only in which annotations
#'   delimit the wave.
#'
#' @inheritParams vectorcardiogram
#' @param wave Either `"QRS"` or `"P"`.
#'
#' @return A `vectorcardiogram` object.
#'
#' @keywords internal
vcg_from_wave <- function(object, wave, beats, channel, baseline) {
  what <- paste0("The ", wave, " vectorcardiogram")
  object <- require_ECG(object, leads = colnames(.kors), what = what)

  frequency <- attributes(object$header)$record_line$frequency
  peak <- if (wave == "QRS") "N" else "p"

  annotation <- get_single_annotation(object)
  if (nrow(annotation) == 0) {
    stop(
      what,
      " requires wave delineation annotations to find the ",
      wave,
      " boundaries; none are attached to this record",
      call. = FALSE
    )
  }

  # Per-lead annotators leave one set of boundaries per channel. Without a
  # guiding channel the onsets of twelve leads interleave and the windows they
  # imply are not beats, so the choice is the caller's to make.
  if (is.null(channel) && !is.null(annotation$channel)) {
    annotated <- unique(annotation$channel[annotation$channel != 0L])
    if (length(annotated) > 1) {
      stop(
        what,
        " needs a guiding `channel`: annotations span channels ",
        paste(sort(annotated), collapse = ", "),
        call. = FALSE
      )
    }
  }

  # `rhythm = "sinus"` is chosen for its overlap rejection rather than as a claim
  # about the rhythm: it discards any window that encloses a second onset, which
  # is how a beat with a dropped offset annotation is kept out of the stack.
  windows <- get_windows(
    object,
    by = by_rhythm(
      rhythm = "sinus",
      onset = list(type = "(", wave = wave),
      offset = list(type = ")", wave = wave),
      reference = list(type = peak),
      channel = channel
    )
  )

  if (length(windows) == 0) {
    stop("No complete ", wave, " waves could be delineated", call. = FALSE)
  }

  if (beats == "median") {
    windows <- list(median_window(
      windows,
      align_feature = peak,
      channel_criteria = channel
    ))
  }

  loops <- lapply(windows, function(w) kors_transform(w$signal, frequency, baseline))

  loop <- data.table::rbindlist(lapply(seq_along(loops), function(i) {
    data.table::data.table(
      beat = i,
      sample = seq_len(nrow(loops[[i]])) - 1L,
      data.table::as.data.table(loops[[i]])
    )
  }))

  metrics <- data.table::rbindlist(lapply(seq_along(loops), function(i) {
    data.table::data.table(beat = i, vcg_loop_metrics(loops[[i]], frequency))
  }))

  structure(
    list(
      loop = loop,
      metrics = metrics,
      wave = wave,
      beats = beats,
      frequency = frequency
    ),
    class = c("vectorcardiogram", "list")
  )
}

#' Apply the Kors regression transformation
#'
#' @description Maps the eight independent leads of a `signal_table` onto the
#'   orthogonal X, Y, Z axes.
#'
#' @param signal A `signal_table` holding at least the leads in `.kors`.
#' @param frequency Sampling frequency in Hz.
#' @param baseline Logical, whether to reference the loop to its own onset.
#'
#' @return A numeric matrix with columns `X`, `Y`, and `Z`.
#'
#' @keywords internal
kors_transform <- function(signal, frequency, baseline = TRUE) {
  leads <- colnames(.kors)
  xyz <- vapply(leads, function(l) as.numeric(signal[[l]]), numeric(nrow(signal)))
  xyz <- matrix(xyz, ncol = length(leads)) %*% t(.kors)

  if (baseline) {
    # Onset of the wave, taken over 10 ms so that a single noisy sample cannot
    # displace the whole loop. Angles are measured from the origin, so an offset
    # baseline rotates all of them.
    n <- min(nrow(xyz), max(1L, round(frequency * 0.01)))
    xyz <- sweep(xyz, 2, apply(xyz[seq_len(n), , drop = FALSE], 2, stats::median))
  }

  xyz
}

#' Describe a vectorcardiographic loop
#'
#' @description The standard spatial descriptors of a single loop: how large it
#'   is, where it points, how much area it encloses, and how flat it is.
#'
#' @param xyz A numeric matrix with columns `X`, `Y`, and `Z`.
#' @param frequency Sampling frequency in Hz.
#'
#' @return A one-row `data.table`.
#'
#' @keywords internal
vcg_loop_metrics <- function(xyz, frequency) {
  magnitude <- sqrt(rowSums(xyz^2))
  peak <- which.max(magnitude)
  degrees <- 180 / pi

  # Orientation of the peak vector. Azimuth is measured in the transverse plane
  # from +X (left) toward +Z (posterior); elevation is the angle out of that
  # plane toward +Y (inferior).
  azimuth <- atan2(xyz[peak, "Z"], xyz[peak, "X"]) * degrees
  elevation <- atan2(
    xyz[peak, "Y"],
    sqrt(xyz[peak, "X"]^2 + xyz[peak, "Z"]^2)
  ) * degrees

  # Vector area of the closed polygon, A = 1/2 * sum(r_i x r_i+1). Its magnitude
  # is the area a planar loop encloses, and the area of the best-fit projection
  # otherwise. Taking the magnitude of the summed cross products rather than
  # summing their magnitudes is what makes a figure-of-eight loop cancel, as it
  # should.
  area <- NA_real_
  planarity <- NA_real_
  if (nrow(xyz) >= 3) {
    nxt <- c(seq_len(nrow(xyz))[-1], 1L)
    area <- sqrt(sum(colSums(cbind(
      xyz[, "Y"] * xyz[nxt, "Z"] - xyz[, "Z"] * xyz[nxt, "Y"],
      xyz[, "Z"] * xyz[nxt, "X"] - xyz[, "X"] * xyz[nxt, "Z"],
      xyz[, "X"] * xyz[nxt, "Y"] - xyz[, "Y"] * xyz[nxt, "X"]
    ))^2)) / 2

    # Share of the loop's variance captured by its best-fit plane. A loop that
    # bulges out of that plane - the non-dipolar content of a diseased atrium -
    # falls away from 1.
    variance <- eigen(stats::cov(xyz), symmetric = TRUE, only.values = TRUE)$values
    if (sum(variance) > 0) {
      planarity <- sum(variance[1:2]) / sum(variance)
    }
  }

  data.table::data.table(
    duration = nrow(xyz) / frequency,
    magnitude_peak = magnitude[peak],
    magnitude_mean = mean(magnitude),
    azimuth = azimuth,
    elevation = elevation,
    area = area,
    planarity = planarity
  )
}

#' @export
print.vectorcardiogram <- function(x, ...) {
  cat("<vectorcardiogram>\n")
  cat(
    "  ",
    x$wave,
    " loop from ",
    nrow(x$metrics),
    if (x$beats == "median") " median beat" else " beats",
    ", ",
    x$frequency,
    " Hz\n",
    sep = ""
  )
  print(x$metrics)
  invisible(x)
}
