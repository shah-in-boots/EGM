# Vectorcardiography -----------------------------------------------------------

#' Kors regression transformation matrix
#'
#' @description The linear map from the 12-lead ECG to the orthogonal X, Y, Z
#'   leads of the Frank vectorcardiographic system, used by
#'   [vectorcardiogram()] and [atrial_vectorcardiogram()].
#'
#' @details Only eight leads enter the transformation. III, aVR, aVL and aVF are
#'   exact linear combinations of I and II, so a matrix over all twelve would be
#'   rank deficient and the extra columns would carry nothing.
#'
#'   Kors' matrix is preferred to the inverse Dower matrix: it reproduces the
#'   recorded Frank leads more closely, and measures derived from it carry more
#'   prognostic information (Man et al. 2011; Kück et al. 2018).
#'
#' @format A 3 by 8 `numeric` matrix. Rows are the orthogonal axes `X` (positive
#'   to the left), `Y` (positive inferiorly) and `Z` (positive posteriorly);
#'   columns are the source leads `I`, `II` and `V1` through `V6`.
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
#' @examples
#' # The orthogonal leads are row combinations of the eight source leads
#' kors["Z", ]
#'
#' @seealso [vectorcardiogram()]
"kors"

#' Reconstruct a vectorcardiogram from the 12-lead ECG
#'
#' @description `vectorcardiogram()` traces the ventricular loop and
#'   `atrial_vectorcardiogram()` the atrial loop, both by applying the [kors]
#'   transformation to the surface ECG and cutting the result at the annotated
#'   wave boundaries. Each returns the orthogonal X, Y, Z signal together with
#'   the components extracted from it.
#'
#' @details
#'
#' # Reconstruction
#'
#' The transformation is linear, so it commutes with the segmentation: the
#' record is transformed once and cut afterwards, which is the same signal as
#' cutting first and transforming each beat. It does not commute with the
#' median, which is not a linear operator, so `beats = "median"` returns the
#' median of the beats' *loops* rather than the loop of the median beat. This is
#' the order used for signal-averaged orthogonal P-wave analysis (Havmöller et
#' al. 2007).
#'
#' # Segmentation
#'
#' Wave boundaries come from the record's own delineation annotations, since
#' neither loop can be delimited without them; a record without them is an error
#' rather than a guess. Beats are anchored on the wave peak - the QRS peak for
#' the ventricular loop, the P peak for the atrial one - and for
#' `beats = "median"` every beat is placed on a common grid at that anchor and
#' reduced sample by sample. The median beat's own boundaries are the medians of
#' the individual ones, so it has a duration rather than the union of all
#' durations.
#'
#' # Components
#'
#' Both loops yield the same geometric components: the peak and mean spatial
#' vectors with their orientation, the area the loop encloses, and how far it
#' departs from a plane.
#'
#' The ventricular loop additionally yields the global electric heterogeneity
#' (GEH) components, which describe the discordance between depolarization and
#' repolarization and so need the T wave to also be delineated: the spatial
#' QRS-T angle in its peak and mean forms, the spatial ventricular gradient
#' (magnitude, azimuth and elevation), and the sum absolute QRST integral (Waks
#' et al. 2016). Where the T wave is not delineated they are returned as `NA`
#' rather than dropped.
#'
#' `magnitude_peak` on the atrial loop is the spatial P-wave vector magnitude,
#' which falls as the left atrium remodels and tracks low-voltage area at
#' electroanatomic mapping (Yano et al. 2023). A P loop only exists in an
#' organised atrial rhythm; in atrial fibrillation the atrial signal is
#' characterised by [extract_f_waves()] instead.
#'
#' @param object An object of class `EGM` or of subclass [ECG]. An `EGM` from an
#'   electrophysiology study is reduced to its surface leads first (see
#'   [as_ECG()]). All eight leads of the [kors] transformation must be present.
#'
#' @param beats Which beats to trace. `"median"` (default) returns a single
#'   loop; `"all"` returns one loop per beat, preserving the beat-to-beat
#'   variability a signal average is designed to remove (Tachmatzidis et al.
#'   2022).
#'
#' @param channel An optional annotation channel guiding the wave delineation.
#'   Required when the annotations span more than one channel, as they do when
#'   an annotator has been run per lead.
#'
#' @param baseline Logical. If `TRUE` (default), each beat is referenced to its
#'   own onset, taken as the median of the first 10 ms. Orientation is measured
#'   from the origin, so an offset baseline rotates every angle.
#'
#' @return A `list` of two `data.table`s:
#'
#'   \describe{
#'     \item{`loop`}{The orthogonal signal, with columns `beat`, `sample`, `X`,
#'       `Y` and `Z`, in the units of the source signal.}
#'     \item{`components`}{One row per loop. Both functions return `duration`
#'       (seconds); `magnitude_peak`, `azimuth_peak` and `elevation_peak` for the
#'       largest spatial vector; `magnitude_mean`, `azimuth_mean` and
#'       `elevation_mean` for the loop's mean vector; `area`, the planar area
#'       enclosed; and `planarity`, the share of the loop's variance lying in its
#'       best-fit plane, 1 being flat. `vectorcardiogram()` adds the GEH
#'       components `qrst_angle_peak`, `qrst_angle_mean`, `svg_magnitude`,
#'       `svg_azimuth`, `svg_elevation` and `sai_qrst`.}
#'   }
#'
#'   Azimuth is measured in the transverse plane from `+X` toward `+Z`, and
#'   elevation out of that plane toward `+Y`, both in degrees.
#'
#' @references
#'
#' Kors JA, van Herpen G, Sittig AC, van Bemmel JH. Reconstruction of the Frank
#' vectorcardiogram from standard electrocardiographic leads: diagnostic
#' comparison of different methods. *European Heart Journal*.
#' 1990;11(12):1083-1092. \doi{10.1093/oxfordjournals.eurheartj.a059647}
#'
#' Waks JW, Sitlani CM, Soliman EZ, et al. Global electric heterogeneity risk
#' score for prediction of sudden cardiac death in the general population: the
#' Atherosclerosis Risk in Communities (ARIC) and Cardiovascular Health (CHS)
#' studies. *Circulation*. 2016;133(23):2222-2234.
#' \doi{10.1161/CIRCULATIONAHA.116.021306}
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
#' @seealso [kors] for the transformation itself, [as_ECG()] for the surface
#'   lead contract, [extract_f_waves()] for the atrial signal when there is no P
#'   wave to trace.
#'
#' @examples
#' \dontrun{
#' ecg <- read_wfdb("muse-sinus", system.file("extdata", package = "EGM"),
#'                  annotator = "ecgpuwave")
#'
#' vectorcardiogram(ecg)$components
#' atrial_vectorcardiogram(ecg, beats = "all")$components
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
  traced <- trace_loops(
    object,
    waves = c("QRS", "T"),
    beats = match.arg(beats),
    channel = channel,
    baseline = baseline,
    what = "The vectorcardiogram"
  )

  qrs <- lapply(traced$beats, function(b) b$segment$QRS)
  components <- data.table::rbindlist(lapply(
    seq_along(traced$beats),
    function(i) {
      beat <- traced$beats[[i]]

      # GEH describes the discordance between depolarization and repolarization,
      # so all of it needs the T wave. Absent, the QRS loop still stands.
      geh <- data.table::data.table(
        qrst_angle_peak = NA_real_,
        qrst_angle_mean = NA_real_,
        svg_magnitude = NA_real_,
        svg_azimuth = NA_real_,
        svg_elevation = NA_real_,
        sai_qrst = NA_real_
      )

      if (!is.null(beat$segment$T)) {
        # Peak vectors are the largest of each loop, mean vectors their centroid;
        # the spatial QRS-T angle is taken between each pair
        qrs_peak <- beat$segment$QRS[which.max(magnitudes(beat$segment$QRS)), ]
        t_peak <- beat$segment$T[which.max(magnitudes(beat$segment$T)), ]
        qrs_mean <- colMeans(beat$segment$QRS)
        t_mean <- colMeans(beat$segment$T)

        # The beat spans QRS onset to T offset, so the ventricular gradient is
        # its integral and the sum absolute QRST integral the non-directional
        # counterpart, both in signal units x seconds
        svg <- colSums(beat$xyz) / traced$frequency
        svg_angles <- orientation(svg)

        geh <- data.table::data.table(
          qrst_angle_peak = spatial_angle(qrs_peak, t_peak),
          qrst_angle_mean = spatial_angle(qrs_mean, t_mean),
          svg_magnitude = sqrt(sum(svg^2)),
          svg_azimuth = svg_angles[["azimuth"]],
          svg_elevation = svg_angles[["elevation"]],
          sai_qrst = sum(abs(beat$xyz)) / traced$frequency
        )
      }

      data.table::data.table(
        beat = i,
        loop_components(beat$segment$QRS, traced$frequency),
        geh
      )
    }
  ))

  list(loop = stack_loops(qrs), components = components)
}

#' @rdname vectorcardiogram
#' @export
atrial_vectorcardiogram <- function(
  object,
  beats = c("median", "all"),
  channel = NULL,
  baseline = TRUE
) {
  traced <- trace_loops(
    object,
    waves = "P",
    beats = match.arg(beats),
    channel = channel,
    baseline = baseline,
    what = "The atrial vectorcardiogram"
  )

  loops <- lapply(traced$beats, function(b) b$segment$P)
  components <- data.table::rbindlist(lapply(seq_along(loops), function(i) {
    data.table::data.table(
      beat = i,
      loop_components(loops[[i]], traced$frequency)
    )
  }))

  list(loop = stack_loops(loops), components = components)
}

# Tracing ----------------------------------------------------------------------

#' Cut the orthogonal signal into beats
#'
#' @description Shared engine behind [vectorcardiogram()] and
#'   [atrial_vectorcardiogram()]. Gates the record on the surface lead contract,
#'   transforms it once, and cuts it at the annotated boundaries of the requested
#'   waves.
#'
#' @details The first wave in `waves` anchors the beat; any others are attached
#'   to the beat whose anchor they follow. Each returned beat holds `xyz`, the
#'   orthogonal signal spanning the whole beat, and `mark`, the onset/offset
#'   positions of each wave within it, plus `segment`, those slices taken.
#'
#' @inheritParams vectorcardiogram
#' @param waves A `character` vector of waves to cut, anchor first.
#' @param what A `character` naming the caller, used in error messages.
#'
#' @return A `list` of `frequency` and `beats`.
#'
#' @keywords internal
trace_loops <- function(object, waves, beats, channel, baseline, what) {
  object <- require_ECG(object, leads = colnames(kors), what = what)
  frequency <- attributes(object$header)$record_line$frequency

  ann <- get_single_annotation(object)
  if (nrow(ann) == 0) {
    stop(
      what,
      " requires wave delineation annotations to find the ",
      waves[1],
      " boundaries; none are attached to this record",
      call. = FALSE
    )
  }
  ann <- label_waves(ann)

  # Per-lead annotators leave one set of boundaries per channel, and interleaved
  # those do not describe beats
  if ("channel" %in% names(ann)) {
    spread <- unique(ann$channel[ann$channel != 0L])
    if (is.null(channel) && length(spread) > 1) {
      stop(
        what,
        " needs a guiding `channel`: annotations span channels ",
        paste(sort(spread), collapse = ", "),
        call. = FALSE
      )
    }
    if (!is.null(channel)) {
      ann <- ann[ann$channel %in% c(as.integer(channel), 0L), ]
    }
  }

  # Whole record in orthogonal leads. Kors is linear, so cutting after is the
  # same signal as cutting first and transforming each beat.
  leads <- colnames(kors)
  signal <- as.data.frame(object$signal)[, leads, drop = FALSE]
  xyz <- as.matrix(signal) %*% t(kors)

  # Bracket pairs, one row per delineated wave: each onset takes the next offset
  # and the peak enclosed between them
  delineated <- lapply(waves, function(w) {
    onset <- sort(ann$sample[ann$type == "(" & ann$wave %in% w])
    offset <- sort(ann$sample[ann$type == ")" & ann$wave %in% w])
    peaks <- sort(ann$sample[ann$type %in% c("p", "N", "t") & ann$wave %in% w])

    matched <- offset[findInterval(onset, offset) + 1L]
    onset <- onset[!is.na(matched)]
    matched <- matched[!is.na(matched)]
    enclosed <- vapply(seq_along(onset), function(i) {
      inside <- peaks[peaks > onset[i] & peaks < matched[i]]
      if (length(inside) > 0) inside[1] else NA_integer_
    }, integer(1))

    # A bracket with nothing recognisable inside it is not a wave
    complete <- !is.na(enclosed)
    data.table::data.table(
      onset = onset[complete],
      offset = matched[complete],
      peak = enclosed[complete]
    )
  })
  names(delineated) <- waves

  anchor <- delineated[[waves[1]]]
  if (nrow(anchor) == 0) {
    stop("No complete ", waves[1], " waves could be delineated", call. = FALSE)
  }

  # Sample offsets of every boundary from the anchor peak, one row per beat.
  # Waves other than the anchor belong to the beat whose anchor they follow.
  bounds <- lapply(waves, function(w) {
    it <- delineated[[w]]
    if (w != waves[1]) {
      it <- it[match(seq_len(nrow(anchor)), findInterval(it$onset, anchor$onset))]
    }
    cbind(it$onset, it$offset) - anchor$peak
  })
  names(bounds) <- waves

  if (beats == "median") {
    # One representative beat: the median shape, on a grid anchored at the peak
    bounds <- lapply(bounds, function(b) {
      matrix(round(apply(b, 2, stats::median, na.rm = TRUE)), nrow = 1)
    })
  }

  # A beat runs from the earliest onset to the latest offset across its waves
  onsets <- do.call(pmin, c(lapply(bounds, function(b) b[, 1]), na.rm = TRUE))
  offsets <- do.call(pmax, c(lapply(bounds, function(b) b[, 2]), na.rm = TRUE))

  # Baselining is per beat and before any median, so that beat-to-beat wander is
  # removed rather than averaged in. Samples off the ends of the record are NA.
  cut_beat <- function(peak, from, to) {
    rows <- peak + seq(from, to) + 1L
    rows[rows < 1 | rows > nrow(xyz)] <- NA_integer_
    beat <- xyz[rows, , drop = FALSE]

    if (baseline) {
      lead_in <- seq_len(min(nrow(beat), max(1, round(frequency * 0.01))))
      onset <- apply(beat[lead_in, , drop = FALSE], 2, stats::median)
      beat <- sweep(beat, 2, onset)
    }

    beat
  }

  if (beats == "median") {
    # Every beat on the median grid, then the row-wise median across them
    stack <- vapply(
      anchor$peak,
      cut_beat,
      matrix(0, offsets - onsets + 1L, 3L),
      from = onsets,
      to = offsets
    )
    loops <- list(apply(stack, c(1, 2), stats::median, na.rm = TRUE))
    # vapply took its dimensions from the template, which carries no lead names
    colnames(loops[[1]]) <- colnames(xyz)
  } else {
    loops <- lapply(seq_along(anchor$peak), function(i) {
      cut_beat(anchor$peak[i], onsets[i], offsets[i])
    })
  }

  # Boundaries become 1-based positions within the beat, so a caller can slice
  # each wave straight out of it
  traced <- lapply(seq_along(loops), function(i) {
    mark <- lapply(bounds, function(b) as.integer(b[i, ] - onsets[i] + 1L))
    segment <- lapply(mark, function(m) {
      if (anyNA(m)) NULL else loops[[i]][m[1]:m[2], , drop = FALSE]
    })
    list(xyz = loops[[i]], mark = mark, segment = segment)
  })

  list(frequency = frequency, beats = traced)
}

# Components -------------------------------------------------------------------

#' Spatial magnitude of each vector in a loop
#'
#' @param xyz A numeric matrix with columns `X`, `Y`, and `Z`.
#'
#' @return A numeric vector of magnitudes.
#'
#' @keywords internal
magnitudes <- function(xyz) {
  sqrt(rowSums(xyz^2))
}

#' Orientation of a spatial vector
#'
#' @description Azimuth is measured in the transverse plane from `+X` (left)
#'   toward `+Z` (posterior); elevation is the angle out of that plane toward
#'   `+Y` (inferior). Both in degrees.
#'
#' @param v A numeric vector of length three, ordered X, Y, Z.
#'
#' @return A named numeric vector of `azimuth` and `elevation`.
#'
#' @keywords internal
orientation <- function(v) {
  # Unnamed so that the X/Y/Z labels a loop carries do not survive into the
  # result and rename its elements
  v <- unname(v)

  c(
    azimuth = atan2(v[3], v[1]) * 180 / pi,
    elevation = atan2(v[2], sqrt(v[1]^2 + v[3]^2)) * 180 / pi
  )
}

#' Angle between two spatial vectors
#'
#' @param a,b Numeric vectors of length three, ordered X, Y, Z.
#'
#' @return The angle in degrees, between 0 and 180.
#'
#' @keywords internal
spatial_angle <- function(a, b) {
  scale <- sqrt(sum(a^2)) * sqrt(sum(b^2))
  if (scale == 0) {
    return(NA_real_)
  }
  # Clamped because rounding can push the cosine a hair outside [-1, 1]
  acos(max(-1, min(1, sum(a * b) / scale))) * 180 / pi
}

#' Geometric components of a vectorcardiographic loop
#'
#' @description How large the loop is, where it points, how much area it
#'   encloses, and how flat it is.
#'
#' @param xyz A numeric matrix with columns `X`, `Y`, and `Z`.
#' @param frequency Sampling frequency in Hz.
#'
#' @return A one-row `data.table`.
#'
#' @keywords internal
loop_components <- function(xyz, frequency) {
  magnitude <- magnitudes(xyz)
  peak <- orientation(xyz[which.max(magnitude), ])
  mean_vector <- colMeans(xyz)

  # Vector area of the closed polygon, A = 1/2 sum(r_i x r_i+1). Its magnitude is
  # the area a planar loop encloses. Summing the cross products before taking the
  # magnitude is what makes a figure-of-eight cancel, as it should.
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
    # bulges out of that plane falls away from 1.
    spread <- eigen(stats::cov(xyz), symmetric = TRUE, only.values = TRUE)$values
    if (sum(spread) > 0) {
      planarity <- sum(spread[1:2]) / sum(spread)
    }
  }

  data.table::data.table(
    duration = nrow(xyz) / frequency,
    magnitude_peak = max(magnitude),
    azimuth_peak = peak[["azimuth"]],
    elevation_peak = peak[["elevation"]],
    magnitude_mean = sqrt(sum(mean_vector^2)),
    azimuth_mean = orientation(mean_vector)[["azimuth"]],
    elevation_mean = orientation(mean_vector)[["elevation"]],
    area = area,
    planarity = planarity
  )
}

#' Collect loops into a long table
#'
#' @param loops A list of numeric matrices with columns `X`, `Y`, and `Z`.
#'
#' @return A `data.table` of `beat`, `sample`, `X`, `Y`, and `Z`.
#'
#' @keywords internal
stack_loops <- function(loops) {
  data.table::rbindlist(lapply(seq_along(loops), function(i) {
    data.table::data.table(
      beat = i,
      sample = seq_len(nrow(loops[[i]])) - 1L,
      data.table::as.data.table(loops[[i]])
    )
  }))
}
