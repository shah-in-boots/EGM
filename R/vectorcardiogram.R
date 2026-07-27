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
#' The transformation is linear, so it commutes with the segmentation: a beat
#' cut from the transformed record and a beat transformed after cutting are the
#' same signal, and it makes no difference that each beat is transformed
#' separately here.
#'
#' It does not commute with the median, which is not a linear operator. The
#' median is taken lead by lead, by [median_window()], so `beats = "median"`
#' returns the loop of the median beat rather than the median of the beats'
#' loops.
#'
#' # Segmentation
#'
#' The unit of analysis is one beat. Wave boundaries come from the record's own
#' delineation annotations, since neither loop can be delimited without them; a
#' record without them is an error rather than a guess.
#'
#' A record is cut into beats by [by_beat()], a fixed span around each wave peak.
#' Equal-length windows are what let [median_window()] reduce them without
#' padding, and a padded sample is a fabricated one - it would drag the median
#' toward whatever the padding says at exactly the loop tails the components are
#' read from.
#'
#' An object that already holds one beat - a window, or a median of them - is
#' taken as it stands, since there is no room in it for a span and nothing left
#' to reduce. So
#'
#' ```r
#' vectorcardiogram(ecg)
#' ```
#'
#' and
#'
#' ```r
#' ecg |>
#'   get_windows() |>
#'   median_window(align_feature = "N") |>
#'   vectorcardiogram()
#' ```
#'
#' both describe one beat, the first taking `beats = "median"` from its default
#' and the second building the beat itself. Reach for the second when the
#' windowing or the alignment needs to be something other than the default;
#' [map_windows()] will run either function over every window of a collection.
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
#' @param object An [ECG]: a whole record, a single windowed beat, or a median
#'   beat. An `EGM` is reduced to its surface leads first (see [as_ECG()]), which
#'   is how a 12-lead record read straight off disk, or the surface leads of an
#'   electrophysiology study, become usable. All eight leads of the [kors]
#'   transformation must be present.
#'
#' @param beats Which beats to trace when the object holds more than one.
#'   `"median"` (default) reduces them with [median_window()] and returns a single
#'   loop; `"all"` returns one loop per beat, preserving the beat-to-beat
#'   variability a signal average is designed to remove (Tachmatzidis et al.
#'   2022). An object that already holds one beat is unaffected by either.
#'
#' @param channel The lead whose annotations delineate the waves, given as a
#'   channel number or name. Required when the annotations span more than one
#'   channel, as they do when an annotator has been run per lead; see the
#'   channels section.
#'
#' @param baseline Logical. If `TRUE` (default), each beat is referenced to its
#'   own onset, taken as the median of the 10 ms following it. Orientation is
#'   measured from the origin, so an offset baseline rotates every angle.
#'
#' @return A `list` of two `data.table`s:
#'
#'   \describe{
#'     \item{`loop`}{The orthogonal signal, with columns `beat`, `sample`, `X`,
#'       `Y` and `Z`, in the units of the source signal.}
#'     \item{`components`}{One row per loop, as tabulated below.}
#'   }
#'
#'   Azimuth is measured in the transverse plane from `+X` toward `+Z`, and
#'   elevation out of that plane toward `+Y`, both in degrees.
#'
#'   Not every beat reaches the result, and [window_dropped()] reads back how
#'   many did not and why: `incomplete_span`, beats too near an end of the record
#'   for the fixed window to be cut, and `no_delineation`, beats the annotator
#'   did not mark the wave in. Both are counted rather than announced, since the
#'   first is unavoidable on a short strip and neither is visible from a
#'   background worker.
#'
#' @section Components and their units:
#'
#'   Half of these components are scale-free and half are not, which matters
#'   because the source signal may be in millivolts or in raw ADC counts
#'   depending on how the record was read (see the `units` argument of
#'   [read_wfdb()], and [signal_units()] to ask an object which it holds). A gain
#'   of 200 changes every value in the "signal units" rows by 200, and none of
#'   the others. Between-record comparisons of those rows are only meaningful on
#'   signals read the same way.
#'
#'   | Component | Units | Scale-free |
#'   |---|---|---|
#'   | `duration` | seconds | yes |
#'   | `magnitude_peak`, `magnitude_mean` | signal units | no |
#'   | `azimuth_peak`, `elevation_peak` | degrees | yes |
#'   | `azimuth_mean`, `elevation_mean` | degrees | yes |
#'   | `area` | signal units squared | no |
#'   | `planarity` | proportion (0-1) | yes |
#'   | `qrst_angle_peak`, `qrst_angle_mean` | degrees | yes |
#'   | `svg_magnitude` | signal units x seconds | no |
#'   | `svg_azimuth`, `svg_elevation` | degrees | yes |
#'   | `sai_qrst` | signal units x seconds | no |
#'
#'   The last five are returned by `vectorcardiogram()` only; they are the global
#'   electric heterogeneity components, and `atrial_vectorcardiogram()` declines
#'   to report a QRS-T relationship for a P loop.
#'
#' @inheritSection channels Guiding channel
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
#'   lead contract, [get_windows()] and [median_window()] for building a beat by
#'   hand, [extract_f_waves()] for the atrial signal when there is no P wave to
#'   trace.
#'
#' @examples
#' \dontrun{
#' ecg <- read_wfdb("muse-sinus", system.file("extdata", package = "EGM"),
#'                  annotator = "ecgpuwave")
#'
#' vectorcardiogram(ecg)$components
#' atrial_vectorcardiogram(ecg, beats = "all")$components
#'
#' # The same loop, with the windowing and alignment chosen explicitly
#' ecg |>
#'   get_windows(by = by_rhythm(channel = 2)) |>
#'   median_window(align_feature = "N", channel = 2) |>
#'   vectorcardiogram()
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
  assemble_loops(
    trace_loops(
      object,
      waves = c("QRS", "T"),
      beats = match.arg(beats),
      channel = channel,
      baseline = baseline,
      what = "The vectorcardiogram"
    ),
    wave = "QRS",
    repolarization = "T"
  )
}

#' @rdname vectorcardiogram
#' @export
atrial_vectorcardiogram <- function(
  object,
  beats = c("median", "all"),
  channel = NULL,
  baseline = TRUE
) {
  assemble_loops(
    trace_loops(
      object,
      waves = "P",
      beats = match.arg(beats),
      channel = channel,
      baseline = baseline,
      what = "The atrial vectorcardiogram"
    ),
    wave = "P"
  )
}

# Tracing ----------------------------------------------------------------------

#' Cut the orthogonal signal into beats
#'
#' @description Shared engine behind [vectorcardiogram()] and
#'   [atrial_vectorcardiogram()]. Gates the record on the surface lead contract,
#'   cuts it into beats, and transforms each one.
#'
#' @details A beat runs from the onset of the first wave in `waves` to the offset
#'   of the last, so every wave the components are read from travels with it.
#'   Windowing is [get_windows()] and reduction is [median_window()], so an object
#'   that already holds a single beat passes through both unchanged - which is
#'   what lets a windowed beat, or a median beat, be handed straight in.
#'
#'   Each returned beat holds `xyz`, the orthogonal signal over the whole span,
#'   and `segment`, that span cut into the individual waves.
#'
#' @inheritParams vectorcardiogram
#' @param waves A `character` vector of waves to cut, in order.
#' @param what A `character` naming the caller, used in error messages.
#'
#' @return A `list` of `frequency` and `beats`.
#'
#' @keywords internal
trace_loops <- function(object, waves, beats, channel, baseline, what) {
  object <- require_ECG(object, leads = colnames(kors), what = what)
  frequency <- attributes(object$header)$record_line$frequency
  peaks <- c(P = "p", QRS = "N", T = "t")

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

  # Per-lead annotators leave one set of boundaries per channel, and interleaved
  # those do not describe beats
  channel <- resolve_annotation_channel(
    ann,
    resolve_channel_spec(object, valid_channel(channel)),
    what = what
  )

  # A fixed span around the fiducial, wide enough to hold the waves at any
  # physiological rate. Equal-length windows are what let the median be taken
  # without padding, which would fabricate the very samples the loop tails are
  # read from.
  span <- if (waves[1] == "P") c(200, 200) else c(400, 600)
  before <- ceiling(span[1] / 1000 * frequency)

  # An object holding a single beat - a window, or a median of them - is taken as
  # it stands. There is no room in it for a span, and nothing left to reduce.
  centres <- locate_features(ann, peaks[[waves[1]]], channel)
  if (length(centres) == 0) {
    stop("No complete ", waves[1], " waves could be delineated", call. = FALSE)
  }

  windows <- if (length(centres) == 1) {
    list(object)
  } else {
    get_windows(
      object,
      by = by_beat(
        before = span[1],
        after = span[2],
        feature = peaks[[waves[1]]],
        channel = channel
      )
    )
  }
  if (length(windows) == 0) {
    stop("No complete ", waves[1], " waves could be delineated", call. = FALSE)
  }

  # Read before the reduction, which returns a bare beat and with it no accounting
  dropped <- window_dropped(windows)

  # Every window is the same length, so the median needs no padding; the feature
  # is passed only so that fiducials are matched outward from it.
  if (beats == "median" && length(windows) > 1) {
    windows <- list(median_window(
      windows,
      align_feature = peaks[[waves[1]]],
      channel = channel
    ))
  }

  traced <- lapply(windows, function(w) {
    fiducials <- label_waves(get_single_annotation(w))
    if (!is.null(channel) && !is.null(fiducials$channel)) {
      fiducials <- fiducials[fiducials$channel %in% c(as.integer(channel), 0L), ]
    }

    # Row range of each wave. The window reaches into the neighbouring beats, so
    # the waves are walked outward from the fiducial: the first is the bracket
    # pair enclosing it, and each later one the first pair to open after the wave
    # before it.
    mark <- stats::setNames(vector("list", length(waves)), waves)

    # Where the fiducial sits in this window. `by_beat()` puts it at `before`; a
    # beat handed in directly puts it wherever it happens to be.
    anchors <- fiducials$sample[fiducials$type == peaks[[waves[1]]]]
    if (length(anchors) == 0) {
      return(NULL)
    }
    walk <- anchors[which.min(abs(anchors - before))]
    for (v in waves) {
      opens <- sort(fiducials$sample[fiducials$type == "(" & fiducials$wave %in% v])
      closes <- sort(fiducials$sample[fiducials$type == ")" & fiducials$wave %in% v])

      # Nearest pair on the right side of the walk: the last to open at or before
      # the fiducial for the first wave, the first to open after the wave before
      # it for the rest
      onset <- if (v == waves[1]) {
        rev(opens[opens <= walk])[1]
      } else {
        opens[opens > walk][1]
      }
      offset <- closes[closes > onset][1]
      if (is.na(onset) || is.na(offset)) next

      mark[[v]] <- c(onset, offset) + 1L
      walk <- offset
    }
    # A beat the annotator did not delineate is dropped and counted, not fatal.
    # One undelineated beat among thirteen used to abort the whole record, which
    # is the wrong trade when the point of `beats = "all"` is the spread across
    # the beats that *are* delineated.
    if (is.null(mark[[waves[1]]])) {
      return(NULL)
    }

    xyz <- as.matrix(
      as.data.frame(w$signal)[, colnames(kors), drop = FALSE]
    ) %*% t(kors)

    if (baseline) {
      # The first 10 ms of the first wave, taken there rather than at the window
      # edge because the window reaches beyond the beat on both sides. A median
      # over 10 ms so that one noisy sample cannot displace the whole loop.
      from <- mark[[waves[1]]][1]
      width <- max(1L, round(frequency * 0.01))
      lead_in <- seq(from, min(nrow(xyz), from + width - 1L))
      xyz <- sweep(xyz, 2, apply(xyz[lead_in, , drop = FALSE], 2, stats::median))
    }

    # The span the components are read over: the first wave's onset to the last
    # offset delineated, ignoring the window either side of it
    ends <- vapply(mark, function(m) if (is.null(m)) NA_real_ else m[2], numeric(1))
    span <- mark[[waves[1]]][1]:max(ends, na.rm = TRUE)

    list(
      xyz = xyz[span, , drop = FALSE],
      segment = lapply(mark, function(m) {
        if (is.null(m)) NULL else xyz[m[1]:m[2], , drop = FALSE]
      })
    )
  })

  undelineated <- vapply(traced, is.null, logical(1))
  traced <- traced[!undelineated]
  if (length(traced) == 0) {
    stop("No complete ", waves[1], " waves could be delineated", call. = FALSE)
  }
  dropped <- sum_dropped(list(
    dropped,
    c(no_delineation = sum(undelineated))
  ))

  list(frequency = frequency, beats = traced, dropped = dropped)
}

# Components -------------------------------------------------------------------

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
  magnitude <- sqrt(rowSums(xyz^2))
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

#' Assemble traced beats into the tables the caller gets back
#'
#' @description Takes what [trace_loops()] cut and returns the pair both
#'   vectorcardiogram functions hand back: the named wave's loop, beat by beat,
#'   and the components read off it.
#'
#' @details Naming a `repolarization` wave adds the global electric heterogeneity
#'   components. Those describe the discordance between depolarization and
#'   repolarization, so they need both loops and the span between them; a beat
#'   without the second wave delineated keeps its loop and takes `NA` for them.
#'
#' @param traced The result of [trace_loops()].
#' @param wave The wave whose loop is returned, e.g. `"QRS"`.
#' @param repolarization Optional second wave to measure the first against.
#'
#' @return A `list` of `loop` and `components`, both `data.table`s.
#'
#' @keywords internal
assemble_loops <- function(traced, wave, repolarization = NULL) {
  loops <- lapply(traced$beats, function(b) b$segment[[wave]])

  loop <- data.table::rbindlist(lapply(seq_along(loops), function(i) {
    data.table::data.table(
      beat = i,
      sample = seq_len(nrow(loops[[i]])) - 1L,
      data.table::as.data.table(loops[[i]])
    )
  }))

  components <- data.table::rbindlist(lapply(seq_along(loops), function(i) {
    beat <- traced$beats[[i]]
    against <- if (is.null(repolarization)) NULL else beat$segment[[repolarization]]

    geh <- if (is.null(repolarization)) {
      NULL
    } else if (is.null(against)) {
      data.table::data.table(
        qrst_angle_peak = NA_real_,
        qrst_angle_mean = NA_real_,
        svg_magnitude = NA_real_,
        svg_azimuth = NA_real_,
        svg_elevation = NA_real_,
        sai_qrst = NA_real_
      )
    } else {
      # Peak vectors are the largest of each loop and mean vectors their
      # centroid; the spatial QRS-T angle is taken between each pair. The beat
      # spans one wave's onset to the other's offset, so the ventricular gradient
      # is its integral and the sum absolute QRST integral the non-directional
      # counterpart, both in signal units x seconds.
      svg <- colSums(beat$xyz) / traced$frequency
      angles <- orientation(svg)

      data.table::data.table(
        qrst_angle_peak = spatial_angle(
          loops[[i]][which.max(rowSums(loops[[i]]^2)), ],
          against[which.max(rowSums(against^2)), ]
        ),
        qrst_angle_mean = spatial_angle(colMeans(loops[[i]]), colMeans(against)),
        svg_magnitude = sqrt(sum(svg^2)),
        svg_azimuth = angles[["azimuth"]],
        svg_elevation = angles[["elevation"]],
        sai_qrst = sum(abs(beat$xyz)) / traced$frequency
      )
    }

    data.table::data.table(
      beat = i,
      loop_components(loops[[i]], traced$frequency),
      geh
    )
  }))

  # Carried as an attribute rather than a third element so that `window_dropped()`
  # is the one accessor for this across the package, and the documented shape of
  # the result - two tables - does not change
  structure(
    list(loop = loop, components = components),
    dropped = traced$dropped
  )
}
