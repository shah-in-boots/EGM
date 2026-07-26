sinus_ecg <- function() {
  read_wfdb("muse-sinus", system.file("extdata", package = "EGM"), "ecgpuwave")
}

# Transformation ----

test_that("the kors dataset is the published matrix", {
  # Kors et al. 1990. A transcription error here is invisible downstream: every
  # loop still looks like a loop.
  expect_equal(dim(kors), c(3L, 8L))
  expect_equal(rownames(kors), c("X", "Y", "Z"))
  expect_equal(
    colnames(kors),
    c("I", "II", "V1", "V2", "V3", "V4", "V5", "V6")
  )
  expect_equal(
    kors["X", ],
    c(I = 0.38, II = -0.07, V1 = -0.13, V2 = 0.05,
      V3 = -0.01, V4 = 0.14, V5 = 0.06, V6 = 0.54)
  )
  expect_equal(
    kors["Y", ],
    c(I = -0.07, II = 0.93, V1 = 0.06, V2 = -0.02,
      V3 = -0.05, V4 = 0.06, V5 = -0.17, V6 = 0.13)
  )
  expect_equal(
    kors["Z", ],
    c(I = 0.11, II = -0.23, V1 = -0.43, V2 = -0.06,
      V3 = -0.14, V4 = -0.20, V5 = -0.11, V6 = 0.31)
  )
})

test_that("the loop is the linear combination the matrix specifies", {
  object <- sinus_ecg()
  loop <- vectorcardiogram(object, beats = "all", baseline = FALSE)$loop
  loop <- loop[loop$beat == 1, ]
  whole <- as.matrix(as.data.frame(object$signal)[, colnames(kors)]) %*% t(kors)

  # An individual loop is a contiguous slice of the transformed record, so it
  # appears in it verbatim. This licenses transforming once and cutting after.
  expect_true(any(vapply(
    seq_len(nrow(whole) - nrow(loop)),
    function(i) isTRUE(all.equal(whole[i + seq_len(nrow(loop)) - 1, "Z"], loop$Z)),
    logical(1)
  )))
})

# Geometry ----

test_that("orientation follows the anatomical convention", {
  # +X left, +Y inferior, +Z posterior
  expect_equal(EGM:::orientation(c(1, 0, 0)), c(azimuth = 0, elevation = 0))
  expect_equal(EGM:::orientation(c(0, 0, 1)), c(azimuth = 90, elevation = 0))
  expect_equal(EGM:::orientation(c(0, 1, 0))[["elevation"]], 90)

  # Column names on a loop vector must not leak into the result
  expect_named(
    EGM:::orientation(c(X = 1, Y = 2, Z = 3)),
    c("azimuth", "elevation")
  )
})

test_that("spatial_angle measures between vectors", {
  expect_equal(EGM:::spatial_angle(c(1, 0, 0), c(0, 1, 0)), 90)
  expect_equal(EGM:::spatial_angle(c(1, 0, 0), c(-1, 0, 0)), 180)
  expect_equal(EGM:::spatial_angle(c(1, 2, 3), c(2, 4, 6)), 0)
  expect_true(is.na(EGM:::spatial_angle(c(0, 0, 0), c(1, 0, 0))))
})

test_that("loop components recover a known circle", {
  # A unit circle in the XY plane: area pi, perfectly planar, peak magnitude 1,
  # and a mean vector of zero since it closes on itself
  theta <- seq(0, 2 * pi, length.out = 721)[-721]
  xyz <- cbind(X = cos(theta), Y = sin(theta), Z = 0)

  components <- EGM:::loop_components(xyz, frequency = 720)

  expect_equal(components$duration, 1)
  expect_equal(components$magnitude_peak, 1)
  expect_equal(components$magnitude_mean, 0, tolerance = 1e-6)
  expect_equal(components$area, pi, tolerance = 1e-4)
  expect_equal(components$planarity, 1)

  # Tilting the circle out of its plane leaves both area and planarity alone
  tilted <- cbind(
    X = xyz[, "X"],
    Y = xyz[, "Y"] / sqrt(2),
    Z = xyz[, "Y"] / sqrt(2)
  )
  expect_equal(EGM:::loop_components(tilted, 720)$area, pi, tolerance = 1e-4)
  expect_equal(EGM:::loop_components(tilted, 720)$planarity, 1)
})

test_that("planarity falls as the loop leaves its plane", {
  theta <- seq(0, 2 * pi, length.out = 361)[-361]
  flat <- cbind(X = cos(theta), Y = sin(theta), Z = 0)
  bulged <- cbind(X = cos(theta), Y = sin(theta), Z = 0.5 * sin(3 * theta))

  expect_equal(EGM:::loop_components(flat, 360)$planarity, 1)
  expect_lt(EGM:::loop_components(bulged, 360)$planarity, 0.99)
})

test_that("a degenerate loop returns missing geometry rather than an error", {
  xyz <- cbind(X = c(0, 1), Y = c(0, 1), Z = c(0, 1))
  components <- EGM:::loop_components(xyz, frequency = 500)

  expect_true(is.na(components$area))
  expect_true(is.na(components$planarity))
  expect_equal(components$magnitude_peak, sqrt(3))
})

# Surface lead contract ----

test_that("vectorcardiograms require all eight Kors leads", {
  study <- read_wfdb("egm", test_path())

  expect_error(
    suppressWarnings(suppressMessages(vectorcardiogram(study))),
    "requires the surface leads"
  )
  expect_error(
    suppressWarnings(suppressMessages(atrial_vectorcardiogram(study))),
    "requires the surface leads"
  )
  expect_error(vectorcardiogram("not an EGM"), "class <EGM> or <ECG>")
})

test_that("vectorcardiograms require wave delineation", {
  bare <- read_wfdb("muse-sinus", system.file("extdata", package = "EGM"))

  expect_error(vectorcardiogram(bare), "requires wave delineation annotations")
})

test_that("a guiding channel is demanded when annotations span leads", {
  object <- read_wfdb("ecg-sinus", test_path(), "ann")

  expect_error(vectorcardiogram(object), "needs a guiding `channel`")
  expect_type(vectorcardiogram(object, channel = 2), "list")
})

# Loops from a record ----

test_that("the ventricular loop is returned with its components", {
  object <- sinus_ecg()
  result <- vectorcardiogram(object)

  # A plain list of two tables, not a bespoke object
  expect_type(result, "list")
  expect_named(result, c("loop", "components"))
  expect_s3_class(result$loop, "data.table")
  expect_equal(names(result$loop), c("beat", "sample", "X", "Y", "Z"))
  expect_equal(nrow(result$components), 1L)

  # The loop table holds exactly the samples the components describe
  expect_equal(nrow(result$loop), result$components$duration * 500)

  # A plausible QRS: 60-140 ms, and nearly planar
  expect_true(result$components$duration > 0.06)
  expect_true(result$components$duration < 0.14)
  expect_gt(result$components$planarity, 0.9)
})

test_that("every beat can be traced separately", {
  object <- sinus_ecg()
  every_beat <- vectorcardiogram(object, beats = "all")

  expect_gt(nrow(every_beat$components), 1L)
  expect_equal(
    sort(unique(every_beat$loop$beat)),
    seq_len(nrow(every_beat$components))
  )

  # Each beat contributes its own samples to the loop table
  expect_equal(
    as.integer(table(every_beat$loop$beat)),
    as.integer(round(every_beat$components$duration * 500))
  )
})

test_that("the median beat sits among the beats it summarises", {
  object <- sinus_ecg()
  median_beat <- vectorcardiogram(object)$components
  every_beat <- vectorcardiogram(object, beats = "all")$components

  # A median taken sample by sample is not the median of any one component, but
  # it should land close to it; a misaligned stack would not
  expect_equal(
    median_beat$magnitude_peak,
    stats::median(every_beat$magnitude_peak),
    tolerance = 0.05
  )
  expect_equal(
    median_beat$duration,
    stats::median(every_beat$duration),
    tolerance = 0.05
  )
  expect_lt(
    abs(median_beat$azimuth_peak - stats::median(every_beat$azimuth_peak)),
    10
  )
})

test_that("a windowed beat can be handed straight in", {
  object <- as_ECG(sinus_ecg())
  windows <- get_windows(object, by = by_rhythm())

  # The window is still an ECG, so it satisfies the contract on its own
  expect_s3_class(windows[[3]], "ECG")

  from_window <- vectorcardiogram(windows[[3]])
  from_record <- vectorcardiogram(object, beats = "all")

  # One beat in, one loop out, matching that beat of the whole record
  expect_equal(nrow(from_window$components), 1L)
  expect_equal(
    from_window$components$magnitude_peak,
    from_record$components$magnitude_peak[3]
  )
  expect_equal(
    from_window$components$qrst_angle_peak,
    from_record$components$qrst_angle_peak[3]
  )
})

test_that("a median beat can be piped in", {
  object <- as_ECG(sinus_ecg())

  piped <- object |>
    get_windows() |>
    median_window(align_feature = "N") |>
    vectorcardiogram()

  expect_equal(nrow(piped$components), 1L)

  # Built by hand or taken from `beats = "median"`, it is the same beat: the
  # windowing differs (P-onset to T-offset rather than QRS-onset to T-offset)
  # but the median across beats does not
  internal <- vectorcardiogram(object)
  expect_equal(
    piped$components$magnitude_peak,
    internal$components$magnitude_peak,
    tolerance = 0.01
  )
  expect_equal(
    piped$components$qrst_angle_peak,
    internal$components$qrst_angle_peak,
    tolerance = 0.01
  )

  # Reducing an object that is already one beat is a no-op, not a second median
  expect_equal(
    vectorcardiogram(median_window(get_windows(object), align_feature = "N")),
    piped
  )
})

test_that("windows carry the ECG class through the transforms", {
  object <- as_ECG(sinus_ecg())
  windows <- get_windows(object, by = by_rhythm())

  expect_true(all(vapply(windows, is_ECG, logical(1))))
  expect_s3_class(pad_window(windows, align = "feature")[[1]], "ECG")
  expect_s3_class(normalize_window(windows, target_samples = 400)[[1]], "ECG")
  expect_s3_class(median_window(windows, align_feature = "N"), "ECG")

  # An EGM that is not a surface ECG stays an EGM
  study <- read_wfdb("ecg-sinus", test_path(), "ann")
  plain <- get_windows(study, by = by_rhythm(channel = 2))
  expect_false(is_ECG(plain[[1]]))
  expect_false(is_ECG(median_window(plain, align_feature = "N", channel_criteria = 2)))
})

# Global electric heterogeneity ----

test_that("GEH components come back with the ventricular loop", {
  components <- vectorcardiogram(sinus_ecg())$components

  expect_true(all(c(
    "qrst_angle_peak", "qrst_angle_mean", "svg_magnitude",
    "svg_azimuth", "svg_elevation", "sai_qrst"
  ) %in% names(components)))

  # Angles are true angles, and the gradient is a non-negative magnitude
  expect_true(components$qrst_angle_peak >= 0 && components$qrst_angle_peak <= 180)
  expect_true(components$qrst_angle_mean >= 0 && components$qrst_angle_mean <= 180)
  expect_gt(components$svg_magnitude, 0)
  expect_gt(components$sai_qrst, 0)

  # SAI QRST bounds the ventricular gradient: it sums absolute values where the
  # gradient allows cancellation between them
  expect_gt(components$sai_qrst, components$svg_magnitude)
})

test_that("GEH is missing rather than invented when the T wave is not delineated", {
  object <- sinus_ecg()
  ann <- object$annotation[[1]]
  object$annotation <- list(qrs_only = ann[ann$type != "t", ])

  result <- vectorcardiogram(object)

  expect_true(is.na(result$components$qrst_angle_peak))
  expect_true(is.na(result$components$svg_magnitude))
  expect_true(is.na(result$components$sai_qrst))

  # The QRS loop itself is unaffected
  expect_equal(
    result$components$magnitude_peak,
    vectorcardiogram(sinus_ecg())$components$magnitude_peak
  )
})

# Atrial loop ----

test_that("the atrial loop traces the P wave", {
  object <- sinus_ecg()
  p_loop <- atrial_vectorcardiogram(object)
  qrs_loop <- vectorcardiogram(object)

  expect_named(p_loop, c("loop", "components"))

  # A P wave is 50-160 ms long and an order of magnitude smaller than the QRS,
  # which is the check that the right wave was cut out
  expect_true(p_loop$components$duration > 0.05)
  expect_true(p_loop$components$duration < 0.16)
  expect_lt(
    p_loop$components$magnitude_peak,
    qrs_loop$components$magnitude_peak / 3
  )

  # GEH belongs to the ventricular loop alone
  expect_false("qrst_angle_peak" %in% names(p_loop$components))

  every_beat <- atrial_vectorcardiogram(object, beats = "all")
  expect_gt(nrow(every_beat$components), 1L)
})

test_that("baselining references each beat to its own onset", {
  # Per beat, since the median is taken after baselining and so is not itself a
  # translation of the unreferenced median
  object <- sinus_ecg()
  referenced <- vectorcardiogram(object, beats = "all")$loop
  raw <- vectorcardiogram(object, beats = "all", baseline = FALSE)$loop

  # The first 10 ms of the beat sit on the origin once referenced
  onset <- referenced[referenced$beat == 1, ][1:5, ]
  expect_equal(stats::median(onset$X), 0)
  expect_equal(stats::median(onset$Y), 0)
  expect_equal(stats::median(onset$Z), 0)
  expect_gt(max(abs(unlist(raw[1, c("X", "Y", "Z")]))), 1)

  # Referencing is a translation, so the shape of each loop is untouched
  expect_equal(
    diff(referenced$X[referenced$beat == 1]),
    diff(raw$X[raw$beat == 1])
  )
})
