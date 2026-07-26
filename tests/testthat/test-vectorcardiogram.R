sinus_ecg <- function() {
  read_wfdb("muse-sinus", system.file("extdata", package = "EGM"), "ecgpuwave")
}

# Kors transformation ----

test_that("the Kors matrix is the published one", {
  # Kors et al. 1990, the regression matrix. Transcription errors here are
  # invisible downstream: every loop still looks like a loop.
  expect_equal(dim(EGM:::.kors), c(3L, 8L))
  expect_equal(rownames(EGM:::.kors), c("X", "Y", "Z"))
  expect_equal(
    colnames(EGM:::.kors),
    c("I", "II", "V1", "V2", "V3", "V4", "V5", "V6")
  )
  expect_equal(
    EGM:::.kors["X", ],
    c(I = 0.38, II = -0.07, V1 = -0.13, V2 = 0.05,
      V3 = -0.01, V4 = 0.14, V5 = 0.06, V6 = 0.54)
  )
  expect_equal(
    EGM:::.kors["Y", ],
    c(I = -0.07, II = 0.93, V1 = 0.06, V2 = -0.02,
      V3 = -0.05, V4 = 0.06, V5 = -0.17, V6 = 0.13)
  )
  expect_equal(
    EGM:::.kors["Z", ],
    c(I = 0.11, II = -0.23, V1 = -0.43, V2 = -0.06,
      V3 = -0.14, V4 = -0.20, V5 = -0.11, V6 = 0.31)
  )
})

test_that("kors_transform reproduces the linear combination by hand", {
  leads <- colnames(EGM:::.kors)
  data <- as.data.frame(matrix(rnorm(80), nrow = 10))
  colnames(data) <- leads
  signal <- do.call(signal_table, as.list(data))

  xyz <- EGM:::kors_transform(signal, frequency = 500, baseline = FALSE)

  expect_equal(colnames(xyz), c("X", "Y", "Z"))
  expect_equal(nrow(xyz), 10)
  expect_equal(
    xyz[, "Y"],
    as.numeric(as.matrix(data) %*% EGM:::.kors["Y", leads])
  )

  # Extra leads are ignored rather than allowed to leak into the result
  wide <- do.call(signal_table, c(as.list(data), list(III = rnorm(10))))
  expect_equal(EGM:::kors_transform(wide, 500, baseline = FALSE), xyz)
})

test_that("baselining references the loop to its own onset", {
  leads <- colnames(EGM:::.kors)
  data <- as.data.frame(matrix(1, nrow = 20, ncol = 8))
  colnames(data) <- leads
  signal <- do.call(signal_table, as.list(data))

  # A constant record is entirely baseline, so nothing survives the subtraction
  expect_true(all(EGM:::kors_transform(signal, 500) == 0))
  expect_false(all(EGM:::kors_transform(signal, 500, baseline = FALSE) == 0))
})

# Loop geometry ----

test_that("loop metrics recover a known circle", {
  # A unit circle in the XY plane: area pi, perfectly planar, peak magnitude 1,
  # and lying in the plane the azimuth is measured from.
  theta <- seq(0, 2 * pi, length.out = 721)[-721]
  xyz <- cbind(X = cos(theta), Y = sin(theta), Z = 0)

  metrics <- EGM:::vcg_loop_metrics(xyz, frequency = 720)

  expect_equal(metrics$magnitude_peak, 1)
  expect_equal(metrics$magnitude_mean, 1)
  expect_equal(metrics$area, pi, tolerance = 1e-4)
  expect_equal(metrics$planarity, 1)
  expect_equal(metrics$duration, 1)

  # Tilting the circle out of its plane leaves the area unchanged but drops the
  # elevation of the peak vector, which now has a Z component
  tilted <- cbind(
    X = xyz[, "X"],
    Y = xyz[, "Y"] / sqrt(2),
    Z = xyz[, "Y"] / sqrt(2)
  )
  expect_equal(EGM:::vcg_loop_metrics(tilted, 720)$area, pi, tolerance = 1e-4)
  expect_equal(EGM:::vcg_loop_metrics(tilted, 720)$planarity, 1)
})

test_that("planarity falls as the loop leaves its plane", {
  theta <- seq(0, 2 * pi, length.out = 361)[-361]
  flat <- cbind(X = cos(theta), Y = sin(theta), Z = 0)
  bulged <- cbind(X = cos(theta), Y = sin(theta), Z = 0.5 * sin(3 * theta))

  expect_equal(EGM:::vcg_loop_metrics(flat, 360)$planarity, 1)
  expect_lt(EGM:::vcg_loop_metrics(bulged, 360)$planarity, 0.99)
})

test_that("a degenerate loop returns missing geometry rather than an error", {
  xyz <- cbind(X = c(0, 1), Y = c(0, 1), Z = c(0, 1))
  metrics <- EGM:::vcg_loop_metrics(xyz, frequency = 500)

  expect_true(is.na(metrics$area))
  expect_true(is.na(metrics$planarity))
  expect_equal(metrics$magnitude_peak, sqrt(3))
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
  expect_s3_class(vectorcardiogram(object, channel = 2), "vectorcardiogram")
})

# Loops from a record ----

test_that("the QRS vectorcardiogram traces one loop per beat", {
  object <- sinus_ecg()

  median_beat <- vectorcardiogram(object)
  every_beat <- vectorcardiogram(object, beats = "all")

  expect_s3_class(median_beat, "vectorcardiogram")
  expect_equal(median_beat$wave, "QRS")
  expect_equal(nrow(median_beat$metrics), 1L)
  expect_gt(nrow(every_beat$metrics), 1L)
  expect_equal(
    sort(unique(every_beat$loop$beat)),
    seq_len(nrow(every_beat$metrics))
  )
  expect_equal(names(median_beat$loop), c("beat", "sample", "X", "Y", "Z"))

  # A median beat is a plausible QRS: 60-140 ms, and the loop is nearly planar
  expect_true(median_beat$metrics$duration > 0.06)
  expect_true(median_beat$metrics$duration < 0.14)
  expect_gt(median_beat$metrics$planarity, 0.9)

  # The median loop sits among the individual loops it summarises
  expect_gt(
    median_beat$metrics$magnitude_peak,
    min(every_beat$metrics$magnitude_peak) * 0.5
  )
  expect_lt(
    median_beat$metrics$magnitude_peak,
    max(every_beat$metrics$magnitude_peak) * 1.5
  )
})

test_that("the transformation commutes with windowing", {
  # Kors is linear, so a loop cut from the transformed record and a loop
  # transformed after cutting are the same signal. This is what licenses
  # transforming once, after segmentation, rather than before it.
  object <- sinus_ecg()
  windows <- get_windows(
    object,
    by = by_rhythm(
      rhythm = "sinus",
      onset = list(type = "(", wave = "QRS"),
      offset = list(type = ")", wave = "QRS"),
      reference = list(type = "N"),
      adjust_sample_indices = FALSE
    )
  )

  whole <- EGM:::kors_transform(object$signal, 500, baseline = FALSE)
  beat <- windows[[1]]

  expect_equal(
    EGM:::kors_transform(beat$signal, 500, baseline = FALSE),
    whole[beat$signal$sample + 1L, , drop = FALSE]
  )
})

test_that("the atrial vectorcardiogram traces the P loop", {
  object <- sinus_ecg()

  p_loop <- atrial_vectorcardiogram(object)
  qrs_loop <- vectorcardiogram(object)

  expect_equal(p_loop$wave, "P")

  # A P wave is 80-140 ms long and roughly an order of magnitude smaller than
  # the QRS, which is the check that the right wave was cut out
  expect_true(p_loop$metrics$duration > 0.05)
  expect_true(p_loop$metrics$duration < 0.16)
  expect_lt(p_loop$metrics$magnitude_peak, qrs_loop$metrics$magnitude_peak / 3)

  # Beat-to-beat analysis returns one P loop per delineated beat
  every_beat <- atrial_vectorcardiogram(object, beats = "all")
  expect_gt(nrow(every_beat$metrics), 1L)
  expect_equal(every_beat$beats, "all")
})

test_that("printing reports the wave and the beat count", {
  object <- sinus_ecg()

  expect_output(print(vectorcardiogram(object)), "QRS loop from 1 median beat")
  expect_output(
    print(atrial_vectorcardiogram(object, beats = "all")),
    "P loop from [0-9]+ beats"
  )
})
