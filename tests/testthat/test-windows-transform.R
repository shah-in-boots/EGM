# Helper shared by the transform tests below: a set of sinus P->T beats guided
# by lead 2, deliberately ragged in length.
make_sinus_windows <- function() {
  object <- read_wfdb("ecg-sinus", test_path(), "ann")
  suppressMessages(get_windows(object, by = by_rhythm(channel = 2)))
}

test_that("pad_window anchors the QRS at a common index", {

  windows <- make_sinus_windows()
  padded <- pad_window(
    windows, align = "feature", align_feature = "N", channel_criteria = 2
  )

  # All windows share one length, and their QRS annotations coincide
  pad_lengths <- vapply(padded, function(w) nrow(w$signal), integer(1))
  expect_length(unique(pad_lengths), 1)

  qrs_pos <- vapply(padded, function(w) {
    a <- EGM:::get_single_annotation(w)
    n <- a$sample[a$type == "N" & a$channel == 2L]
    if (length(n) > 0) n[1] else NA_integer_
  }, integer(1))
  expect_length(unique(qrs_pos[!is.na(qrs_pos)]), 1)

  # No signal is lost: the padded length spans the full feature extent
  expect_gte(pad_lengths[1], max(vapply(windows, function(w) nrow(w$signal), integer(1))))
})

test_that("pad_window places signal by alignment with zero edges", {

  windows <- make_sinus_windows()
  n1 <- nrow(windows[[1]]$signal)

  left <- pad_window(windows, target_samples = 800, align = "left")
  right <- pad_window(windows, target_samples = 800, align = "right")

  # Left alignment: real signal at the front, zero padding at the tail
  expect_equal(nrow(left[[1]]$signal), 800)
  expect_equal(left[[1]]$signal$II[800], 0)
  expect_equal(left[[1]]$signal$II[seq_len(n1)], windows[[1]]$signal$II)

  # Right alignment: zero padding at the front, real signal at the tail
  expect_equal(right[[1]]$signal$II[1], 0)
  expect_equal(
    right[[1]]$signal$II[(800 - n1 + 1):800],
    windows[[1]]$signal$II
  )
})

test_that("median_window collapses windows to a single beat", {

  windows <- make_sinus_windows()

  # Ragged windows require an explicit alignment choice
  expect_error(median_window(windows), "differing lengths")

  beat <- median_window(windows, align_feature = "N", channel_criteria = 2)
  expect_s3_class(beat, "EGM")
  expect_true(is_EGM(beat))

  # One beat, all 12 leads retained, named after the source record
  leads <- setdiff(names(beat$signal), "sample")
  expect_length(leads, 12)
  expect_match(
    attributes(beat$header)$record_line$record_name,
    "_median$"
  )

  # Uniform-length input needs no alignment feature
  padded <- pad_window(windows, align = "feature", channel_criteria = 2)
  beat2 <- median_window(padded)
  expect_s3_class(beat2, "EGM")
  expect_equal(nrow(beat2$signal), nrow(padded[[1]]$signal))

  # The median lies within the beat-to-beat range at each sample
  mat <- vapply(padded, function(w) w$signal$II, numeric(nrow(padded[[1]]$signal)))
  expect_true(all(beat2$signal$II >= apply(mat, 1, min) - 1e-8))
  expect_true(all(beat2$signal$II <= apply(mat, 1, max) + 1e-8))
})

test_that("normalize_window stretches every window to a fixed length", {

  windows <- make_sinus_windows()
  normalized <- normalize_window(windows, target_samples = 400)

  expect_s3_class(normalized, "windows")
  norm_lengths <- vapply(normalized, function(w) nrow(w$signal), integer(1))
  expect_true(all(norm_lengths == 400))

  # Now uniform, they can be averaged directly into a median beat
  beat <- median_window(normalized)
  expect_equal(nrow(beat$signal), 400)
})

test_that("normalize_window centers align_feature on the guiding lead", {

  windows <- make_sinus_windows()

  target <- 300
  center_point <- floor((target - 1L) / 2L)

  guided <- normalize_window(
    windows,
    align_feature = "N",
    channel_criteria = 2,
    target_samples = target
  )

  # The guiding lead's QRS peak must land exactly at the window center in every
  # normalized beat, regardless of how the per-lead fiducials are ordered.
  ch2_pos <- vapply(
    guided,
    function(w) {
      a <- EGM:::get_single_annotation(w)
      n <- a$sample[a$type == "N" & a$channel == 2L]
      if (length(n) > 0) n[1] else NA_integer_
    },
    integer(1)
  )
  expect_true(all(ch2_pos == center_point))
  expect_equal(nrow(guided[[1]]$signal), target)
  expect_identical(guided[[1]]$signal$sample, 0:(target - 1L))

  # Without channel guidance the feature aligns on whichever lead sorts first,
  # so the guiding lead's peak is generally NOT centered.
  unguided <- normalize_window(
    windows,
    align_feature = "N",
    target_samples = target
  )
  ch2_pos_unguided <- vapply(
    unguided,
    function(w) {
      a <- EGM:::get_single_annotation(w)
      n <- a$sample[a$type == "N" & a$channel == 2L]
      if (length(n) > 0) n[1] else NA_integer_
    },
    integer(1)
  )
  expect_false(all(ch2_pos_unguided == center_point))

  # preserve_class = FALSE drops back to a plain list of EGM objects
  plain <- normalize_window(windows, target_samples = target, preserve_class = FALSE)
  expect_false(is_window_set(plain))
  expect_s3_class(plain[[1]], "EGM")
})
