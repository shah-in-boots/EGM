test_that("window produces stable rhythm windows", {
  object <- read_wfdb("ecg", test_path(), "ecgpuwave")

  windows <- window(
    object,
    window_method = "rhythm",
    rhythm_type = "sinus",
    onset_criteria = list(type = "(", number = 0),
    offset_criteria = list(type = ")", number = 2),
    reference_criteria = list(type = "N")
  )

  expect_s3_class(windows, "windowed")
  expect_gt(length(windows), 0)
  expect_s3_class(windows[[1]], "EGM")
  expect_gt(nrow(windows[[1]]$signal), 0)

  first_five <- windows[1:min(5, length(windows))]
  expect_s3_class(first_five, "windowed")
  expect_lte(length(first_five), 5)

  amplitudes <- lapply(windows, function(w) {
    max(w$signal$II) - min(w$signal$II)
  })
  expect_length(amplitudes, length(windows))
  expect_true(all(is.finite(unlist(amplitudes))))
})

test_that("can window novel sinus annotator with channel guidance", {

  object <- read_wfdb("ecg-sinus", test_path(), "ann")

  windows <- window(
    object,
    window_method = "rhythm",
    rhythm_type = "sinus",
    channel_criteria = 2,
    onset_criteria = list(type = "(", wave = "P"),
    offset_criteria = list(type = ")", wave = "T"),
    reference_criteria = list(type = "N")
  )

  expect_s3_class(windows, "windowed")
  expect_gt(length(windows), 0)
  expect_s3_class(windows[[1]], "EGM")

  # Each window keeps all 12 signal channels (guided by one lead)
  signal_leads <- setdiff(names(windows[[1]]$signal), "sample")
  expect_length(signal_leads, 12)

  # Each window should span a plausible single sinus beat (~0.5-1.2 s at 500 Hz)
  beat_lengths <- vapply(windows, function(w) nrow(w$signal), integer(1))
  expect_true(all(beat_lengths > 200 & beat_lengths < 700))
})

test_that("sinus defaults produce the same P->T beats", {

  object <- read_wfdb("ecg-sinus", test_path(), "ann")

  explicit <- window(
    object,
    window_method = "rhythm",
    rhythm_type = "sinus",
    channel_criteria = 2,
    onset_criteria = list(type = "(", wave = "P"),
    offset_criteria = list(type = ")", wave = "T"),
    reference_criteria = list(type = "N")
  )

  # Bare call relying on sinus defaults (P-onset -> T-offset, QRS reference)
  defaulted <- window(
    object,
    window_method = "rhythm",
    rhythm_type = "sinus",
    channel_criteria = 2
  )

  expect_equal(length(defaulted), length(explicit))
  expect_equal(
    vapply(defaulted, function(w) nrow(w$signal), integer(1)),
    vapply(explicit, function(w) nrow(w$signal), integer(1))
  )
})

test_that("standardize_windows centers align_feature on the guiding lead", {

  object <- read_wfdb("ecg-sinus", test_path(), "ann")
  windows <- window(
    object,
    window_method = "rhythm",
    rhythm_type = "sinus",
    channel_criteria = 2
  )

  target <- 300
  center_point <- floor((target - 1L) / 2L)

  guided <- standardize_windows(
    windows,
    align_feature = "N",
    channel_criteria = 2,
    target_samples = target
  )

  # The guiding lead's QRS peak must land exactly at the window center in every
  # standardized beat, regardless of how the per-lead fiducials are ordered.
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
  unguided <- standardize_windows(
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
})

test_that("label_waves recovers wave identity positionally", {

  object <- read_wfdb("ecg-sinus", test_path(), "ann")
  ann <- object$annotation[[1]]
  labelled <- label_waves(ann)

  expect_true("wave" %in% names(labelled))

  # Peaks map directly by symbol
  expect_true(all(labelled$wave[labelled$type == "p"] == "P"))
  expect_true(all(labelled$wave[labelled$type == "N"] == "QRS"))
  expect_true(all(labelled$wave[labelled$type == "t"] == "T"))

  # Brackets get a wave even though `number` is uniformly 0
  brackets <- labelled[labelled$type %in% c("(", ")"), ]
  expect_true(all(brackets$number == 0))
  expect_true(any(brackets$wave == "P", na.rm = TRUE))
  expect_true(any(brackets$wave == "T", na.rm = TRUE))
})

# Helper shared by the transform tests below: a set of sinus P->T beats guided
# by lead 2, deliberately ragged in length.
make_sinus_windows <- function() {
  object <- read_wfdb("ecg-sinus", test_path(), "ann")
  suppressMessages(window(
    object,
    window_method = "rhythm",
    rhythm_type = "sinus",
    channel_criteria = 2
  ))
}

test_that("resample_window changes rate while preserving duration", {

  windows <- make_sinus_windows()
  orig_len <- nrow(windows[[1]]$signal)
  orig_freq <- attributes(windows[[1]]$header)$record_line$frequency

  # Downsample 500 -> 250 Hz roughly halves the sample count but keeps duration
  half <- resample_window(windows, resample_frequency = 250)
  expect_s3_class(half, "windowed")
  expect_equal(
    attributes(half[[1]]$header)$record_line$frequency,
    250
  )
  expect_equal(nrow(half[[1]]$signal), as.integer(round((orig_len - 1) / 2) + 1))

  orig_seconds <- orig_len / orig_freq
  half_seconds <- nrow(half[[1]]$signal) / 250
  expect_lt(abs(orig_seconds - half_seconds), 0.01)

  # NULL means "keep native rate": windows come back untouched
  same <- resample_window(windows, resample_frequency = NULL)
  expect_equal(nrow(same[[1]]$signal), orig_len)
})

test_that("window(resample_frequency) resamples inline", {

  object <- read_wfdb("ecg-sinus", test_path(), "ann")
  native <- suppressMessages(window(
    object, rhythm_type = "sinus", channel_criteria = 2
  ))
  upsampled <- suppressMessages(window(
    object, rhythm_type = "sinus", channel_criteria = 2,
    resample_frequency = 1000
  ))

  expect_equal(
    attributes(upsampled[[1]]$header)$record_line$frequency,
    1000
  )
  # 500 -> 1000 Hz roughly doubles the sample count
  expect_gt(nrow(upsampled[[1]]$signal), 1.9 * nrow(native[[1]]$signal))
})

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

  expect_s3_class(normalized, "windowed")
  norm_lengths <- vapply(normalized, function(w) nrow(w$signal), integer(1))
  expect_true(all(norm_lengths == 400))

  # Now uniform, they can be averaged directly into a median beat
  beat <- median_window(normalized)
  expect_equal(nrow(beat$signal), 400)
})

test_that("windowed print shows the window method (regression)", {

  windows <- make_sinus_windows()
  out <- capture.output(print(windows))

  # The constructor stores the method as `window_method`; the print/format
  # methods must read that attribute (previously read a missing `method`).
  expect_true(any(grepl("<windowed: .* EGM windows>", out)))
  expect_true(any(grepl("Method:\\s*rhythm", out)))
})
