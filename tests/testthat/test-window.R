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
