test_that("get_windows produces stable rhythm windows", {
  object <- read_wfdb("ecg", test_path(), "ecgpuwave")

  windows <- get_windows(
    object,
    by = by_rhythm(
      onset = list(type = "(", number = 0),
      offset = list(type = ")", number = 2),
      reference = list(type = "N")
    )
  )

  expect_s3_class(windows, "windows")
  expect_gt(length(windows), 0)
  expect_s3_class(windows[[1]], "EGM")
  expect_gt(nrow(windows[[1]]$signal), 0)

  amplitudes <- lapply(windows, function(w) {
    max(w$signal$II) - min(w$signal$II)
  })
  expect_length(amplitudes, length(windows))
  expect_true(all(is.finite(unlist(amplitudes))))
})

test_that("can window novel sinus annotator with channel guidance", {

  object <- read_wfdb("ecg-sinus", test_path(), "ann")

  windows <- get_windows(
    object,
    by = by_rhythm(
      channel = 2,
      onset = list(type = "(", wave = "P"),
      offset = list(type = ")", wave = "T"),
      reference = list(type = "N")
    )
  )

  expect_s3_class(windows, "windows")
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

  explicit <- get_windows(
    object,
    by = by_rhythm(
      channel = 2,
      onset = list(type = "(", wave = "P"),
      offset = list(type = ")", wave = "T"),
      reference = list(type = "N")
    )
  )

  # Bare call relying on sinus defaults (P-onset -> T-offset, QRS reference)
  defaulted <- get_windows(object, by = by_rhythm(channel = 2))

  expect_equal(length(defaulted), length(explicit))
  expect_equal(
    vapply(defaulted, function(w) nrow(w$signal), integer(1)),
    vapply(explicit, function(w) nrow(w$signal), integer(1))
  )
})

test_that("a strategy name and a strategy object are equivalent", {

  object <- read_wfdb("ecg", test_path(), "ecgpuwave")

  named <- suppressWarnings(get_windows(object, by = "rhythm"))
  built <- suppressWarnings(get_windows(object, by = by_rhythm()))

  # The shorthand routes through the same constructor, so neither form can
  # drift from the other
  expect_equal(length(named), length(built))
  expect_equal(
    vapply(named, function(w) nrow(w$signal), integer(1)),
    vapply(built, function(w) nrow(w$signal), integer(1))
  )
  expect_equal(attr(named, "method"), attr(built, "method"))
})

test_that("strategy arguments are validated where they are written", {

  object <- read_wfdb("ecg-sinus", test_path(), "ann")

  # A mistyped argument is now reported rather than swallowed by `...`
  expect_error(by_rhythm(onst = list(type = "(")), "unused argument")

  expect_error(by_rhythm(onset = list("("), rhythm = "afib"), "`onset` must be")
  expect_error(
    by_rhythm(rhythm = "afib", onset = list(type = "("), offset = list()),
    "`offset` must be"
  )
  expect_error(by_rhythm(channel = -1), "`channel` must be")
  expect_error(by_rhythm(channel = "II"), "`channel` must be")
  expect_error(by_rhythm(rhythm = ""), "`rhythm` must be")
  expect_error(by_rhythm(adjust_sample_indices = NA), "must be TRUE or FALSE")

  expect_error(get_windows(object, by = "nonsense"), "Unsupported windowing strategy")
  expect_error(get_windows(object, by = 42), "must be a `window_strategy`")
  expect_error(get_windows("not an EGM"), "<EGM> class")
})

test_that("by_rhythm builds an inspectable strategy", {

  strategy <- by_rhythm(channel = 2)

  expect_true(is_window_strategy(strategy))
  expect_equal(strategy@method, "rhythm")
  expect_equal(strategy@params$channel, 2L)
  expect_equal(strategy@params$onset, list(type = "(", wave = "P"))
  expect_equal(strategy@params$offset, list(type = ")", wave = "T"))
  expect_equal(strategy@params$reference, list(type = "N"))

  out <- capture.output(print(strategy))
  expect_true(any(grepl("<window_strategy: rhythm>", out)))
  expect_true(any(grepl("onset", out)))
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
