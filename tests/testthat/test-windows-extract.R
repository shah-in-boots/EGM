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

# Beat strategy ----

test_that("by_beat cuts a fixed span around every fiducial", {

  object <- read_wfdb("muse-sinus", system.file("extdata", package = "EGM"),
                      "ecgpuwave")
  windows <- get_windows(object, by = by_beat(before = 300, after = 500))

  expect_s3_class(windows, "windows")
  expect_gt(length(windows), 1)

  # Every window is the same length, which is the whole point: 800 ms at 500 Hz
  lengths <- vapply(windows, function(w) nrow(w$signal), integer(1))
  expect_length(unique(lengths), 1)
  expect_equal(unique(lengths), 401L)

  # The fiducial lands at the same index in each, `before` samples in
  qrs <- vapply(windows, function(w) {
    a <- EGM:::get_single_annotation(w)
    as.numeric(a$sample[a$type == "N"][1])
  }, numeric(1))
  expect_equal(unique(qrs), 150)

  # Ragged rhythm windows of the same record are, by contrast, not uniform
  ragged <- get_windows(object, by = by_rhythm())
  expect_gt(length(unique(vapply(ragged, function(w) nrow(w$signal), integer(1)))), 1)
})

test_that("by_beat drops beats without room rather than truncating them", {

  object <- read_wfdb("muse-sinus", system.file("extdata", package = "EGM"),
                      "ecgpuwave")
  beats <- length(EGM:::locate_features(object$annotation[[1]], "N"))

  # A span wider than the record's margins costs beats at each end
  expect_message(
    windows <- get_windows(object, by = by_beat(before = 2000, after = 2000)),
    "too near the ends of the record"
  )
  expect_lt(length(windows), beats)
  expect_length(unique(vapply(windows, function(w) nrow(w$signal), integer(1))), 1)
})

test_that("by_beat validates its arguments where they are written", {

  expect_s3_class(by_beat(), "EGM::window_strategy")
  expect_equal(by_beat()@method, "beat")
  expect_equal(by_beat(feature = "p", channel = 2)@params$channel, 2L)

  expect_error(by_beat(before = -1), "non-negative")
  expect_error(by_beat(after = c(1, 2)), "non-negative")
  expect_error(by_beat(feature = 3), "type symbol")
  expect_error(by_beat(channel = -1), "non-negative whole number")
  expect_error(by_beat(adjust_sample_indices = NA), "TRUE or FALSE")

  out <- capture.output(print(by_beat()))
  expect_true(any(grepl("<window_strategy: beat>", out)))
})

test_that("fixed-span windows need no padding to be reduced", {

  object <- read_wfdb("muse-sinus", system.file("extdata", package = "EGM"),
                      "ecgpuwave")
  windows <- get_windows(object, by = by_beat())

  # No alignment argument is needed, and nothing comes back missing
  beat <- median_window(windows)
  expect_equal(nrow(beat$signal), nrow(windows[[1]]$signal))
  expect_false(anyNA(beat$signal$II))

  # Each sample of the median is backed by every window
  mat <- vapply(windows, function(w) w$signal$II, numeric(nrow(beat$signal)))
  expect_false(anyNA(mat))
  expect_true(all(beat$signal$II >= apply(mat, 1, min) - 1e-8))
  expect_true(all(beat$signal$II <= apply(mat, 1, max) + 1e-8))
})

test_that("median fiducials are matched outward from an anchor", {

  # In atrial fibrillation the neighbouring beats drift in and out of a fixed
  # span, so rank counted from the window start names a different fiducial in
  # each window. Counted from the anchor, it names the same one.
  object <- read_wfdb("muse-af", system.file("extdata", package = "EGM"),
                      "ecgpuwave")
  windows <- get_windows(object, by = by_beat(before = 400, after = 600))

  anchored <- EGM:::label_waves(EGM:::median_annotations(
    windows, frequency = 500, anchor = "N"
  ))
  unanchored <- EGM:::label_waves(EGM:::median_annotations(windows, frequency = 500))

  qrs_peak <- anchored$sample[anchored$type == "N"][1]
  t_onset <- anchored$sample[anchored$type == "(" & anchored$wave %in% "T"]

  # The T wave of the anchored beat opens after its QRS, where it belongs
  expect_length(t_onset, 1)
  expect_gt(t_onset, qrs_peak)

  # Without the anchor it does not
  loose <- unanchored$sample[unanchored$type == "(" & unanchored$wave %in% "T"]
  expect_false(length(loose) == 1 && loose > qrs_peak)
})
