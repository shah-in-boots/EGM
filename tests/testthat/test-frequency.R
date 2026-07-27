# Helper: a set of sinus P->T beats guided by lead 2 from the 500 Hz test record
make_sinus_windows <- function() {
  object <- read_wfdb("ecg-sinus", test_path(), "ann")
  suppressMessages(get_windows(
    object,
    by = by_rhythm(channel = 2)
  ))
}

test_that("frequency reports the header sampling rate", {
  object <- read_wfdb("ecg-sinus", test_path(), "ann")
  expect_equal(frequency(object), 500)

  # A harmonised collection collapses to a single rate
  windows <- make_sinus_windows()
  expect_equal(frequency(windows), 500)

  # An empty collection has no rate to report
  expect_length(frequency(new_windows(list())), 0)
})

test_that("change_frequency converts a record while preserving duration", {
  object <- read_wfdb("ecg-sinus", test_path(), "ann")
  orig_len <- nrow(object$signal)

  # Down-sampling 500 -> 250 Hz roughly halves the sample count but keeps duration
  half <- change_frequency(object, from = 500, to = 250)
  expect_s3_class(half, "EGM")
  expect_equal(frequency(half), 250)
  expect_equal(nrow(half$signal), as.integer(round((orig_len - 1) / 2) + 1))
  expect_equal(
    attributes(half$header)$record_line$samples,
    nrow(half$signal)
  )

  orig_seconds <- orig_len / 500
  half_seconds <- nrow(half$signal) / 250
  expect_lt(abs(orig_seconds - half_seconds), 0.01)

  # Up-sampling is the same operation in the other direction
  fast <- change_frequency(object, from = 500, to = 1000)
  expect_equal(frequency(fast), 1000)
  expect_gt(nrow(fast$signal), 1.9 * orig_len)

  # A matching pair of rates is a no-op on the signal
  same <- change_frequency(object, from = 500, to = 500)
  expect_equal(nrow(same$signal), orig_len)
  expect_equal(same$signal$II, object$signal$II)
})

test_that("change_frequency moves annotations onto the new grid", {
  object <- read_wfdb("ecg-sinus", test_path(), "ann")
  ann <- EGM:::get_single_annotation(object)
  skip_if(nrow(ann) == 0)

  half <- change_frequency(object, from = 500, to = 250)
  half_ann <- EGM:::get_single_annotation(half)

  # Every annotator is carried forward, with the same number of annotations
  expect_named(half$annotation, names(object$annotation))
  expect_equal(nrow(half_ann), nrow(ann))

  # Positions scale with the rate and stay inside the new grid
  expect_equal(half_ann$sample, as.integer(round(ann$sample / 2)))
  expect_true(all(half_ann$sample <= nrow(half$signal) - 1L))

  # Absolute time is unchanged by a duration-preserving conversion
  expect_equal(half_ann$time, ann$time)
})

test_that("change_frequency handles windowed collections and bare lists", {
  windows <- make_sinus_windows()
  orig_len <- nrow(windows[[1]]$signal)

  half <- change_frequency(windows, from = 500, to = 250)
  expect_s3_class(half, "windows")
  expect_equal(frequency(half), 250)
  expect_length(half, length(windows))
  expect_equal(nrow(half[[1]]$signal), as.integer(round((orig_len - 1) / 2) + 1))

  # preserve_class = FALSE drops back to a plain list
  plain <- change_frequency(windows, from = 500, to = 250, preserve_class = FALSE)
  expect_false(is_window_set(plain))
  expect_s3_class(plain[[1]], "EGM")

  # A bare list of EGM objects is accepted and returns a bare list
  from_list <- change_frequency(unclass(windows)[1:2], from = 500, to = 250)
  expect_false(is_window_set(from_list))
  expect_equal(frequency(from_list[[1]]), 250)
})

test_that("change_frequency converts a bare numeric lead", {
  object <- read_wfdb("ecg-sinus", test_path(), "ann")
  lead <- object$signal$II

  doubled <- change_frequency(lead, from = 500, to = 1000)
  expect_type(doubled, "double")
  expect_equal(length(doubled), 2L * (length(lead) - 1L) + 1L)

  # Endpoints are anchored, so the conversion is not a shift
  expect_equal(doubled[1], as.numeric(lead[1]))
  expect_equal(doubled[length(doubled)], as.numeric(lead[length(lead)]))

  # Matrices are not leads, and are rejected rather than silently flattened
  expect_error(
    change_frequency(matrix(lead, ncol = 2), from = 500, to = 1000),
    "plain `numeric` vector"
  )
})

test_that("change_frequency checks the declared source rate", {
  object <- read_wfdb("ecg-sinus", test_path(), "ann")

  # The record is 500 Hz, so a mistaken claim about it is caught
  expect_error(
    change_frequency(object, from = 250, to = 1000),
    "recorded at 500 Hz"
  )
  expect_error(
    change_frequency(make_sinus_windows(), from = 1000, to = 500),
    "recorded at 500 Hz"
  )

  # A bare vector has no header, so the declared rate is taken on trust
  expect_length(
    change_frequency(object$signal$II, from = 250, to = 250),
    nrow(object$signal)
  )
})

test_that("change_frequency supports each standard method", {
  object <- read_wfdb("ecg-sinus", test_path(), "ann")
  target <- 250
  expected_seconds <- nrow(object$signal) / 500

  for (m in c("linear", "spline", "step", "polyphase")) {
    out <- change_frequency(object, from = 500, to = target, method = m)
    expect_equal(frequency(out), target, info = m)
    # Every method preserves duration to within a sample or two
    expect_lt(
      abs(nrow(out$signal) / target - expected_seconds),
      0.02,
      label = m
    )
    expect_true(all(is.finite(out$signal$II)), info = m)
  }
})

test_that("change_frequency rejects unusable inputs", {
  object <- read_wfdb("ecg-sinus", test_path(), "ann")

  expect_error(change_frequency(object, from = 500, to = 0), "`to` must be")
  expect_error(change_frequency(object, from = 500, to = -100), "`to` must be")
  expect_error(
    change_frequency(object, from = 500, to = c(250, 500)),
    "`to` must be"
  )
  expect_error(change_frequency(object, from = 500, to = "250"), "`to` must be")
  expect_error(change_frequency(object, from = NA, to = 250), "`from` must be")
  expect_error(
    change_frequency(data.frame(x = 1), from = 500, to = 250),
    "must be an `EGM` object"
  )
})

test_that("anti-aliasing attenuates content above the new Nyquist", {
  # A 100 Hz tone sampled at 500 Hz sits above the 62.5 Hz Nyquist of a 125 Hz
  # target, so it must not survive the rate change as a full-amplitude alias.
  n <- 2000L
  t <- seq_len(n) - 1L
  tone <- sin(2 * pi * 100 * t / 500)

  filtered <- change_frequency(tone, from = 500, to = 125, anti_alias = TRUE)
  raw <- change_frequency(tone, from = 500, to = 125, anti_alias = FALSE)

  # Ignore the filter's edge transient when comparing amplitudes
  core <- function(x) x[seq(50, length(x) - 50)]
  expect_lt(max(abs(core(filtered))), 0.2)
  expect_gt(max(abs(core(raw))), 0.8)
})

test_that("rational_ratio approximates frequency ratios", {
  expect_equal(EGM:::rational_ratio(2), list(p = 2L, q = 1L))
  expect_equal(EGM:::rational_ratio(0.5), list(p = 1L, q = 2L))
  expect_equal(EGM:::rational_ratio(1000 / 250), list(p = 4L, q = 1L))
  expect_equal(EGM:::rational_ratio(360 / 500), list(p = 18L, q = 25L))

  # Irrational ratios are bounded by the denominator limit but stay accurate
  approx_pi <- EGM:::rational_ratio(pi)
  expect_lte(approx_pi$q, 1000L)
  expect_lt(abs(approx_pi$p / approx_pi$q - pi), 1e-5)
})

test_that("a header answers for its own sampling rate", {
  object <- read_wfdb("ecg-sinus", test_path(), "ann")

  # Without a method the default one answers 1 for any object with no `tsp`,
  # which is a plausible-looking rate rather than a refusal
  expect_equal(frequency(object$header), 500)
  expect_equal(frequency(object$header), frequency(object))
})

test_that("a missing sampling rate is refused rather than returned", {
  object <- read_wfdb("ecg-sinus", test_path(), "ann")
  rl <- attributes(object$header)$record_line
  rl$frequency <- integer()
  attr(object$header, "record_line") <- rl

  expect_error(frequency(object), "no usable sampling frequency")
  expect_error(frequency(object$header), "no usable sampling frequency")
  expect_true(is.na(EGM:::frequency_of(object)))

  # `change_frequency()` is the way to repair one, so it still accepts the record
  repaired <- change_frequency(object, to = 250, from = 500)
  expect_equal(frequency(repaired), 250)
})

test_that("change_frequency reads the source rate off the record", {
  object <- read_wfdb("ecg-sinus", test_path(), "ann")

  # The single-argument call reads as "resample to 250 Hz" and means it
  slow <- change_frequency(object, 250)
  expect_equal(frequency(slow), 250)
  expect_equal(slow$signal, change_frequency(object, to = 250, from = 500)$signal)

  # A stated `from` is an assertion about the data, so a wrong one is an error
  expect_error(change_frequency(object, to = 250, from = 300), "recorded at 500")

  # A bare lead carries no header to read it from
  expect_error(
    change_frequency(as.numeric(object$signal$II), to = 1000),
    "`from` is required"
  )
  expect_length(
    change_frequency(as.numeric(object$signal$II), to = 1000, from = 500),
    2 * nrow(object$signal) - 1
  )
})
