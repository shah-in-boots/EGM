make_sinus_windows <- function() {
  object <- read_wfdb("ecg-sinus", test_path(), "ann")
  suppressMessages(get_windows(object, by = by_rhythm(channel = 2)))
}

test_that("EGM does not mask base or stats generics", {
  # The reason `window()` was renamed: `stats::window` is a real S3 generic, and
  # exporting our own broke it for every `ts` object in the session. `windows()`
  # is likewise reserved - `grDevices::windows()` is a graphics device on Windows
  # builds of R - so the constructor is `new_windows()`.
  exports <- getNamespaceExports("EGM")
  expect_false("window" %in% exports)
  expect_false("windows" %in% exports)
})

test_that("windowed print reports method and history", {
  windows <- make_sinus_windows()
  out <- capture.output(print(windows))

  expect_true(any(grepl("<windows: .* EGM segments>", out)))
  expect_true(any(grepl("Method:\\s*rhythm", out)))
  expect_true(any(grepl("History:\\s*rhythm", out)))
})

test_that("subsetting and concatenation preserve class and provenance", {
  windows <- make_sinus_windows()

  first_five <- windows[1:min(5, length(windows))]
  expect_s3_class(first_five, "windows")
  expect_lte(length(first_five), 5)
  expect_equal(attr(first_five, "method"), "rhythm")
  expect_equal(
    attr(first_five, "source_record"),
    attr(windows, "source_record")
  )

  doubled <- c(first_five, first_five)
  expect_s3_class(doubled, "windows")
  expect_length(doubled, 2 * length(first_five))
  expect_equal(attr(doubled, "method"), "rhythm")

  expect_error(c(first_five, list()), "must be `windows` objects")
})

test_that("history accumulates while the extraction method is preserved", {
  windows <- make_sinus_windows()

  piped <- normalize_window(pad_window(windows), target_samples = 200)

  # Each transform appends exactly one step, and none of them overwrite the
  # record of how the collection was originally extracted.
  expect_equal(attr(piped, "history"), c("rhythm", "padded", "normalized"))
  expect_equal(attr(piped, "method"), "rhythm")
  expect_equal(
    attr(piped, "source_record"),
    attr(windows, "source_record")
  )

  # The count is derived, never stored, so it cannot fall out of sync
  expect_null(attr(piped, "window_count"))
  expect_length(piped, length(windows))
})

test_that("map_windows rewraps EGM results and passes others through", {
  windows <- make_sinus_windows()

  # Non-EGM results are a plain list, the natural shape for measurements
  lengths <- map_windows(windows, function(w) nrow(w$signal))
  expect_type(lengths, "list")
  expect_false(is_window_set(lengths))
  expect_length(lengths, length(windows))

  # EGM results rebuild the collection and record the step
  identity_windows <- map_windows(windows, function(w) w)
  expect_s3_class(identity_windows, "windows")
  expect_equal(attr(identity_windows, "history"), c("rhythm", "mapped"))
  expect_equal(attr(identity_windows, "method"), "rhythm")
})

test_that("new_windows validates its contents", {
  expect_s3_class(new_windows(list()), "windows")
  expect_length(new_windows(list()), 0)

  expect_error(new_windows("not a list"), "must be a list")
  expect_error(new_windows(list(1, 2)), "must be of class 'EGM'")
})
