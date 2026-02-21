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
