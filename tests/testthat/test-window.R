test_that("ECGs can be windowed", {
  skip_on_ci()

  rec <- "ecg"
  dir <- test_path()
  object <- read_wfdb(rec, dir, "ecgpuwave")

  # Should create a number of high likelihood sinus beats
  beats <- window(
    object,
    by = "rhythm",
    rhythm_type = "sinus",
    onset_criteria = list(type = "(", number = 0),
    offset_criteria = list(type = ")", number = 2),
    reference_criteria = list(type = "N")
  )

  expect_length(beats, 13)
  expect_equal(nrow(beats[[1]]$signal), 264) # Checked the size of the 1st beat
  expect_s3_class(beats, "windowed")
})

test_that("Basic `windowed` specific functions work", {
  skip_on_ci()

  rec <- "ecg"
  dir <- test_path()
  object <- read_wfdb(rec, dir, "ecgpuwave")

  # Create windows from an egm object
  windows <- window(
    object,
    method = "rhythm",
    rhythm_type = "sinus",
    onset_criteria = list(type = "(", number = 0),
    offset_criteria = list(type = ")", number = 2),
    reference_criteria = list(type = "N")
  )

  expect_s3_class(windows, "windowed")
  expect_s3_class(windows[[1]], "egm")

  # Get information about the windows
  expect_output(
    print(windows),
    regexp = "windowed: 13 EGM segments"
  )

  # Subset to the first 5 windows
  first_five <- windows[1:5]
  expect_length(first_five, 5)

  # Apply a function to calculate amplitude for each window
  amplitudes <- lapply(windows, function(w) {
    max(w$signal$II) - min(w$signal$II)
  })
  expect_length(amplitudes, 13)
})
