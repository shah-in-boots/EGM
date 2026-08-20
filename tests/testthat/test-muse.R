test_that("convert ECG from MUSE XML format to WFDB", {
  # ECG XML file
  file <- system.file("extdata", "muse-sinus.xml", package = "EGM")

  # Get signal data
  ecg <- read_muse(file)
  expect_length(ecg, 3)
  expect_length(ecg$signal, 13)
  expect_equal(nrow(ecg$signal), 5000)
  expect_s3_class(ecg, c("EGM", "ECG"))
  expect_s3_class(ecg$signal, "signal_table")
  expect_s3_class(ecg$signal, "data.table")
  expect_s3_class(ecg$header, "header_table")
})

test_that("the ADC gain matches the units the samples are actually in", {
  file <- system.file("extdata", "muse-sinus.xml", package = "EGM")
  ecg <- read_muse(file)

  # `LeadAmplitudeUnitsPerBit` has been applied, so the smallest step between
  # stored values is that per-bit value and the payload is in microvolts.  A
  # gain of 200 - the WFDB default that stood here before - would call the same
  # samples ADC counts and inflate every amplitude five-fold.
  ii <- ecg$signal$II
  expect_equal(min(diff(sort(unique(ii)))), 4.88)
  expect_equal(unique(as.numeric(ecg$header$ADC_gain)), 1000)

  # Which puts lead II at a physiologic amplitude rather than 4.6 mV
  expect_equal(diff(range(ii)) / 1000, 0.9272)
})

test_that("a MUSE record round trips through WFDB in millivolts", {
  file <- system.file("extdata", "muse-sinus.xml", package = "EGM")
  ecg <- read_muse(file)

  dir <- withr::local_tempdir()
  write_wfdb(ecg, "muse-round-trip", dir)
  back <- read_wfdb("muse-round-trip", dir, units = "physical")

  # Rounding to whole microvolts on write costs less than a microvolt a sample
  expect_equal(diff(range(back$signal$II)), 0.9272, tolerance = 1e-3)
  expect_equal(signal_units(back), "physical")
})

test_that("an unrecognised amplitude unit is refused rather than scaled", {
  file <- system.file("extdata", "muse-sinus.xml", package = "EGM")
  xml <- readLines(file, warn = FALSE)

  # There is no gain that describes a unit the reader does not know, and a
  # guessed one is invisible: the samples would look like any other record.
  odd <- withr::local_tempfile(fileext = ".xml")
  writeLines(gsub("MICROVOLTS", "NANOVOLTS", xml, fixed = TRUE), odd)
  expect_error(read_muse(odd), "NANOVOLTS")

  # The same holds where the leads disagree with each other. Only the last
  # lead is changed, because the rhythm waveform this reads is the second of
  # the two the file carries.
  mixed <- withr::local_tempfile(fileext = ".xml")
  last <- max(grep("MICROVOLTS", xml, fixed = TRUE))
  xml[last] <- sub("MICROVOLTS", "MILLIVOLTS", xml[last], fixed = TRUE)
  writeLines(xml, mixed)
  expect_error(read_muse(mixed), "MICROVOLTS, MILLIVOLTS")
})
