test_that("native reader returns expected signal dimensions", {
  rec <- "muse-sinus"
  dir <- system.file("extdata", package = "EGM")

  sig <- read_signal_native(rec, record_dir = dir)

  expect_s3_class(sig, "signal_table")
  expect_equal(nrow(sig), 5000)
  expect_equal(ncol(sig), 13)
  expect_equal(sig$sample[1], 0)
  expect_equal(sig$sample[5000], 4999)
})

test_that("native reader can return physical units", {
  rec <- "muse-sinus"
  dir <- system.file("extdata", package = "EGM")

  sig <- read_signal_native(rec, record_dir = dir, units = "physical")

  expect_true(all(vapply(as.list(sig)[-1], is.double, logical(1))))
})

test_that("native WFDB reader builds an egm object", {
  rec <- "muse-sinus"
  dir <- system.file("extdata", package = "EGM")

  obj <- read_wfdb_native(rec, record_dir = dir)

  expect_s3_class(obj, "egm")
  expect_s3_class(obj$signal, "signal_table")
  expect_s3_class(obj$header, "header_table")
  expect_s3_class(obj$annotation, "annotation_table")
})
