test_that("paths are available", {
  on.exit(options(wfdb_path = NULL), add = TRUE)
  options(wfdb_path = tempdir())

  path <- find_wfdb_software()

  expect_identical(path, getOption("wfdb_path"))
})
