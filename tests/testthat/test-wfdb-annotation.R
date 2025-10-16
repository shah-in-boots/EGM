test_that("can read in annotation files", {
  skip_on_cran()
  skip_on_ci()

  x <- read_annotation(
    record = "300",
    record_dir = test_path(),
    annotator = "ecgpuwave"
  )

  expect_s3_class(x, "data.frame")
  expect_length(x, 6)
  expect_output(print(x), 'ecgpuwave')
  expect_s3_class(x, "data.table")
  expect_named(
    x,
    expected = c("time", "sample", "type", "subtype", "channel", "number")
  )
})

test_that("can read in faulty signal safely", {
  skip_on_cran()
  skip_on_ci()

  # Bad ECG that has no signal
  record <- "bad-ecg"
  record_dir <- test_path()

  read_wfdb(record, record_dir, annotator = "ecgpuwave")
  expect_s3_class(read_header(record, record_dir), "header_table")
  expect_s3_class(read_signal(record, record_dir), "signal_table")
  expect_message({
    ann <- read_annotation(record, record_dir, annotator = "ecgpuwave")
  })
  expect_length(ann, 6)
  expect_equal(nrow(ann), 0)
})

test_that("native annotation write is lossless", {
  record <- "300"
  annotator <- "ecgpuwave"
  record_dir <- test_path()
  tmp_dir <- withr::local_tempdir()

  ann <- read_annotation(
    record = record,
    record_dir = record_dir,
    annotator = annotator,
    backend = "native"
  )

  fs::file_copy(
    fs::path(record_dir, paste0(record, ".hea")),
    fs::path(tmp_dir, paste0(record, ".hea"))
  )

  write_annotation(
    data = ann,
    annotator = annotator,
    record = record,
    record_dir = tmp_dir,
    backend = "native",
    overwrite = TRUE
  )

  roundtrip <- read_annotation(
    record = record,
    record_dir = tmp_dir,
    annotator = annotator,
    backend = "native"
  )

  expect_equal(roundtrip, ann)

  original_path <- fs::path(record_dir, paste0(record, ".", annotator))
  new_path <- fs::path(tmp_dir, paste0(record, ".", annotator))

  expect_true(fs::file_exists(new_path))

  original_size <- as.integer(fs::file_size(original_path))
  new_size <- as.integer(fs::file_size(new_path))
  expect_equal(new_size, original_size)

  original_raw <- readBin(original_path, what = "raw", n = original_size)
  new_raw <- readBin(new_path, what = "raw", n = new_size)
  expect_equal(new_raw, original_raw)
})
