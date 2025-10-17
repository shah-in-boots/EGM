# Tests for native WFDB annotation readers and writers

test_that("read_annotation_native parses annotations", {
  data_dir <- testthat::test_path("..", "..", "inst", "extdata")
  header <- read_header_native("muse-sinus", record_dir = data_dir)
  ann <- read_annotation_native(
    record = "muse-sinus",
    annotator = "ecgpuwave",
    record_dir = data_dir,
    header = header
  )

  expect_s3_class(ann, "annotation_table")
  expect_gt(nrow(ann), 0)
  expect_true(all(c("time", "sample", "type", "subtype", "channel", "number") %in% names(ann)))
  expect_type(ann$sample, "integer")
  expect_type(ann$type, "character")
  expect_true(all(ann$sample >= 0))
})

test_that("read_annotation_native respects begin and end windows", {
  data_dir <- testthat::test_path("..", "..", "inst", "extdata")
  header <- read_header_native("muse-sinus", record_dir = data_dir)
  frequency <- attr(header, "record_line")$frequency

  ann_window <- read_annotation_native(
    record = "muse-sinus",
    annotator = "ecgpuwave",
    record_dir = data_dir,
    begin = 0.25,
    end = 0.75,
    header = header
  )

  if (nrow(ann_window) > 0) {
    expect_true(min(ann_window$sample) >= floor(0.25 * frequency))
    expect_true(max(ann_window$sample) <= ceiling(0.75 * frequency))
  } else {
    succeed()
  }
})

test_that("write_annotation_native produces round-trip compatible files", {
  data_dir <- testthat::test_path("..", "..", "inst", "extdata")
  header <- read_header_native("muse-sinus", record_dir = data_dir)
  ann <- read_annotation_native(
    record = "muse-sinus",
    annotator = "ecgpuwave",
    record_dir = data_dir,
    header = header
  )

  tmp_dir <- withr::local_tempdir()
  write_annotation_native(
    data = ann,
    annotator = "ecgpuwave",
    record = "muse-native",
    record_dir = tmp_dir
  )

  ann_roundtrip <- read_annotation_native(
    record = "muse-native",
    annotator = "ecgpuwave",
    record_dir = tmp_dir,
    header = header
  )

  expect_equal(ann_roundtrip$sample, ann$sample)
  expect_equal(ann_roundtrip$type, ann$type)
  expect_equal(ann_roundtrip$subtype, ann$subtype)
  expect_equal(ann_roundtrip$channel, ann$channel)
  expect_equal(ann_roundtrip$number, ann$number)
})
