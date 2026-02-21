# Read and access annotations -------------------------------------------------

test_that("read_annotation returns expected structure", {
  ann <- read_annotation(
    record = "300",
    record_dir = test_path(),
    annotator = "ecgpuwave"
  )

  expect_s3_class(ann, "annotation_table")
  expect_named(
    ann,
    c("time", "sample", "type", "subtype", "channel", "number", "aux")
  )
  expect_true(all(ann$sample >= 0L))
})

test_that("read_annotation supports multiple annotators", {
  anns <- read_annotation(
    record = "300",
    record_dir = test_path(),
    annotator = c("ecgpuwave", "atr")
  )

  expect_type(anns, "list")
  expect_named(anns, c("ecgpuwave", "atr"))
  expect_s3_class(anns$ecgpuwave, "annotation_table")
  expect_s3_class(anns$atr, "annotation_table")
})

test_that("read_wfdb stores multiple annotators", {
  x <- read_wfdb(
    record = "300",
    record_dir = test_path(),
    annotator = c("ecgpuwave", "atr")
  )

  expect_s3_class(x, "EGM")
  expect_type(x$annotation, "list")
  expect_named(x$annotation, c("ecgpuwave", "atr"))
})

test_that("empty annotation files are handled safely", {
  record <- "bad-ecg"
  record_dir <- test_path()

  expect_message({
    ann <- read_annotation(record, record_dir, annotator = "ecgpuwave")
  })

  expect_s3_class(ann, "annotation_table")
  expect_equal(nrow(ann), 0)
})

test_that("annotation helper functions work for multi-annotator EGM", {
  x <- read_wfdb(
    record = "300",
    record_dir = test_path(),
    annotator = c("ecgpuwave", "atr")
  )

  expect_equal(list_annotators(x), c("ecgpuwave", "atr"))
  expect_s3_class(get_annotation(x, "ecgpuwave"), "annotation_table")
  expect_error(get_annotation(x, "missing"), "not found")

  merged <- merge_annotations(x)
  expect_s3_class(merged, "data.table")
  expect_true("annotator" %in% names(merged))
  expect_true(all(c("ecgpuwave", "atr") %in% merged$annotator))
})

# add_annotation -------------------------------------------------------------

small_test_egm <- function() {
  signal <- signal_table(data.table::data.table(
    sample = 0:999,
    II = as.integer(rep(0, 1000))
  ))

  header <- header_table(
    record_name = "toy",
    number_of_channels = 1L,
    frequency = 360,
    samples = nrow(signal),
    storage_format = 16L,
    ADC_gain = 200,
    ADC_baseline = 0L,
    ADC_units = "mV",
    label = "II"
  )

  EGM(signal = signal, header = header)
}

test_that("add_annotation can add, merge, and overwrite", {
  x <- EGM()

  ann1 <- annotation_table(
    annotator = "test",
    time = "00:00:01",
    sample = 360L,
    type = "N",
    subtype = 0L,
    channel = 0L,
    number = 0L
  )
  ann2 <- annotation_table(
    annotator = "test",
    time = c("00:00:01", "00:00:02"),
    sample = c(360L, 720L),
    type = c("N", "V"),
    subtype = c(0L, 0L),
    channel = c(0L, 0L),
    number = c(0L, 0L)
  )

  added <- add_annotation(x, ann1)
  expect_s3_class(added, "EGM")
  expect_equal(nrow(added$annotation$test), 1)

  merged <- add_annotation(added, ann2, overwrite = FALSE)
  expect_equal(merged$annotation$test$sample, c(360L, 720L))

  replaced <- add_annotation(merged, ann1, overwrite = TRUE)
  expect_equal(nrow(replaced$annotation$test), 1)
  expect_equal(replaced$annotation$test$sample, 360L)
})

test_that("add_annotation validates channel and sample bounds", {
  x <- small_test_egm()

  invalid_channel <- annotation_table(
    annotator = "test",
    time = "00:00:01",
    sample = 100L,
    type = "N",
    subtype = 0L,
    channel = 2L,
    number = 0L
  )
  expect_error(add_annotation(x, invalid_channel), "invalid channels")

  invalid_sample <- annotation_table(
    annotator = "test",
    time = "00:00:10",
    sample = 2000L,
    type = "N",
    subtype = 0L,
    channel = 0L,
    number = 0L
  )
  expect_error(add_annotation(x, invalid_sample), "outside valid range")
})

test_that("add_annotation validates inputs", {
  expect_error(add_annotation(EGM(), data.frame()))

  ann <- annotation_table(annotator = "test")
  expect_error(add_annotation(list(), ann))

  ann_no_attr <- annotation_table()
  attr(ann_no_attr, "annotator") <- NULL
  expect_error(add_annotation(EGM(), ann_no_attr), "annotator.*attribute")
})
