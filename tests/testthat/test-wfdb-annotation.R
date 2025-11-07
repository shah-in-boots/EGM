test_that("can read in annotation files", {
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

test_that("annotation read in uses appropriate header data", {
  skip_on_ci()

  hea <- read_header("ecg", test_path())
})

# Multi-annotator tests -------------------------------------------------------

test_that("can read multiple annotators at once", {
  skip_on_ci()

  # Read multiple annotators
  anns <- read_annotation(
    record = "300",
    record_dir = test_path(),
    annotator = c("ecgpuwave", "atr")
  )

  # Should return a named list
  expect_type(anns, "list")
  expect_named(anns, c("ecgpuwave", "atr"))
  expect_length(anns, 2)

  # Each element should be an annotation_table
  expect_s3_class(anns$ecgpuwave, "annotation_table")
  expect_s3_class(anns$atr, "annotation_table")

  # Check that each has the expected structure
  expect_named(
    anns$ecgpuwave,
    c("time", "sample", "type", "subtype", "channel", "number")
  )
  expect_named(
    anns$atr,
    c("time", "sample", "type", "subtype", "channel", "number")
  )
})

test_that("single annotator returns annotation_table directly", {
  skip_on_ci()

  # Single annotator should return annotation_table (backwards compatible)
  ann <- read_annotation(
    record = "300",
    record_dir = test_path(),
    annotator = "ecgpuwave"
  )

  expect_s3_class(ann, "annotation_table")
  expect_s3_class(ann, "data.table")
})

test_that("read_wfdb handles multiple annotators", {
  skip_on_ci()

  # Read with multiple annotators
  x <- read_wfdb(
    record = "300",
    record_dir = test_path(),
    annotator = c("ecgpuwave", "atr")
  )

  expect_s3_class(x, "egm")

  # Annotation should be a list
  expect_type(x$annotation, "list")
  expect_named(x$annotation, c("ecgpuwave", "atr"))

  # Each annotation should be an annotation_table
  expect_s3_class(x$annotation$ecgpuwave, "annotation_table")
  expect_s3_class(x$annotation$atr, "annotation_table")
})

test_that("get_annotation extracts specific annotator", {
  skip_on_ci()

  # Create egm with multiple annotators
  x <- read_wfdb(
    record = "300",
    record_dir = test_path(),
    annotator = c("ecgpuwave", "atr")
  )

  # Get specific annotator
  ecg_ann <- get_annotation(x, "ecgpuwave")
  expect_s3_class(ecg_ann, "annotation_table")
  expect_equal(attr(ecg_ann, "annotator"), "ecgpuwave")

  atr_ann <- get_annotation(x, "atr")
  expect_s3_class(atr_ann, "annotation_table")
  expect_equal(attr(atr_ann, "annotator"), "atr")

  # Get all annotations
  all_anns <- get_annotation(x)
  expect_type(all_anns, "list")
  expect_named(all_anns, c("ecgpuwave", "atr"))

  # Error on missing annotator
  expect_error(
    get_annotation(x, "nonexistent"),
    "not found"
  )
})

test_that("list_annotators returns annotator names", {
  skip_on_ci()

  # With multiple annotators
  x <- read_wfdb(
    record = "300",
    record_dir = test_path(),
    annotator = c("ecgpuwave", "atr")
  )

  annotators <- list_annotators(x)
  expect_type(annotators, "character")
  expect_equal(annotators, c("ecgpuwave", "atr"))

  # With no annotators
  y <- read_wfdb(
    record = "300",
    record_dir = test_path()
  )

  empty_annotators <- list_annotators(y)
  expect_type(empty_annotators, "character")
  expect_length(empty_annotators, 0)
})

test_that("merge_annotations combines multiple annotators", {
  skip_on_ci()

  # Create egm with multiple annotators
  x <- read_wfdb(
    record = "300",
    record_dir = test_path(),
    annotator = c("ecgpuwave", "atr")
  )

  # Merge all annotators
  merged <- merge_annotations(x)

  # Should have annotator column
  expect_true("annotator" %in% names(merged))

  # Should have both annotators represented
  expect_true("ecgpuwave" %in% merged$annotator)
  expect_true("atr" %in% merged$annotator)

  # Should be data.table
  expect_s3_class(merged, "data.table")

  # Merge specific annotators only
  merged_subset <- merge_annotations(x, annotators = "ecgpuwave")
  expect_true(all(merged_subset$annotator == "ecgpuwave"))

  # Error on missing annotator
  expect_error(
    merge_annotations(x, annotators = "nonexistent"),
    "not found"
  )
})

test_that("egm constructor validates annotation list", {
  # Valid list of annotations
  ann_list <- list(
    ecgpuwave = annotation_table(annotator = "ecgpuwave"),
    atr = annotation_table(annotator = "atr")
  )

  x <- egm(
    signal = signal_table(),
    header = header_table(),
    annotation = ann_list
  )

  expect_s3_class(x, "egm")
  expect_type(x$annotation, "list")

  # Invalid - unnamed list
  expect_error(
    egm(
      annotation = list(
        annotation_table(),
        annotation_table()
      )
    ),
    "must be named"
  )

  # Invalid - non-annotation_table in list
  expect_error(
    egm(annotation = list(foo = data.frame())),
    "must be annotation_table"
  )
})

test_that("single annotation_table is converted to list", {
  # Single annotation_table should be converted to list
  ann <- annotation_table(
    annotator = "test",
    time = "00:00:01",
    sample = 360,
    type = "N",
    subtype = 0,
    channel = 0,
    number = 0
  )

  x <- egm(annotation = ann)

  # Should be converted to named list
  expect_type(x$annotation, "list")
  expect_named(x$annotation, "test")
  expect_s3_class(x$annotation$test, "annotation_table")

  # Empty annotation_table should be converted to unnamed list with empty annotation_table
  y <- egm(annotation = annotation_table())
  expect_type(y$annotation, "list")
  expect_length(y$annotation, 1)
  expect_null(names(y$annotation))
  expect_s3_class(y$annotation[[1]], "annotation_table")
  expect_equal(nrow(y$annotation[[1]]), 0)
})
