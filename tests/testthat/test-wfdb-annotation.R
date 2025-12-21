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

  expect_s3_class(x, "EGM")

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

test_that("EGM constructor validates annotation list", {
  # Valid list of annotations
  ann_list <- list(
    ecgpuwave = annotation_table(annotator = "ecgpuwave"),
    atr = annotation_table(annotator = "atr")
  )

  x <- EGM(
    signal = signal_table(),
    header = header_table(),
    annotation = ann_list
  )

  expect_s3_class(x, "EGM")
  expect_type(x$annotation, "list")

  # Invalid - unnamed list
  expect_error(
    EGM(
      annotation = list(
        annotation_table(),
        annotation_table()
      )
    ),
    "must be named"
  )

  # Invalid - non-annotation_table in list
  expect_error(
    EGM(annotation = list(foo = data.frame())),
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

  x <- EGM(annotation = ann)

  # Should be converted to named list
  expect_type(x$annotation, "list")
  expect_named(x$annotation, "test")
  expect_s3_class(x$annotation$test, "annotation_table")

  # Empty annotation_table should be converted to unnamed list with empty annotation_table
  y <- EGM(annotation = annotation_table())
  expect_type(y$annotation, "list")
  expect_length(y$annotation, 1)
  expect_null(names(y$annotation))
  expect_s3_class(y$annotation[[1]], "annotation_table")
  expect_equal(nrow(y$annotation[[1]]), 0)
})

# add_annotation tests --------------------------------------------------------

test_that("add_annotation adds new annotation to empty EGM", {
  # Create an empty egm object
  x <- EGM()

  # Create a new annotation table
  ann <- annotation_table(
    annotator = "test",
    time = "00:00:01",
    sample = 360,
    type = "N",
    subtype = 0,
    channel = 0,
    number = 0
  )

  # Add annotation
  result <- add_annotation(x, ann)

  expect_s3_class(result, "EGM")
  expect_type(result$annotation, "list")
  expect_named(result$annotation, "test")
  expect_s3_class(result$annotation$test, "annotation_table")
  expect_equal(nrow(result$annotation$test), 1)
})

test_that("add_annotation adds annotation to EGM with existing annotations", {
  skip_on_ci()

  # Read egm with existing annotation
  x <- read_wfdb(
    record = "300",
    record_dir = test_path(),
    annotator = "ecgpuwave"
  )

  # Create a new annotation with different annotator name
  ann <- annotation_table(
    annotator = "custom",
    time = "00:00:01",
    sample = 360,
    type = "N",
    subtype = 0,
    channel = 0,
    number = 0
  )

  # Add annotation
  result <- add_annotation(x, ann)

  # Should have both annotators
  annotators <- list_annotators(result)
  expect_length(annotators, 2)
  expect_true("ecgpuwave" %in% annotators)
  expect_true("custom" %in% annotators)

  # New annotation should be present
  expect_s3_class(result$annotation$custom, "annotation_table")
  expect_equal(nrow(result$annotation$custom), 1)
})

test_that("add_annotation overwrites existing annotation when overwrite = TRUE", {
  # Create egm with annotation
  ann1 <- annotation_table(
    annotator = "test",
    time = "00:00:01",
    sample = 360,
    type = "N",
    subtype = 0,
    channel = 0,
    number = 0
  )

  x <- EGM(annotation = ann1)

  # Create new annotation with same annotator name
  ann2 <- annotation_table(
    annotator = "test",
    time = "00:00:02",
    sample = 720,
    type = "V",
    subtype = 0,
    channel = 0,
    number = 0
  )

  # Add with overwrite = TRUE
  result <- add_annotation(x, ann2, overwrite = TRUE)

  # Should have only one annotation with the new data
  expect_length(list_annotators(result), 1)
  expect_equal(nrow(result$annotation$test), 1)
  expect_equal(result$annotation$test$sample, 720)
  expect_equal(result$annotation$test$type, "V")
})

test_that("add_annotation merges annotations when overwrite = FALSE", {
  # Create egm with annotation
  ann1 <- annotation_table(
    annotator = "test",
    time = "00:00:01",
    sample = 360,
    type = "N",
    subtype = 0,
    channel = 0,
    number = 0
  )

  x <- EGM(annotation = ann1)

  # Create new annotation with same annotator name
  ann2 <- annotation_table(
    annotator = "test",
    time = "00:00:02",
    sample = 720,
    type = "V",
    subtype = 0,
    channel = 0,
    number = 0
  )

  # Add with overwrite = FALSE (default)
  result <- add_annotation(x, ann2, overwrite = FALSE)

  # Should have merged annotations
  expect_length(list_annotators(result), 1)
  expect_equal(nrow(result$annotation$test), 2)

  # Should be sorted by sample
  expect_equal(result$annotation$test$sample, c(360, 720))
  expect_equal(result$annotation$test$type, c("N", "V"))
})

test_that("add_annotation removes duplicates when merging", {
  # Create egm with annotation
  ann1 <- annotation_table(
    annotator = "test",
    time = "00:00:01",
    sample = 360,
    type = "N",
    subtype = 0,
    channel = 0,
    number = 0
  )

  x <- EGM(annotation = ann1)

  # Create annotation with duplicate entry
  ann2 <- annotation_table(
    annotator = "test",
    time = c("00:00:01", "00:00:02"),
    sample = c(360, 720),
    type = c("N", "V"),
    subtype = c(0, 0),
    channel = c(0, 0),
    number = c(0, 0)
  )

  # Add - should merge and remove duplicate
  result <- add_annotation(x, ann2, overwrite = FALSE)

  # Should have only 2 unique annotations
  expect_equal(nrow(result$annotation$test), 2)
  expect_equal(result$annotation$test$sample, c(360, 720))
})

test_that("add_annotation validates input types", {
  # Create valid egm
  x <- EGM()

  # Invalid annotation (not annotation_table)
  expect_error(
    add_annotation(x, data.frame()),
  )

  # Invalid egm (not egm object)
  ann <- annotation_table(annotator = "test")
  expect_error(
    add_annotation(list(), ann),
  )
})

test_that("add_annotation validates annotator attribute", {
  x <- EGM()

  # Create annotation_table without annotator attribute
  ann <- annotation_table()
  attr(ann, "annotator") <- NULL

  # Should error
  expect_error(
    add_annotation(x, ann),
    "annotator.*attribute"
  )
})

test_that("add_annotation validates channels exist in header", {
  skip_on_ci()

  # Read real egm with multiple channels
  x <- read_wfdb(
    record = "300",
    record_dir = test_path()
  )

  # Get valid channels from header
  valid_channels <- x$header$number

  # Create annotation with invalid channel
  invalid_channel <- max(valid_channels) + 1
  ann <- annotation_table(
    annotator = "test",
    time = "00:00:01",
    sample = 360,
    type = "N",
    subtype = 0,
    channel = invalid_channel,
    number = 0
  )

  # Should error
  expect_error(
    add_annotation(x, ann),
    "invalid channels"
  )
})

test_that("add_annotation allows channel 0 (global channel)", {
  skip_on_ci()

  # Read real egm
  x <- read_wfdb(
    record = "300",
    record_dir = test_path()
  )

  # Create annotation with channel 0 (global/default)
  ann <- annotation_table(
    annotator = "test",
    time = "00:00:01",
    sample = 360,
    type = "N",
    subtype = 0,
    channel = 0,
    number = 0
  )

  # Should succeed (will produce message about adding annotation)
  expect_message({
    result <- add_annotation(x, ann)
  }, "Added annotation table")

  expect_s3_class(result, "EGM")
})

test_that("add_annotation validates sample range", {
  skip_on_ci()

  # Read real egm to get valid sample range
  x <- read_wfdb(
    record = "300",
    record_dir = test_path()
  )

  # Get max samples from header
  max_samples <- attr(x$header, "record_line")$samples

  # Create annotation with sample beyond valid range
  ann <- annotation_table(
    annotator = "test",
    time = "99:99:99",
    sample = max_samples + 1000,
    type = "N",
    subtype = 0,
    channel = 0,
    number = 0
  )

  # Should error
  expect_error(
    add_annotation(x, ann),
    "outside valid range"
  )
})

test_that("add_annotation allows samples within valid range", {
  skip_on_ci()

  # Read real egm
  x <- read_wfdb(
    record = "300",
    record_dir = test_path()
  )

  # Get max samples from header
  max_samples <- attr(x$header, "record_line")$samples

  # Create annotation with valid sample at upper bound
  ann <- annotation_table(
    annotator = "test",
    time = "00:00:01",
    sample = max_samples - 100,
    type = "N",
    subtype = 0,
    channel = 0,
    number = 0
  )

  # Should succeed (will produce message about adding annotation)
  expect_message({
    result <- add_annotation(x, ann)
  }, "Added annotation table")

  expect_s3_class(result, "EGM")
})

test_that("add_annotation returns EGM object", {
  x <- EGM()
  ann <- annotation_table(annotator = "test")

  result <- add_annotation(x, ann)

  expect_s3_class(result, "EGM")
  expect_s3_class(result, "list")
})

