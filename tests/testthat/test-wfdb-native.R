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

test_that("native writer round-trips 16-bit digital records", {
  tmp <- withr::local_tempdir()

  sig <- signal_table(
    sample = 0:9,
    lead1 = as.integer(seq(0L, 9L)),
    lead2 = as.integer(-seq(0L, 9L))
  )

  hea <- header_table(
    record_name = "roundtrip16",
    number_of_channels = 2L,
    frequency = 500L,
    samples = nrow(sig),
    storage_format = rep(16L, 2),
    ADC_gain = rep(200L, 2),
    ADC_baseline = rep(0L, 2),
    label = c("LEAD1", "LEAD2")
  )

  write_wfdb_native(
    data = sig,
    header = hea,
    record = "roundtrip16",
    record_dir = tmp,
    overwrite = TRUE
  )

  roundtrip <- read_signal_native("roundtrip16", record_dir = tmp)
  expect_equal(roundtrip$LEAD1, sig$lead1)
  expect_equal(roundtrip$LEAD2, sig$lead2)
})

test_that("channel labels are canonicalized to known definitions", {
  raw <- c("cs12", "his_prox", "unknown")
  canonical <- EGM:::native_canonicalize_labels(raw)

  expect_equal(canonical[[1]], "CS 1-2")
  expect_equal(canonical[[2]], "HIS PROX")
  expect_equal(canonical[[3]], "unknown")
})

test_that("native writer converts physical units back to digital", {
  tmp <- withr::local_tempdir()

  digital <- as.integer(c(-2000L, -1000L, 0L, 1000L, 2000L))
  gain <- 200
  baseline <- 0
  physical <- (digital - baseline) / gain

  sig <- signal_table(
    sample = 0:(length(physical) - 1L),
    lead = physical
  )

  hea <- header_table(
    record_name = "physical16",
    number_of_channels = 1L,
    frequency = 250L,
    samples = nrow(sig),
    storage_format = 16L,
    ADC_gain = gain,
    ADC_baseline = baseline,
    label = "LEAD"
  )

  write_wfdb_native(
    data = sig,
    header = hea,
    record = "physical16",
    record_dir = tmp,
    units = "physical",
    overwrite = TRUE
  )

  roundtrip <- read_signal_native(
    "physical16",
    record_dir = tmp,
    units = "digital"
  )
  expect_equal(roundtrip$LEAD, digital)
})

test_that("native backend reads and writes 32-bit WFDB records", {
  tmp <- withr::local_tempdir()

  base_values <- as.integer(seq_len(12L) * 100000L)
  sig <- signal_table(
    sample = 0:(length(base_values) - 1L),
    lead1 = base_values,
    lead2 = -base_values
  )

  hea <- header_table(
    record_name = "roundtrip32",
    number_of_channels = 2L,
    frequency = 360L,
    samples = nrow(sig),
    storage_format = rep(32L, 2),
    ADC_gain = rep(1L, 2),
    ADC_baseline = rep(0L, 2),
    label = c("lead1", "lead2")
  )

  write_wfdb_native(
    data = sig,
    header = hea,
    record = "roundtrip32",
    record_dir = tmp,
    overwrite = TRUE
  )

  roundtrip <- read_signal_native("roundtrip32", record_dir = tmp)
  expect_equal(roundtrip$LEAD1, sig$lead1)
  expect_equal(roundtrip$LEAD2, sig$lead2)
})

test_that("native backend reads and writes format 212 WFDB records", {
  tmp <- withr::local_tempdir()

  values <- as.integer(c(-2048L, -1024L, -1L, 0L, 1L, 1023L, 2047L, 0L))
  sig <- signal_table(
    sample = 0:(length(values) - 1L),
    "LEAD I" = values,
    "LEAD J" = rev(values)
  )

  hea <- header_table(
    record_name = "roundtrip212",
    number_of_channels = 2L,
    frequency = 128L,
    samples = nrow(sig),
    storage_format = rep(212L, 2),
    ADC_gain = rep(1L, 2),
    ADC_baseline = rep(0L, 2),
    label = c("LEAD_I", "LEAD_J")
  )

  write_wfdb_native(
    data = sig,
    header = hea,
    record = "roundtrip212",
    record_dir = tmp,
    overwrite = TRUE
  )

  roundtrip <- read_signal_native("roundtrip212", record_dir = tmp)
  expect_equal(roundtrip$LEAD_I, sig$LEAD_I)
  expect_equal(roundtrip$LEAD_J, sig$LEAD_J)
})

test_that("native writer aligns header labels with the signal table", {
  tmp <- withr::local_tempdir()

  sig <- signal_table(
    sample = 0:4,
    lead_a = as.integer(1:5),
    lead_b = as.integer(seq(10L, 50L, by = 10L))
  )

  hea <- header_table(
    record_name = "label-alignment",
    number_of_channels = 2L,
    frequency = 250L,
    samples = nrow(sig),
    storage_format = rep(16L, 2),
    ADC_gain = rep(200L, 2),
    ADC_baseline = rep(0L, 2),
    label = c("LEAD B", "LEAD A")
  )

  write_wfdb_native(
    data = sig,
    header = hea,
    record = "label-alignment",
    record_dir = tmp,
    overwrite = TRUE
  )

  roundtrip <- read_signal_native("label-alignment", record_dir = tmp)
  expect_equal(roundtrip$`LEAD B`, sig$lead_b)
  expect_equal(roundtrip$`LEAD A`, sig$lead_a)
})

test_that("can read in annotation files natively", {
  x <- read_annotation_native(
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

test_that("annotation channels align to header labels", {
  record <- "300"
  record_dir <- test_path()
  header <- read_header(record, record_dir)
  ann <- read_annotation_native(
    record = record,
    record_dir = record_dir,
    annotator = "ecgpuwave"
  )

  header_labels <- EGM:::native_canonicalize_labels(header$label)
  channel_values <- ann$channel
  known_channels <- channel_values[!channel_values %in% c("0", NA_character_)]

  expect_true(all(known_channels %in% header_labels))
})

test_that("write_annotation_native matches channel names to header", {
  tmp <- withr::local_tempdir()

  hea <- header_table(
    record_name = "annot", 
    number_of_channels = 2L,
    frequency = 500L,
    samples = 100L,
    storage_format = rep(16L, 2),
    ADC_gain = rep(200L, 2),
    ADC_baseline = rep(0L, 2),
    label = c("cs12", "his_prox")
  )

  ann <- annotation_table(
    annotator = "qa",
    sample = c(0L, 10L),
    type = c("N", "N"),
    subtype = c(0L, 0L),
    channel = c("CS 1-2", "HIS PROX"),
    number = c(0L, 0L)
  )

  expect_no_error(write_annotation_native(
    data = ann,
    record = "annot",
    annotator = "qa",
    record_dir = tmp,
    overwrite = TRUE,
    header = hea
  ))

  expect_true(fs::file_exists(fs::path(tmp, "annot.qa")))
})


test_that("can read in annotations natively with faulty signal safely", {
  # Bad ECG that has no signal
  record <- "bad-ecg"
  record_dir <- test_path()

  x <- suppressMessages(read_wfdb_native(
    record,
    record_dir,
    annotator = "ecgpuwave"
  ))
  expect_s3_class(read_header(record, record_dir), "header_table")
  expect_s3_class(read_signal_native(record, record_dir), "signal_table")
  expect_message({
    ann <- read_annotation_native(record, record_dir, annotator = "ecgpuwave")
  })
  expect_length(ann, 6)
  expect_equal(nrow(ann), 0)
})
