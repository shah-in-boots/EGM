test_that("native header reader parses WFDB headers", {
        fp <- system.file("extdata", "muse-sinus.hea", package = "EGM")
        dir <- fs::path_dir(fp)
        header <- read_header_native("muse-sinus", dir)

        expect_s3_class(header, "header_table")
        expect_equal(nrow(header), 12L)
        expect_equal(attr(header, "record_line")$frequency, 500)
        expect_equal(attr(header, "record_line")$number_of_channels, 12L)
})

test_that("native signal reader returns a signal_table", {
        fp <- system.file("extdata", "muse-sinus.dat", package = "EGM")
        dir <- fs::path_dir(fp)
        header <- read_header_native("muse-sinus", dir)
        signal <- read_signal_native("muse-sinus", dir, header = header)

        expect_s3_class(signal, "signal_table")
        expect_equal(ncol(signal), 13L)
        expect_equal(nrow(signal), attr(header, "record_line")$samples)
})

test_that("native reader returns an egm object", {
        fp <- system.file("extdata", "muse-sinus.dat", package = "EGM")
        dir <- fs::path_dir(fp)
        egm_obj <- read_wfdb_native("muse-sinus", dir)

        expect_s3_class(egm_obj, "egm")
        expect_s3_class(egm_obj$signal, "signal_table")
        expect_s3_class(egm_obj$header, "header_table")
})

test_that("native writer produces WFDB files", {
        skip_on_cran()
        skip_if_not_installed("withr")

        fp <- system.file("extdata", "muse-sinus.dat", package = "EGM")
        dir <- fs::path_dir(fp)
        egm_obj <- read_wfdb_native("muse-sinus", dir)

        tmp <- withr::local_tempdir()
        write_wfdb_native(egm_obj, record = "native-test", record_dir = tmp)

        expect_true(fs::file_exists(fs::path(tmp, "native-test.dat")))
        expect_true(fs::file_exists(fs::path(tmp, "native-test.hea")))

        roundtrip <- read_wfdb_native("native-test", tmp)
        expect_equal(nrow(roundtrip$signal), nrow(egm_obj$signal))
        expect_equal(ncol(roundtrip$signal), ncol(egm_obj$signal))
})

test_that("format 212 records roundtrip correctly", {
        skip_on_cran()
        skip_if_not_installed("withr")

        header <- header_table(
                record_name = "pair",
                number_of_channels = 2L,
                frequency = 360,
                samples = 4L,
                storage_format = c(212L, 212L),
                ADC_gain = c(200, 200),
                ADC_baseline = c(0L, 0L),
                ADC_units = c("mV", "mV"),
                label = c("I", "II")
        )

        signal <- signal_table(data.table::data.table(
                sample = 0:3,
                I = c(-1000L, -500L, 0L, 1023L),
                II = c(500L, -250L, 2047L, -2048L + 10L)
        ))

        tmp <- withr::local_tempdir()
        write_wfdb_native(signal, record = "pair", record_dir = tmp, header = header)

        roundtrip <- read_wfdb_native("pair", tmp)
        expect_equal(roundtrip$signal$sample, signal$sample)
        expect_equal(roundtrip$signal$I, signal$I)
        expect_equal(roundtrip$signal$II, signal$II)
})

test_that("mixed storage formats are supported", {
        skip_on_cran()
        skip_if_not_installed("withr")

        header <- header_table(
                record_name = "mixed",
                number_of_channels = 2L,
                frequency = 125,
                samples = 5L,
                storage_format = c(24L, 32L),
                ADC_gain = c(200, 200),
                ADC_baseline = c(0L, 0L),
                ADC_units = c("mV", "mV"),
                label = c("CH24", "CH32")
        )

        signal <- signal_table(data.table::data.table(
                sample = 0:4,
                CH24 = c(-500000L, -100L, 0L, 100L, 500000L),
                CH32 = c(-100000L, -1L, 0L, 1L, 100000L)
        ))

        tmp <- withr::local_tempdir()
        write_wfdb_native(signal, record = "mixed", record_dir = tmp, header = header)

        roundtrip <- read_signal_native("mixed", tmp, units = "digital")
        expect_equal(roundtrip$sample, signal$sample)
        expect_equal(roundtrip$CH24, signal$CH24)
        expect_equal(roundtrip$CH32, signal$CH32)
})


# Native annotation ----
# Tests for native WFDB annotation readers and writers

test_that("read_annotation_native parses annotations", {
  data_dir <- testthat::test_path()
  header <- read_header_native("ecg", record_dir = data_dir)
  ann <- read_annotation_native(
    record = "ecg",
    annotator = "ecgpuwave",
    record_dir = data_dir,
    header = header
  )

  # Should be similar even if header is not given
  ann2 <- read_annotation_native(
    record = "ecg",
    annotator = "ecgpuwave",
    record_dir = data_dir
  )

  expect_equal(ann, ann2)

  expect_s3_class(ann, "annotation_table")
  expect_gt(nrow(ann), 0)
  expect_true(all(c("time", "sample", "type", "subtype", "channel", "number") %in% names(ann)))
  expect_type(ann$sample, "integer")
  expect_type(ann$type, "character")
  expect_true(all(ann$sample >= 0))
})

test_that("read_annotation_native respects begin and end windows", {
  data_dir <- testthat::test_path()
  header <- read_header_native("ecg", record_dir = data_dir)
  frequency <- attr(header, "record_line")$frequency

  ann_window <- read_annotation_native(
    record = "ecg",
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
  data_dir <- testthat::test_path()
  header <- read_header_native("ecg", record_dir = data_dir)
  ann <- read_annotation_native(
    record = "ecg",
    annotator = "ecgpuwave",
    record_dir = data_dir,
    header = header
  )

  tmp_dir <- withr::local_tempdir()
  write_annotation_native(
    data = ann,
    annotator = "ecgpuwave",
    record = "ecg-native",
    record_dir = tmp_dir
  )

  ann_roundtrip <- read_annotation_native(
    record = "ecg-native",
    annotator = "ecgpuwave",
    record_dir = tmp_dir,
    header = header
  )

  expect_equal(ann_roundtrip, ann)
  expect_equal(ann_roundtrip$sample, ann$sample)
  expect_equal(ann_roundtrip$type, ann$type)
  expect_equal(ann_roundtrip$subtype, ann$subtype)
  expect_equal(ann_roundtrip$channel, ann$channel)
  expect_equal(ann_roundtrip$number, ann$number)
})
