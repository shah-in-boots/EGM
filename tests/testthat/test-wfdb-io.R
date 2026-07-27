# Helpers -----------------------------------------------------------------

skip_if_no_rdann <- function() {
  skip_on_cran()
  skip_on_ci()
  if (Sys.which("rdann") == "") {
    skip("rdann is not available")
  }
}

# Writing WFDB records -----------------------------------------------------

test_that('write_wfdb honours explicit headers and preserves integer storage', {
  skip_if_not_installed('withr')

  sig <- signal_table(channel = 0:4)
  default_header <- header_table(
    record_name = 'base',
    number_of_channels = 1L,
    frequency = 250L,
    samples = nrow(sig),
    storage_format = 16L,
    label = 'BASE'
  )
  EGM_obj <- EGM(sig, default_header)

  override_header <- header_table(
    record_name = 'override',
    number_of_channels = 1L,
    frequency = 360L,
    samples = nrow(sig),
    storage_format = 16L,
    label = 'BASE'
  )

  tmp <- withr::local_tempdir()
  captured <- NULL

  expect_message(
    with_mocked_bindings(
      write_wfdb_native_cpp = function(
        data_path,
        header_path,
        signal_matrix_sexp,
        channel_names,
        file_names,
        storage_format,
        adc_gain,
        adc_baseline,
        adc_units,
        adc_resolution,
        adc_zero,
        initial_value,
        checksum,
        blocksize,
        frequency,
        samples,
        record_name,
        start_time,
        info_strings,
        physical = FALSE
      ) {
        captured <<- list(
          signal_matrix = signal_matrix_sexp,
          frequency = frequency
        )
        invisible(NULL)
      },
      {
        write_wfdb(
          EGM_obj,
          record = 'custom',
          record_dir = tmp,
          header = override_header
        )
      }
    ),
    "Ignoring the supplied `header`"
  )

  expect_false(is.null(captured))
  expect_true(is.integer(captured$signal_matrix))
  expect_equal(
    captured$frequency,
    attr(default_header, 'record_line')$frequency
  )
})

# Reading WFDB records -----------------------------------------------------

test_that("native header reader parses WFDB headers", {
  fp <- system.file("extdata", "muse-sinus.hea", package = "EGM")
  dir <- fs::path_dir(fp)
  header <- read_header("muse-sinus", dir)

  expect_s3_class(header, "header_table")
  expect_equal(nrow(header), 12L)
  expect_equal(attr(header, "record_line")$frequency, 500)
  expect_equal(attr(header, "record_line")$number_of_channels, 12L)
})

test_that("native signal reader returns a signal_table", {
  fp <- system.file("extdata", "muse-sinus.dat", package = "EGM")
  dir <- fs::path_dir(fp)
  header <- read_header("muse-sinus", dir)
  signal <- read_signal("muse-sinus", dir, header = header)

  expect_s3_class(signal, "signal_table")
  expect_equal(ncol(signal), 13L)
  expect_equal(nrow(signal), attr(header, "record_line")$samples)
})

test_that("signal windows use clock times and duration intervals", {
  data_dir <- testthat::test_path()
  header <- read_header("ecg", record_dir = data_dir)
  full <- read_signal("ecg", record_dir = data_dir, header = header)

  by_character <- read_signal(
    "ecg",
    record_dir = data_dir,
    header = header,
    begin = "00:00:00.020",
    end = "00:00:00.030"
  )
  expect_identical(by_character$sample, 10:14)
  expect_equal(by_character[[2]], full[[2]][11:15])

  by_elapsed_time <- read_signal(
    "ecg",
    record_dir = data_dir,
    header = header,
    begin = as.difftime(0.02, units = "secs"),
    end = as.difftime(0.03, units = "secs")
  )
  expect_identical(by_elapsed_time$sample, 10:14)
  expect_equal(by_elapsed_time[[2]], by_character[[2]])

  by_duration <- read_signal(
    "ecg",
    record_dir = data_dir,
    header = header,
    begin = as.difftime(0.02, units = "secs"),
    interval = "10 ms"
  )
  expect_identical(by_duration$sample, 10:14)

  start_time <- attr(header, "record_line")$start_time
  by_clock <- read_signal(
    "ecg",
    record_dir = data_dir,
    header = header,
    begin = start_time + 0.02,
    end = start_time + 0.03
  )
  expect_identical(by_clock$sample, 10:14)

  numeric_interval <- read_signal(
    "ecg",
    record_dir = data_dir,
    header = header,
    begin = as.difftime(1, units = "secs"),
    interval = 1
  )
  expect_identical(numeric_interval$sample, 500:999)

  clamped <- read_signal(
    "ecg",
    record_dir = data_dir,
    header = header,
    begin = "00:00:09.990",
    interval = "30s"
  )
  expect_identical(clamped$sample, 4995:4999)

  empty <- read_signal(
    "ecg",
    record_dir = data_dir,
    header = header,
    begin = "00:00:00",
    end = "00:00:00"
  )
  expect_equal(nrow(empty), 0L)
})

test_that("native reader returns an EGM object", {
  fp <- system.file("extdata", "muse-sinus.dat", package = "EGM")
  dir <- fs::path_dir(fp)
  EGM_obj <- read_wfdb("muse-sinus", dir)

  expect_s3_class(EGM_obj, "EGM")
  expect_s3_class(EGM_obj$signal, "signal_table")
  expect_s3_class(EGM_obj$header, "header_table")
})

test_that("native writer produces WFDB files", {
  skip_if_not_installed("withr")

  fp <- system.file("extdata", "muse-sinus.dat", package = "EGM")
  dir <- fs::path_dir(fp)
  EGM_obj <- read_wfdb("muse-sinus", dir)

  tmp <- withr::local_tempdir()
  write_wfdb(EGM_obj, record = "native-test", record_dir = tmp)

  expect_true(fs::file_exists(fs::path(tmp, "native-test.dat")))
  expect_true(fs::file_exists(fs::path(tmp, "native-test.hea")))

  roundtrip <- read_wfdb("native-test", tmp)
  expect_equal(nrow(roundtrip$signal), nrow(EGM_obj$signal))
  expect_equal(ncol(roundtrip$signal), ncol(EGM_obj$signal))
})

test_that("writer rejects sample indices that do not match WFDB row order", {
  signal <- signal_table(data.table::data.table(
    sample = c(0L, 2L),
    I = c(1L, 2L)
  ))
  header <- header_table(
    record_name = "gapped",
    number_of_channels = 1L,
    frequency = 250,
    samples = 2L,
    storage_format = 16L,
    label = "I"
  )

  expect_error(
    write_wfdb(
      signal,
      record = "gapped",
      record_dir = withr::local_tempdir(),
      header = header
    ),
    "contiguous zero-based indices"
  )
})

test_that("format 212 records roundtrip correctly", {
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
  write_wfdb(signal, record = "pair", record_dir = tmp, header = header)

  roundtrip <- read_wfdb("pair", tmp)
  expect_equal(roundtrip$signal$sample, signal$sample)
  expect_equal(roundtrip$signal$I, signal$I)
  expect_equal(roundtrip$signal$II, signal$II)
})

test_that("format 212 supports one signal and an odd sample count", {
  skip_if_not_installed("withr")

  header <- header_table(
    record_name = "single212",
    number_of_channels = 1L,
    frequency = 360,
    samples = 5L,
    storage_format = 212L,
    label = "MLII"
  )
  signal <- signal_table(data.table::data.table(
    sample = 0:4,
    MLII = c(-2048L, -1L, 0L, 1L, 2047L)
  ))

  tmp <- withr::local_tempdir()
  write_wfdb(signal, "single212", tmp, header)
  roundtrip <- read_signal("single212", tmp)

  expect_equal(roundtrip$MLII, signal$MLII)
  expect_equal(as.numeric(fs::file_size(fs::path(tmp, "single212.dat"))), 9)
})

test_that("formats 61 and 160 roundtrip with C byte ordering", {
  skip_if_not_installed("withr")

  values <- c(-32768L, -100L, 0L, 100L, 32767L)
  for (format in c(61L, 160L)) {
    record <- paste0("format", format)
    header <- header_table(
      record_name = record,
      number_of_channels = 1L,
      frequency = 250,
      samples = length(values),
      storage_format = format,
      label = "VALUE"
    )
    signal <- signal_table(data.table::data.table(
      sample = seq_along(values) - 1L,
      VALUE = values
    ))

    tmp <- withr::local_tempdir()
    write_wfdb(signal, record, tmp, header)
    expect_equal(read_signal(record, tmp)$VALUE, values)
  }
})

test_that("format 8 uses the header initial value exactly once", {
  skip_if_not_installed("withr")

  signal <- signal_table(data.table::data.table(
    sample = 0:5,
    DIFF = c(50L, 55L, 45L, 60L, 61L, 40L)
  ))
  header <- header_table(
    record_name = "diff8",
    number_of_channels = 1L,
    frequency = 250,
    samples = nrow(signal),
    storage_format = 8L,
    label = "DIFF"
  )

  tmp <- withr::local_tempdir()
  write_wfdb(signal, record = "diff8", record_dir = tmp, header = header)

  roundtrip <- read_wfdb("diff8", tmp)
  expect_equal(roundtrip$signal$DIFF, signal$DIFF)
  expect_equal(roundtrip$header$initial_value, signal$DIFF[[1]])
})

test_that("writer computes C-compatible signal checksums", {
  skip_if_not_installed("withr")

  signal <- signal_table(data.table::data.table(
    sample = 0:3,
    I = c(32767L, 2L, -3L, -32768L),
    II = c(-100L, 50L, 25L, 10L)
  ))
  header <- header_table(
    record_name = "checksum",
    number_of_channels = 2L,
    frequency = 250,
    samples = nrow(signal),
    storage_format = c(16L, 16L),
    checksum = c(123L, 456L),
    label = c("I", "II")
  )

  tmp <- withr::local_tempdir()
  write_wfdb(signal, record = "checksum", record_dir = tmp, header = header)
  written_header <- read_header("checksum", tmp)

  signed_checksum <- function(x) {
    value <- sum(as.double(x)) %% 65536
    if (value >= 32768) value <- value - 65536
    as.integer(value)
  }
  expect_equal(
    written_header$checksum,
    c(signed_checksum(signal$I), signed_checksum(signal$II))
  )
})

test_that("header reader preserves nonstandard signal file names and defaults", {
  skip_if_not_installed("withr")

  tmp <- withr::local_tempdir()
  writeLines(
    c(
      "# provenance before record",
      "custom 1",
      "samples.bin 16 200 12 0 7 5 0 Mixed Case Lead"
    ),
    fs::path(tmp, "custom.hea")
  )
  writeBin(as.integer(c(7L, -2L)), fs::path(tmp, "samples.bin"),
    size = 2L, endian = "little"
  )

  header <- read_header("custom", tmp)
  signal <- read_signal("custom", tmp, header = header)

  expect_equal(header$file_name, "samples.bin")
  expect_equal(attr(header, "record_line")$frequency, 250)
  expect_equal(header$ADC_gain, 200)
  expect_equal(header$ADC_baseline, 0L)
  expect_equal(header$label, "Mixed Case Lead")
  expect_equal(attr(header, "info_strings")$provenance, c("before", "record"))
  expect_equal(signal[[2]], c(7, -2))
})

test_that("signal reader honours absolute file names in headers", {
  skip_if_not_installed("withr")

  tmp <- withr::local_tempdir()
  signal_path <- fs::path(tmp, "absolute.bin")
  writeBin(as.integer(c(11L, -4L)), signal_path,
    size = 2L, endian = "little"
  )
  writeLines(
    c("absolute 1 250 2", paste(signal_path, "16")),
    fs::path(tmp, "absolute.hea")
  )

  signal <- read_signal("absolute", tmp)
  expect_equal(signal[[2]], c(11, -4))
})

test_that("unsupported header format modifiers fail instead of misdecoding", {
  skip_if_not_installed("withr")

  tmp <- withr::local_tempdir()
  writeLines(
    c("modified 1 250 2", "modified.dat 16x2:1+8"),
    fs::path(tmp, "modified.hea")
  )

  expect_error(
    read_header("modified", tmp),
    "format modifiers are not currently supported"
  )
})

test_that("mixed storage formats in one signal group are rejected", {
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
  expect_error(
    write_wfdb(signal, record = "mixed", record_dir = tmp, header = header),
    "same storage format"
  )
})

# Native annotation ---------------------------------------------------------

test_that("read_annotation parses annotations", {
  data_dir <- testthat::test_path()
  header <- read_header("ecg", record_dir = data_dir)
  ann <- read_annotation(
    record = "ecg",
    annotator = "ecgpuwave",
    record_dir = data_dir,
    header = header
  )

  ann2 <- read_annotation(
    record = "ecg",
    annotator = "ecgpuwave",
    record_dir = data_dir
  )

  expect_equal(ann, ann2)
  expect_s3_class(ann, "annotation_table")
  expect_gt(nrow(ann), 0)
  expect_true(all(
    c("time", "sample", "type", "subtype", "channel", "number") %in% names(ann)
  ))
  expect_type(ann$sample, "integer")
  expect_type(ann$type, "character")
  expect_true(all(ann$sample >= 0))
})

test_that("read_annotation respects begin and end windows", {
  data_dir <- testthat::test_path()
  header <- read_header("ecg", record_dir = data_dir)
  frequency <- attr(header, "record_line")$frequency

  ann_window <- read_annotation(
    record = "ecg",
    annotator = "ecgpuwave",
    record_dir = data_dir,
    begin = "00:00:00.250",
    end = "00:00:00.750",
    header = header
  )

  if (nrow(ann_window) > 0) {
    expect_true(min(ann_window$sample) >= ceiling(0.25 * frequency))
    expect_true(max(ann_window$sample) < ceiling(0.75 * frequency))
  } else {
    succeed()
  }

  ann_clock <- read_annotation(
    record = "ecg",
    annotator = "ecgpuwave",
    record_dir = data_dir,
    begin = "00:00:00.710",
    end = "00:00:00.850",
    header = header
  )
  expect_identical(ann_clock$sample, c(355L, 374L, 406L))
})

test_that("read_wfdb applies the same sample window to annotations", {
  data_dir <- testthat::test_path()
  record <- read_wfdb(
    record = "ecg",
    record_dir = data_dir,
    annotator = "ecgpuwave",
    begin = "00:00:00.710",
    end = "00:00:00.850"
  )

  expect_identical(record$signal$sample, 355:424)
  expect_identical(record$annotation$ecgpuwave$sample, c(355L, 374L, 406L))
})

test_that("write_annotation produces round-trip compatible files", {
  skip_if_not_installed("withr")

  data_dir <- testthat::test_path()
  header <- read_header("ecg", record_dir = data_dir)
  ann <- read_annotation(
    record = "ecg",
    annotator = "ecgpuwave",
    record_dir = data_dir,
    header = header
  )

  tmp_dir <- withr::local_tempdir()
  write_annotation(
    data = ann,
    annotator = "ecgpuwave",
    record = "ecg-native",
    record_dir = tmp_dir
  )

  ann_roundtrip <- read_annotation(
    record = "ecg-native",
    annotator = "ecgpuwave",
    record_dir = tmp_dir,
    header = header
  )

  expect_equal(ann_roundtrip, ann)
})

test_that("write_annotation emits rdann-compatible modifier records", {
  skip_if_not_installed("withr")
  skip_if_no_rdann()

  tmp_dir <- withr::local_tempdir()

  signal <- signal_table(data.table::data.table(
    sample = 0:499,
    II = as.integer(rep(0, 500))
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
  write_wfdb(signal, record = "toy", record_dir = tmp_dir, header = header)

  ann <- annotation_table(
    annotator = "ecgpuwave",
    time = c("00:00:00.278", "00:00:00.556", "00:00:00.833", "00:00:01.111"),
    sample = c(100L, 200L, 300L, 400L),
    type = c("N", "N", "N", "N"),
    subtype = c(-1L, 0L, 0L, 0L),
    channel = c(2L, 2L, 0L, 0L),
    number = c(-7L, -7L, 0L, 0L),
    aux = c("", "", "", "")
  )
  write_annotation(
    data = ann,
    annotator = "ecgpuwave",
    record = "toy",
    record_dir = tmp_dir
  )

  rdann_output <- withr::with_dir(
    tmp_dir,
    system2(
      "rdann",
      c("-e", "-r", "toy", "-a", "ecgpuwave"),
      stdout = TRUE
    )
  )
  tokens <- strsplit(trimws(rdann_output), "[[:space:]]+")

  parsed <- data.table::data.table(
    sample = as.integer(vapply(tokens, `[[`, character(1), 2)),
    type = vapply(tokens, `[[`, character(1), 3),
    subtype = as.integer(vapply(tokens, `[[`, character(1), 4)),
    channel = as.integer(vapply(tokens, `[[`, character(1), 5)),
    number = as.integer(vapply(tokens, `[[`, character(1), 6))
  )

  expect_equal(nrow(parsed), 4L)
  expect_equal(parsed$sample, c(100L, 200L, 300L, 400L))
  expect_equal(parsed$type, rep("N", 4L))
  expect_equal(parsed$subtype, c(-1L, 0L, 0L, 0L))
  expect_equal(parsed$channel, c(2L, 2L, 0L, 0L))
  expect_equal(parsed$number, c(-7L, -7L, 0L, 0L))

  roundtrip <- read_annotation(
    record = "toy",
    annotator = "ecgpuwave",
    record_dir = tmp_dir
  )
  expect_equal(as.integer(roundtrip$subtype), ann$subtype)
  expect_equal(roundtrip$channel, ann$channel)
  expect_equal(roundtrip$number, ann$number)
})

# Digital/Physical Units and Baseline Handling -----------------------------

test_that('digital units preserve raw ADC values with non-zero baseline', {
  skip_if_not_installed('withr')

  tmp_dir <- withr::local_tempdir()

  signal <- signal_table(data.table::data.table(
    sample = 0:9,
    I = as.integer(c(1024, 1124, 1224, 1324, 1424, 1524, 1624, 1724, 1824, 1924)),
    II = as.integer(c(2048, 2148, 2248, 2348, 2448, 2548, 2648, 2748, 2848, 2948))
  ))

  header <- header_table(
    record_name = 'baseline-test',
    number_of_channels = 2L,
    frequency = 250,
    samples = nrow(signal),
    storage_format = c(16L, 16L),
    ADC_gain = c(200, 200),
    ADC_baseline = c(1024L, 2048L),
    ADC_units = c("mV", "mV"),
    label = c("I", "II")
  )

  EGM_obj <- EGM(signal, header)

  write_wfdb(
    data = EGM_obj,
    record = 'baseline-test',
    record_dir = tmp_dir,
    units = "digital"
  )

  EGM_read <- read_wfdb(
    record = 'baseline-test',
    record_dir = tmp_dir,
    units = "digital"
  )

  expect_equal(EGM_read$signal$I, signal$I)
  expect_equal(EGM_read$signal$II, signal$II)
  expect_equal(EGM_read$header$ADC_baseline, c(1024L, 2048L))
})

test_that('digital-to-physical-to-digital round-trip is exact', {
  skip_if_not_installed('withr')

  tmp_dir <- withr::local_tempdir()

  signal_digital <- signal_table(data.table::data.table(
    sample = 0:4,
    I = as.integer(c(1024, 1124, 1224, 1324, 1424)),
    II = as.integer(c(2048, 2248, 2448, 2648, 2848))
  ))

  header <- header_table(
    record_name = 'full-roundtrip',
    number_of_channels = 2L,
    frequency = 250,
    samples = nrow(signal_digital),
    storage_format = c(16L, 16L),
    ADC_gain = c(200, 200),
    ADC_baseline = c(1024L, 2048L),
    ADC_units = c("mV", "mV"),
    label = c("I", "II")
  )

  EGM_obj <- EGM(signal_digital, header)
  write_wfdb(
    data = EGM_obj,
    record = 'full-roundtrip',
    record_dir = tmp_dir,
    units = "digital"
  )

  EGM_physical <- read_wfdb(
    record = 'full-roundtrip',
    record_dir = tmp_dir,
    units = "physical"
  )

  expect_equal(EGM_physical$signal$I[1], 0.0, tolerance = 1e-10)
  expect_equal(EGM_physical$signal$I[3], 1.0, tolerance = 1e-10)
  expect_equal(EGM_physical$signal$II[1], 0.0, tolerance = 1e-10)
  expect_equal(EGM_physical$signal$II[3], 2.0, tolerance = 1e-10)

  write_wfdb(
    data = EGM_physical,
    record = 'full-roundtrip-2',
    record_dir = tmp_dir,
    units = "physical"
  )

  EGM_digital_final <- read_wfdb(
    record = 'full-roundtrip-2',
    record_dir = tmp_dir,
    units = "digital"
  )

  expect_equal(EGM_digital_final$signal$I, signal_digital$I, tolerance = 1)
  expect_equal(EGM_digital_final$signal$II, signal_digital$II, tolerance = 1)
})

# Signal units --------------------------------------------------------------

test_that('a signal table says which units it holds', {
  digital <- read_wfdb('ecg', test_path())
  physical <- read_wfdb('ecg', test_path(), units = 'physical')

  expect_equal(signal_units(digital), 'digital')
  expect_equal(signal_units(physical), 'physical')
  expect_equal(signal_units(physical$signal), 'physical')

  # The two differ by the ADC gain, which nothing in the values reveals
  gain <- as.numeric(digital$header$ADC_gain[[1]])
  expect_equal(
    as.numeric(physical$signal$II),
    as.numeric(digital$signal$II) / gain,
    tolerance = 1e-8
  )

  # A table built by hand is digital unless it says otherwise
  expect_equal(signal_units(signal_table(sample = 0:4, II = as.numeric(0:4))), 'digital')
  expect_equal(
    signal_units(signal_table(sample = 0:4, II = as.numeric(0:4), units = 'physical')),
    'physical'
  )
  expect_match(
    paste(capture.output(print(physical$signal)), collapse = ' '),
    'physical units'
  )
})

test_that('the units label survives the transforms', {
  physical <- read_wfdb('ecg-sinus', test_path(), 'ann', units = 'physical')

  beats <- suppressMessages(get_windows(physical, by = by_beat(channel = 2)))
  expect_equal(signal_units(beats[[1]]), 'physical')
  expect_equal(signal_units(median_window(beats)), 'physical')
  expect_equal(signal_units(change_frequency(physical, 250)), 'physical')
  expect_equal(signal_units(normalize_window(beats)[[1]]), 'physical')
  expect_equal(
    signal_units(pad_window(beats, target_samples = 900)[[1]]),
    'physical'
  )
  expect_equal(signal_units(as_ECG(physical)), 'physical')
})

test_that('write_wfdb refuses units that contradict the signal', {
  skip_if_not_installed('withr')

  physical <- read_wfdb('ecg', test_path(), units = 'physical')
  digital <- read_wfdb('ecg', test_path())

  withr::with_tempdir({
    # Writing physical values as though they were ADC counts is a gain-sized
    # error, and one that leaves no trace in the file
    expect_error(
      write_wfdb(physical, 'out', '.'),
      'labelled "physical"'
    )
    expect_no_error(write_wfdb(physical, 'out', '.', units = 'physical'))

    # A digital signal still round-trips on the defaults
    expect_no_error(write_wfdb(digital, 'digital', '.'))
    expect_equal(
      read_wfdb('digital', '.')$signal$II,
      digital$signal$II
    )
  })
})
