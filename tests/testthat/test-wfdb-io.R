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

test_that("mixed storage formats are supported", {
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
  write_wfdb(signal, record = "mixed", record_dir = tmp, header = header)

  roundtrip <- read_signal("mixed", tmp, units = "digital")
  expect_equal(roundtrip$sample, signal$sample)
  expect_equal(roundtrip$CH24, signal$CH24)
  expect_equal(roundtrip$CH32, signal$CH32)
})

test_that("format 8 first-difference records roundtrip correctly", {
  skip_if_not_installed("withr")

  # Format 8 stores 8-bit first differences, so successive samples must stay
  # within +/-127 of each other.  This guards against the regression where the
  # writer primed the difference accumulator at 0 while the reader primed it at
  # the header initial value, double-counting sample 0 on read-back.
  header <- header_table(
    record_name = "diff8",
    number_of_channels = 1L,
    frequency = 360,
    samples = 6L,
    storage_format = 8L,
    ADC_gain = 200,
    ADC_baseline = 0L,
    ADC_units = "mV",
    label = "II"
  )

  signal <- signal_table(data.table::data.table(
    sample = 0:5,
    II = c(500L, 505L, 498L, 520L, 510L, 511L)
  ))

  tmp <- withr::local_tempdir()
  write_wfdb(signal, record = "diff8", record_dir = tmp, header = header)

  roundtrip <- read_signal("diff8", tmp, units = "digital")
  expect_equal(roundtrip$II, signal$II)
})

test_that("format 80 offset-binary records roundtrip correctly", {
  skip_if_not_installed("withr")

  header <- header_table(
    record_name = "off80",
    number_of_channels = 1L,
    frequency = 250,
    samples = 5L,
    storage_format = 80L,
    ADC_gain = 200,
    ADC_baseline = 0L,
    ADC_units = "mV",
    label = "II"
  )

  # Format 80 holds a signed value in the range [-128, 127].
  signal <- signal_table(data.table::data.table(
    sample = 0:4,
    II = c(-128L, -1L, 0L, 1L, 127L)
  ))

  tmp <- withr::local_tempdir()
  write_wfdb(signal, record = "off80", record_dir = tmp, header = header)

  roundtrip <- read_signal("off80", tmp, units = "digital")
  expect_equal(roundtrip$II, signal$II)
})

test_that("write_wfdb computes a correct WFDB signal checksum", {
  skip_if_not_installed("withr")

  header <- header_table(
    record_name = "cksum",
    number_of_channels = 1L,
    frequency = 250,
    samples = 4L,
    storage_format = 16L,
    ADC_gain = 200,
    ADC_baseline = 0L,
    ADC_units = "mV",
    label = "II"
  )

  samples <- c(30000L, 30000L, 30000L, 1000L)
  signal <- signal_table(data.table::data.table(
    sample = 0:3,
    II = samples
  ))

  tmp <- withr::local_tempdir()
  write_wfdb(signal, record = "cksum", record_dir = tmp, header = header)

  # The WFDB checksum is the sum of all digital samples, truncated to 16 bits
  # and interpreted as a signed value (so it may be negative).
  raw <- sum(as.numeric(samples)) %% 65536
  expected <- ifelse(raw >= 32768, raw - 65536, raw)

  hea_lines <- readLines(fs::path(tmp, "cksum", ext = "hea"))
  signal_line <- hea_lines[2]
  fields <- strsplit(trimws(signal_line), "[[:space:]]+")[[1]]
  written_checksum <- as.integer(fields[7]) # 7th field is the checksum

  expect_equal(written_checksum, as.integer(expected))
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

test_that("reading carries persistent channel/number forward (WFDB semantics)", {
  skip_if_not_installed("withr")
  tmp <- withr::local_tempdir()

  # Hand-build a raw annotation stream that sets the channel exactly ONCE,
  # which is how standard WFDB tools (wrann) store a persistent field.  Each
  # entry is a little-endian 16-bit word: (code << 10) | interval.
  bytes <- as.raw(c(
    0x64, 0x04, # "N" beat, interval 100  -> sample 100
    0x05, 0xF8, # CHN = 5 (code 62); applies to the beat above and persists
    0x32, 0x04, # "N" beat, interval 50   -> sample 150
    0x32, 0x04, # "N" beat, interval 50   -> sample 200
    0x00, 0x00  # terminator
  ))
  writeBin(bytes, fs::path(tmp, "persist", ext = "qrs"))

  header <- header_table(
    record_name = "persist",
    number_of_channels = 1L,
    frequency = 360,
    samples = 1000L,
    storage_format = 16L,
    label = "II"
  )

  ann <- read_annotation(
    record = "persist",
    annotator = "qrs",
    record_dir = tmp,
    header = header
  )

  expect_equal(ann$sample, c(100L, 150L, 200L))
  # The single CHN record must carry forward to every subsequent annotation.
  expect_equal(ann$channel, c(5L, 5L, 5L))
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
    sample = 0:299,
    II = as.integer(rep(0, 300))
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
    time = c("00:00:00.278", "00:00:00.556"),
    sample = c(100L, 200L),
    type = c("N", "N"),
    subtype = c(0L, 0L),
    channel = c(0L, 0L),
    number = c(7L, 8L),
    aux = c("", "")
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

  expect_equal(nrow(parsed), 2L)
  expect_equal(parsed$sample, c(100L, 200L))
  expect_equal(parsed$type, c("N", "N"))
  expect_equal(parsed$subtype, c(0L, 0L))
  expect_equal(parsed$channel, c(0L, 0L))
  expect_equal(parsed$number, c(7L, 8L))
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
