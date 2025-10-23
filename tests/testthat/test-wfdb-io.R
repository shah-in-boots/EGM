# Writing WFDB records ----

test_that('can convert bard to wfdb with wrsamp', {
  skip_on_ci()

  # Convert a bard text file into a WFDB compatible format
  wd <- getwd()

  file <- test_path('bard-egm.txt')
  bard <- read_bard(file)
  write_wfdb(
    data = bard,
    record = 'bard-egm',
    record_dir = test_path()
  )

  expect_true(file.exists(file.path(test_path(), 'bard-egm.hea')))
  expect_true(file.exists(file.path(test_path(), 'bard-egm.dat')))
  expect_equal(wd, getwd())
})

test_that('R data objects can be converted or written to WFDB format', {
  skip_on_ci()

  file <- test_path('bard-egm.txt')
  sig <- read_bard_signal(file)
  hea <- read_bard_header(file)
  rec <- attributes(hea)$record_line
  data <- egm(sig, hea)

  write_wfdb(
    data = data,
    record = 'bard-egm',
    record_dir = test_path()
  )

  headerFile <- readLines(test_path('bard-egm.hea'))
  expect_gt(length(headerFile), 14)
  expect_output(print(headerFile[1]), 'egm 14')

  file <- system.file('extdata', 'muse-sinus.xml', package = 'EGM')
  ecg <- read_muse(file)

  write_wfdb(
    data = ecg,
    record = 'ecg',
    record_dir = test_path()
  )
})

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
  egm_obj <- egm(sig, default_header)

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
        info_strings
      ) {
        captured <<- list(
          signal_matrix = signal_matrix_sexp,
          frequency = frequency
        )
        invisible(NULL)
      },
      {
        write_wfdb(
          egm_obj,
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

# Reading WFDB records ----

test_that('rdsamp can read in WFDB formatted files for signal data', {
  skip_on_ci()

  # Reads in EGM data (which is an EP study)
  x <- read_signal(
    record = 'egm',
    record_dir = test_path(),
    begin = 0L,
    units = 'digital'
  )

  expect_s3_class(x, 'data.frame')

  # Reads in ECG data
  y <- read_signal(
    record = 'ecg',
    record_dir = test_path(),
    begin = 0,
    units = 'digital'
  )

  expect_s3_class(y, 'data.frame')

  # Read in a ECG file from PhysioNet
  z <- read_signal(
    record = '300',
    record_dir = test_path(),
    begin = 20
  )

  expect_s3_class(z, 'signal_table')
})

test_that('internals of `read_header()` can create `header_table` from bard data', {
  fp <- test_path("egm.hea")

  record_line <- readLines(con = fp, n = 1)
  record_items <-
    record_line |>
    strsplit('\ ') |>
    unlist()

  record_name <- as.character(record_items[1])
  number_of_channels <- as.integer(record_items[2])
  frequency <- as.integer(record_items[3])
  samples <- as.integer(record_items[4])
  start_time <- parse_date_and_time(record_line)

  # Number of columns is important here
  sig_data <-
    data.table::fread(
      file = fp,
      skip = 1, # Skip head line
      nrows = number_of_channels
    ) # Read in channel data

  # ADC gain is in multiple parts that need to be split
  # Units will come after a forward slash `/`
  # Baseline value will be within parenthesis
  adc <- sig_data[[3]]
  ADC_gain <- stringr::str_extract(adc, '\\d+([.]\\d+)?')
  ADC_baseline <- stringr::str_extract(adc, "\\((\\d+)\\)", group = 1)
  ADC_baseline <-
    ifelse(is.na(ADC_baseline), formals(header_table)$ADC_zero, ADC_baseline)
  ADC_units <- stringr::str_extract(adc, "/([:alpha:]+)", group = 1)
  ADC_units <-
    ifelse(is.na(ADC_units), formals(header_table)$ADC_units, ADC_units)

  h <- header_table(
    record_name = record_name,
    number_of_channels = number_of_channels,
    frequency = frequency,
    samples = samples,
    start_time = start_time,
    file_name = sig_data[[1]],
    storage_format = sig_data[[2]],
    ADC_gain = ADC_gain,
    ADC_baseline = ADC_baseline,
    ADC_units = ADC_units,
    ADC_resolution = sig_data[[4]],
    ADC_zero = sig_data[[5]],
    initial_value = sig_data[[6]],
    checksum = sig_data[[7]],
    blocksize = sig_data[[8]],
    label = sig_data[[9]]
  )

  expect_s3_class(h, 'header_table')
  expect_equal(nrow(h), 14)
})

test_that('can read in WFDB file into `egm` directly', {
  skip_on_ci()

  # Basics
  record = 'ecg'
  record_dir = test_path()
  annotator = 'ecgpuwave'
  begin = 0
  end = NA_integer_
  interval = NA_integer_
  units = "digital"
  channels = character()

  x <- read_wfdb(
    record = record,
    record_dir = record_dir,
    annotator = annotator,
    begin = begin,
    end = end,
    interval = interval,
    units = units,
    channels = channels
  )

  expect_s3_class(x, 'egm')

  # From the stored package data

  rec <- 'muse-sinus'
  dir <- system.file('extdata', 'muse-sinus.dat', package = 'EGM')
  ecg <- read_wfdb(rec, fs::path_dir(dir))
})

test_that('can read in MUSE ECG header', {
  skip_on_ci()

  # Simple header
  hea <- read_header("ecg", record_dir = test_path())
  expect_equal(unique(hea$file_name), "ecg.dat")

  # Complex header
  fp <- system.file("extdata", "muse-sinus.hea", package = "EGM")
  hea <- read_header(
    record = fs::path_file(fp) |>
      fs::path_ext_remove(),
    record_dir = fs::path_dir(fp)
  )

  header <- readLines(fp)
  expect_equal(hea$color, unlist(strsplit(header[16], " "))[-c(1:2)])
})


# Specific testing for native features ----

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

test_that("native reader returns an egm object", {
  fp <- system.file("extdata", "muse-sinus.dat", package = "EGM")
  dir <- fs::path_dir(fp)
  egm_obj <- read_wfdb("muse-sinus", dir)

  expect_s3_class(egm_obj, "egm")
  expect_s3_class(egm_obj$signal, "signal_table")
  expect_s3_class(egm_obj$header, "header_table")
})

test_that("native writer produces WFDB files", {
  skip_if_not_installed("withr")

  fp <- system.file("extdata", "muse-sinus.dat", package = "EGM")
  dir <- fs::path_dir(fp)
  egm_obj <- read_wfdb("muse-sinus", dir)

  tmp <- withr::local_tempdir()
  write_wfdb(egm_obj, record = "native-test", record_dir = tmp)

  expect_true(fs::file_exists(fs::path(tmp, "native-test.dat")))
  expect_true(fs::file_exists(fs::path(tmp, "native-test.hea")))

  roundtrip <- read_wfdb("native-test", tmp)
  expect_equal(nrow(roundtrip$signal), nrow(egm_obj$signal))
  expect_equal(ncol(roundtrip$signal), ncol(egm_obj$signal))
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


# Native annotation ----
# Tests for native WFDB annotation readers and writers

test_that("read_annotation parses annotations", {
  data_dir <- testthat::test_path()
  header <- read_header("ecg", record_dir = data_dir)
  ann <- read_annotation(
    record = "ecg",
    annotator = "ecgpuwave",
    record_dir = data_dir,
    header = header
  )

  # Should be similar even if header is not given
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

test_that("write_annotation produces round-trip compatible files", {
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
  expect_equal(ann_roundtrip$sample, ann$sample)
  expect_equal(ann_roundtrip$type, ann$type)
  expect_equal(ann_roundtrip$subtype, ann$subtype)
  expect_equal(ann_roundtrip$channel, ann$channel)
  expect_equal(ann_roundtrip$number, ann$number)
})
