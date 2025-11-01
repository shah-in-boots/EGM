# WFDB ------------------------------------------------------------------------

#' Waveform Database (WFDB) Software Package
#'
#' @author
#' Original software: George Moody, Tom Pollard, Benjamin Moody \cr
#' R implementation: Anish S. Shah \cr
#' Last updated: `r Sys.Date()` \cr
#'
#' @description
#' This implementation of WFDB is a back-end for the WFDB using a combination of
#' _python_, _C++_, and _C_ language. The related functions are documented
#' separately. This serves as an overview of the conversion of WFDB formats to R
#' formats. In this documentation, the specific WFDB generated files will be
#' described.
#'
#' @details
#' # WFDB
#'
#' The WFDB (Waveform Database) Software Package has been developed over the
#' past thirty years, providing a large collection of software for processing
#' and analyzing physiological waveforms. The package is written in highly
#' portable C and can be used on all popular platforms, including GNU/Linux,
#' MacOS X, MS-Windows, and all versions of Unix.
#'
#' The foundation of the WFDB Software Package is the WFDB library,
#' consisting of a set of functions for reading and writing digitized signals
#' and annotations. These functions can be used by programs written in C, C++,
#' or Fortran, running under any operating system for which an ANSI/ISO C
#' compiler is available, including all versions of Unix, MS-DOS, MS-Windows,
#' the Macintosh OS, and VMS.
#'
#' # Data format
#'
#' The records that the WFDB uses have three components...
#'
#' 1. Signals: integer values that are at equal intervals at a certain sampling
#' frequency
#'
#' 1. Header attributes: recording information such as sample number, gain,
#' sampling frequency
#'
#' 1. Annotations: information about the record such as a beat labels or alarm
#' triggers
#'
#' @param record String that will be used to name the WFDB record. Cannot
#'   include extensions, and is not a filepath. alphanumeric characters are
#'   acceptable, as well as hyphens (-) and underscores (_)
#'
#' @param record_dir File path of directory that should be used read and write
#'   files. Defaults to current directory.
#'
#' @param annotator String that is the name of a WFDB-compatible annotation
#'   type, serving as the extension for the file that is written containing that
#'   annotation. Please see [read_annotation()] and [write_annotation()] for
#'   further details.
#'
#' @param ... Additional arguments to be passed to the function
#'
#' @name wfdb
NULL

# Writing WFDB format data -----------------------------------------------------

#' I/O of WFDB-compatible signal & header files from EP recording systems
#'
#' @description
#' This function allows for WFDB files to be read from any WFDB-compatible
#' system, and also allows writing out WFDB-compatible files from specific EP
#' recording systems, as indicated in the details section. Writing WFDB leads to
#' creation of both a __dat__ (signal) and __hea__ (header) file. These are both
#' required for reading in files as well.
#'
#' @details
#' # Recording systems
#'
#' Type of signal data, as specified by the recording system, that are currently
#' supported.
#'
#' * _bard_ = Bard (LabSystem Pro), e.g. [read_bard()]
#'
#' * _muse_ = MUSE (GE), e.g. [read_muse()]
#'
#' * _prucka_ = Prucka (CardioLab), e.g. [read_prucka()]
#'
#' @inheritParams wfdb
#'
#' @param data Can either be an `egm` object, or a `data.frame` (or similar)
#'   object. The function will appropriately set defaults based on the type.
#'
#'   * `egm` = Will extract signal and header data directly from object, and thus is simplest to convert to a WFDB format
#'
#'   * `signal_table` = This is a customized `data.table` class that has an invariant column containing sample information.
#'
#'   * `data.frame` or `data.table` = Must have a column that represents a time point or index, and columns that represent signal values (preferably integers)
#'
#' @param header A header file is an optional named list of parameters that
#'   will be used to organize and describe the signal input from the __data__
#'   argument. If the __type__ is given, specific additional elements will be
#'   searched for, such as the low or high pass filters, colors, or other signal
#'   attributes. At minimum, the following elements are required (as cannot be
#'   calculated):
#'
#'   * frequency = sample frequency in Hertz as `integer`
#'
#'   * label = vector of names for each channel as `character`
#'
#'   * start_time = date/time object
#'
#' @param begin,end,interval Timepoint as an `integer` (representing seconds),
#'   which is converted to an index position based on sampling frequency. The
#'   default is to start at the beginning of the record. If `end` or `interval`
#'   are given, the earlier of the two will be returned. The `end` argument
#'   gives a time index to read until. The `interval` argument is the length of
#'   time past the start point.
#'
#' @param units A `character` string representing either *digital* (DEFAULT) or *physical*
#'   units that should be used for signal values.
#'
#'   * `"digital"` = Returns raw ADC (analog-to-digital converter) counts as stored
#'   in the .dat file. These are integer values representing the digitized signal
#'   without any scaling applied. Use this to preserve exact round-trip fidelity.
#'
#'   * `"physical"` = Returns signal values converted to physical units (e.g., mV)
#'   using the formula: `physical = (digital - baseline) / gain`, where `baseline`
#'   and `gain` are specified in the header file. This is the human-readable format
#'   for analysis.
#'
#' @param channels Either the signal/channel in a `character` vector as a name or number.
#'   Allows for duplication of signal or to re-order signal if needed. If
#'   nothing is given, will default to all channels available.
#'
#' @param info_strings A `list` of strings that will be written as an appendix
#'   to the header file, usually containing information about the channels,
#'   (e.g. list of colors, extra labels, etc).
#'
#' @returns Depends on if it is a reading or writing function. For writing, will
#'   output an WFDB-based object reflecting the function. For reading, will
#'   output an extension of a `data.table` object reflecting the underlying
#'   function (e.g. `signal_table()` will return an object of class).
#'
#' @details
#' The `begin`, `end`, and `interval` arguments are converted into sample
#' positions using the sampling frequency declared in the WFDB header. The
#' reader first determines the starting sample from `begin`, then gives
#' precedence to `interval` (when supplied) before falling back to `end`. Any
#' request that extends beyond the recorded range is clamped so that the caller
#' still receives all available data without a hard failure.
#'
#' @name wfdb_io
NULL

#' @describeIn wfdb_io Writes out signal and header data into a WFDB-compatible
#'   format from R. The `units` parameter indicates whether the input signal data
#'   is in digital (raw ADC counts) or physical units. When `units="physical"`,
#'   the function automatically converts to digital units using the inverse formula:
#'   `digital = (physical * gain) + baseline` before writing to disk.
#'
#' @export
write_wfdb <- function(
  data,
  record,
  record_dir = ".",
  header = NULL,
  info_strings = list(),
  units = c("digital", "physical"),
  ...
) {
  units <- match.arg(units)
  # Create the output directory up-front so subsequent file writes do not
  # fail midway through the export pipeline.
  if (!fs::dir_exists(record_dir)) {
    fs::dir_create(record_dir, recurse = TRUE)
  }

  if (inherits(data, "egm")) {
    if (!is.null(header)) {
      message(
        "Ignoring the supplied `header` because `data` is an `egm` object; its embedded header already contains the metadata required for WFDB exports."
      )
    }
    signal <- data$signal
    header <- data$header
  } else {
    signal <- signal_table(data)
  }

  if (is.null(header)) {
    stop(
      "A header_table must be supplied when `data` is not an `egm` object"
    )
  }
  if (!inherits(header, "header_table")) {
    stop("`header` must be a `header_table` object")
  }

  # The parsed record line is stored as an attribute, allowing us to reuse
  # metadata calculated during header import without reparsing the file.
  record_line <- attr(header, "record_line")
  frequency <- record_line$frequency
  start_time <- record_line$start_time
  if (!inherits(start_time, "POSIXt") || is.na(start_time)) {
    start_time_str <- ""
  } else {
    start_time_str <- format(start_time, "%H:%M:%OS %d/%m/%Y")
  }

  # Fall back to the original record name stored in the header when the
  # caller does not supply one explicitly. This preserves compatibility
  # with existing WFDB datasets that expect a specific prefix.
  original_record_name <- record_line$record_name
  if (length(original_record_name) == 0) {
    original_record_name <- NA_character_
  } else {
    original_record_name <- as.character(original_record_name)[1]
  }
  record_name <- record
  if (
    is.null(record_name) ||
      length(record_name) == 0 ||
      is.na(record_name) ||
      record_name == ""
  ) {
    record_name <- original_record_name
  }
  if (
    length(record_name) == 0 ||
      is.na(record_name) ||
      record_name == ""
  ) {
    stop("`record` must be supplied or present in the header")
  }
  record_name <- as.character(record_name)[1]

  # Align the signal table to the header ordering. WFDB requires the
  # sample column followed by one column per channel in the same order as
  # the header entries.
  signal_dt <- data.table::as.data.table(signal)
  header_labels <- as.character(header$label)
  header_labels[is.na(header_labels)] <- ""
  if (all(header_labels == "")) {
    header_labels <- paste0("CH", header$number)
  }
  signal_cols <- setdiff(names(signal_dt), "sample")
  if (length(signal_cols) == 0) {
    stop("Signal data must contain at least one channel")
  }
  match_idx <- match(header_labels, signal_cols)
  if (any(is.na(match_idx))) {
    if (length(signal_cols) == nrow(header)) {
      match_idx <- seq_along(signal_cols)
    } else {
      stop("Signal columns do not align with header labels")
    }
  }
  channel_cols <- signal_cols[match_idx]
  label_output <- header_labels
  label_output[label_output == ""] <- paste0(
    "CH",
    seq_along(label_output)
  )
  channel_data <- signal_dt[,
    channel_cols,
    with = FALSE
  ]
  channel_list <- as.list(channel_data)
  signal_matrix <- matrix(
    data = unlist(channel_list, use.names = FALSE),
    nrow = nrow(channel_data),
    ncol = length(channel_cols),
    dimnames = list(NULL, channel_cols)
  )

  file_names <- as.character(header$file_name)
  file_names[is.na(file_names)] <- ""
  default_file_name <- paste0(record_name, ".dat")
  legacy_name <- paste0(original_record_name, ".dat")
  replace_idx <- file_names == "" |
    (!is.na(original_record_name) &
      original_record_name != "" &
      file_names == legacy_name)
  if (all(replace_idx)) {
    file_names[] <- default_file_name
  } else {
    file_names[replace_idx] <- default_file_name
  }
  unique_files <- unique(file_names)
  if (length(unique_files) > 1) {
    stop(
      "Multiple signal files per record are not currently supported"
    )
  }

  # Coerce channel metadata to basic vectors and replace missing values
  # with sensible defaults so the C++ writer receives explicit parameters
  # for every field it expects.
  storage_format <- as.integer(header$storage_format)
  storage_format[is.na(storage_format)] <- 16L
  adc_gain <- as.numeric(header$ADC_gain)
  adc_baseline <- as.integer(header$ADC_baseline)
  adc_units <- as.character(header$ADC_units)
  adc_resolution <- as.integer(header$ADC_resolution)
  adc_zero <- as.integer(header$ADC_zero)
  initial_value <- as.integer(header$initial_value)
  checksum <- as.integer(header$checksum)
  blocksize <- as.integer(header$blocksize)

  existing_info <- attr(header, "info_strings")
  if (is.null(existing_info)) {
    existing_info <- list()
  }
  # Merge the info strings bundled with the header and any additions from
  # the caller. The WFDB header syntax requires each entry to be named so
  # we validate that before writing anything to disk.
  combined_info <- c(existing_info, info_strings)
  if (length(combined_info) > 0) {
    if (
      is.null(names(combined_info)) ||
        any(names(combined_info) == "")
    ) {
      stop("All info strings must be named")
    }
    combined_info <- lapply(combined_info, as.character)
  }

  data_path <- fs::path(record_dir, unique_files[[1]])
  header_path <- fs::path(record_dir, record_name, ext = "hea")

  # The heavy lifting happens inside the native writer, which handles the
  # byte-level encoding for both the signal and header files.
  write_wfdb_native_cpp(
    data_path = data_path,
    header_path = header_path,
    signal_matrix_sexp = signal_matrix,
    channel_names = label_output,
    file_names = file_names,
    storage_format = storage_format,
    adc_gain = adc_gain,
    adc_baseline = adc_baseline,
    adc_units = adc_units,
    adc_resolution = adc_resolution,
    adc_zero = adc_zero,
    initial_value = initial_value,
    checksum = checksum,
    blocksize = blocksize,
    frequency = frequency,
    samples = nrow(signal_matrix),
    record_name = record_name,
    start_time = start_time_str,
    info_strings = combined_info,
    physical = (units == "physical")
  )

  invisible(header_path)
}


# Reading WFDB format data -----------------------------------------------------

#' @describeIn wfdb_io Reads a multicomponent WFDB-formatted set of files
#'   directly into an `egm` object. This serves to pull together
#'   [read_signal()], [read_header()], and [read_annotation()] for simplicity.
#'
#' @export
read_wfdb <- function(
  record,
  record_dir = ".",
  annotator = NULL,
  begin = 0,
  end = NA_integer_,
  interval = NA_integer_,
  units = c("digital", "physical"),
  channels = character(),
  ...
) {
  # Load the shared header once so it can be passed to both the signal and
  # optional annotation readers without re-parsing the file.
  header <- read_header(record = record, record_dir = record_dir)

  signal <- read_signal(
    record = record,
    record_dir = record_dir,
    header = header,
    begin = begin,
    end = end,
    interval = interval,
    units = units,
    channels = channels
  )

  # If annotatator is present, read annotation in as well
  if (!is.null(annotator)) {
    annotation <- read_annotation(
      record = record,
      record_dir = record_dir,
      annotator = annotator,
      header = header
    )
  } else {
    annotation <- annotation_table()
  }

  egm(signal = signal, header = header, annotation = annotation)
}


#' @describeIn wfdb_io Specifically reads the signal data from the WFDB binary
#'   format, returning a `signal_table` object for evaluation in the R
#'   environment
#'
#' @export
read_signal <- function(
  record,
  record_dir = ".",
  header = NULL,
  begin = 0,
  end = NA_integer_,
  interval = NA_integer_,
  units = c("digital", "physical"),
  channels = character(),
  ...
) {
  units <- match.arg(units)

  if (is.null(header)) {
    header <- read_header(
      record = record,
      record_dir = record_dir
    )
  }
  record_line <- attr(header, "record_line")
  frequency <- record_line$frequency
  total_samples <- record_line$samples
  number_of_channels <- record_line$number_of_channels

  # Translate the requested time window into sample indices using the
  # header frequency. Missing or negative inputs default to the start of
  # the record so the native reader always receives a valid range.
  begin_sample <- as.integer(round(begin * frequency))
  if (is.na(begin_sample) || begin_sample < 0) {
    begin_sample <- 0L
  }

  if (!is.na(interval)) {
    end_sample <- begin_sample +
      as.integer(round(interval * frequency))
  } else if (!is.na(end)) {
    end_sample <- as.integer(round(end * frequency))
  } else {
    end_sample <- total_samples
  }
  if (is.na(end_sample) || end_sample <= 0) {
    end_sample <- total_samples
  }

  # Determine which channels to retrieve by matching the caller's request
  # against the header labels. Matching is forgiving to case differences
  # and supports both numeric indices and character names.
  labels <- as.character(header$label)
  labels[is.na(labels)] <- ""
  if (length(labels) == 0) {
    labels <- paste0("CH", seq_len(nrow(header)))
  }

  if (length(channels) == 0) {
    selection <- seq_len(nrow(header))
  } else if (is.numeric(channels)) {
    selection <- as.integer(channels)
  } else {
    selection <- match(channels, labels)
    missing <- which(is.na(selection))
    if (length(missing) > 0) {
      selection[missing] <- match(
        toupper(channels[missing]),
        toupper(labels)
      )
    }
  }

  if (any(is.na(selection))) {
    stop(
      "Requested channels could not be matched to the header definition"
    )
  }
  selection <- as.integer(selection)
  if (any(selection < 1L | selection > nrow(header))) {
    stop("Requested channels are outside the available range")
  }

  channel_names <- labels[selection]
  channel_names[channel_names == ""] <- paste0("CH", selection)

  file_names <- unique(as.character(header$file_name))
  file_names[file_names == ""] <- paste0(record, ".dat")
  file_names <- unique(file_names)
  if (length(file_names) != 1) {
    stop(
      "Multiple signal files per record are not currently supported"
    )
  }

  # Delegate decoding of the binary signal file to the C++ implementation.
  # Channel indices are converted to zero-based offsets to match the WFDB
  # on-disk layout.
  signal_list <- read_signal_native_cpp(
    data_path = fs::path(record_dir, file_names),
    number_of_channels = number_of_channels,
    total_samples = total_samples,
    storage_format = as.integer(header$storage_format),
    begin_sample = begin_sample,
    end_sample = end_sample,
    channel_indices = as.integer(header$number[selection] - 1L),
    adc_gain = as.numeric(header$ADC_gain),
    adc_baseline = as.integer(header$ADC_baseline),
    physical = identical(units, "physical"),
    channel_names = channel_names
  )

  signal <- data.table::as.data.table(signal_list)
  signal[, sample := as.integer(sample)]
  signal_table(signal)
}

#' @describeIn wfdb_io Specifically reads the header data from the WFDB header
#'   text format, returning a `header_table` object for evaluation in the R
#'   environment
#'
#' @export
read_header <- function(record, record_dir = ".", ...) {
  header_path <- fs::path(record_dir, record, ext = "hea")
  if (!fs::file_exists(header_path)) {
    stop(record, " not found in ", record_dir)
  }

  # Parse the header using the C++ helper which mirrors the WFDB
  # specification. The resulting list is then massaged into the structured
  # `header_table` S3 class returned to R callers.
  header_info <- read_header_native_cpp(header_path)
  start_time <- parse_date_and_time(header_info$record_line)
  if (is.na(start_time)) {
    start_time <- formals(header_table)$start_time
  }

  info_strings <- header_info$info_strings
  if (!is.null(info_strings) && length(info_strings) > 0) {
    info_strings <- lapply(info_strings, as.character)
  } else {
    info_strings <- list()
  }

  record_name <- header_info$record_name
  if (
    length(record_name) == 0 ||
      is.na(record_name) ||
      record_name == ""
  ) {
    record_name <- record
  }

  header_table(
    record_name = record_name,
    number_of_channels = header_info$number_of_channels,
    frequency = header_info$frequency,
    samples = header_info$samples,
    start_time = start_time,
    file_name = header_info$file_name,
    storage_format = as.integer(header_info$storage_format),
    ADC_gain = as.numeric(header_info$adc_gain),
    ADC_baseline = as.integer(header_info$adc_baseline),
    ADC_units = as.character(header_info$adc_units),
    ADC_resolution = as.integer(header_info$adc_resolution),
    ADC_zero = as.integer(header_info$adc_zero),
    initial_value = as.integer(header_info$initial_value),
    checksum = as.integer(header_info$checksum),
    blocksize = as.integer(header_info$blocksize),
    label = as.character(header_info$label),
    info_strings = info_strings
  )
}
