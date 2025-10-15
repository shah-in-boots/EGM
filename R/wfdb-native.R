#' Native WFDB readers without external utilities
#'
#' @description
#' These functions provide experimental replacements for the existing WFDB
#' readers that rely on the external `wfdb` command line utilities. The native
#' implementations focus on stability and currently support 16-bit interleaved
#' WFDB records stored in a single signal file. When provided with a matching
#' pair of `.hea` and `.dat` files, the native reader parses the header using
#' [read_header()] and streams the binary signal data directly from the
#' `.dat` file without invoking any system commands.
#'
#' @details
#' The native backend mirrors the record-selection semantics of
#' [read_signal()] and the WFDB `rdsamp` utility for the supported subset of
#' records. Unlike the C-based `rdsamp`, which links against the full WFDB
#' library and understands the entire range of WFDB storage formats, the
#' current implementation is intentionally conservative and focuses on the most
#' common configuration used by the bundled example data (a single interleaved
#' 16-bit signal file). This trade-off keeps the initial implementation simple
#' and portable while we iterate towards broader format coverage.
#'
#' @param record Character scalar giving the record name (without extension).
#' @param record_dir Directory that contains the WFDB files.
#' @param annotator Optional annotator to read. Annotation support is not yet
#'   available for the native backend and an empty table is returned when an
#'   annotator is requested.
#' @param begin,end,interval Numeric values expressed in seconds that describe
#'   the portion of the record that should be read. Only one of `end` or
#'   `interval` should be supplied.
#' @param units Either "digital" (default) or "physical" indicating whether to
#'   return raw integer samples or scaled values.
#' @param channels Optional character or numeric vector specifying the channels
#'   to retain.
#' @param ... Reserved for future extensions.
#'
#' @return `read_signal_native()` returns a [signal_table] that mirrors the
#'   structure produced by [read_signal()]. `read_wfdb_native()` returns an
#'   [egm] object composed of the native signal data, the parsed header and an
#'   empty annotation table.
#'
#' @seealso [read_signal()], [read_wfdb()]
#'
#' @export
read_signal_native <- function(record,
                               record_dir = ".",
                               begin = 0,
                               end = NA_real_,
                               interval = NA_real_,
                               units = c("digital", "physical"),
                               channels = character(),
                               ...) {

  units <- match.arg(units)
  if (begin < 0) {
    stop("`begin` must be non-negative", call. = FALSE)
  }
  if (!is.na(end) && !is.na(interval)) {
    stop("Specify only one of `end` or `interval`", call. = FALSE)
  }

  header <- read_header(record, record_dir = record_dir)
  record_line <- attributes(header)$record_line

  frequency <- record_line$frequency
  samples <- record_line$samples

  if (is.null(frequency) || is.na(frequency)) {
    stop("Header is missing the sampling frequency; native reading is not available", call. = FALSE)
  }

  if (is.null(samples) || is.na(samples)) {
    stop("Header is missing the total number of samples; native reading is not available", call. = FALSE)
  }

  start_sample <- as.integer(round(begin * frequency))
  if (!is.na(interval)) {
    stopifnot("`interval` must be non-negative" = interval >= 0)
    end_sample <- start_sample + as.integer(round(interval * frequency))
  } else if (!is.na(end)) {
    stopifnot("`end` must be greater than or equal to `begin`" = end >= begin)
    end_sample <- as.integer(round(end * frequency))
  } else {
    end_sample <- samples
  }

  end_sample <- min(end_sample, samples)
  if (end_sample <= start_sample) {
    stop("Requested sample range is empty", call. = FALSE)
  }

  n_samples <- end_sample - start_sample

  file_names <- unique(header$file_name)
  if (length(file_names) != 1L) {
    stop("Native reader currently supports records stored in a single signal file", call. = FALSE)
  }

  storage_format <- unique(header$storage_format)
  if (length(storage_format) != 1L) {
    stop("Native reader requires a single storage format across channels", call. = FALSE)
  }

  bytes_per_sample <- storage_format / 8L
  if (!bytes_per_sample %in% c(2L)) {
    stop("Only 16-bit WFDB records are supported by the native reader", call. = FALSE)
  }

  signal_path <- fs::path(record_dir, file_names[[1]])
  if (!fs::file_exists(signal_path)) {
    stop("Signal file not found at ", signal_path, call. = FALSE)
  }
  n_channels <- nrow(header)

  # Load the selected range from the binary signal file using the
  # cpp11-backed helper. The helper returns a numeric matrix with each column
  # representing a channel and each row representing a sample.
  raw_matrix <- read_wfdb_dat_cpp(
    path = signal_path,
    n_channels = n_channels,
    start_sample = start_sample,
    n_samples = n_samples,
    bytes_per_sample = bytes_per_sample
  )

  channel_names <- as.character(header$label)
  if (length(channel_names) != n_channels) {
    channel_names <- paste0("channel_", seq_len(n_channels))
  }

  selected_header <- header
  if (length(channels) > 0) {
    idx <- if (is.numeric(channels)) {
      as.integer(channels)
    } else {
      match(toupper(channels), toupper(channel_names))
    }

    if (anyNA(idx)) {
      stop("One or more requested channels could not be matched", call. = FALSE)
    }

    raw_matrix <- raw_matrix[, idx, drop = FALSE]
    channel_names <- channel_names[idx]
    selected_header <- header[idx, ]
  }

  # Convert the dense matrix returned from C++ into the signal_table format
  # used throughout the package. The first column is the implicit sample index
  # and the remaining columns store the per-channel measurements.
  dat <- data.table::as.data.table(raw_matrix)
  if (ncol(dat) > 0) {
    data.table::setnames(dat, channel_names)
  }
  dat[, sample := seq.int(from = start_sample, length.out = n_samples)]
  data.table::setcolorder(dat, c("sample", setdiff(names(dat), "sample")))

  # For digital units, cast each column back to integer so downstream code can
  # rely on the same data types exposed by the CLI-backed readers.
  if (identical(units, "digital") && length(channel_names) > 0) {
    for (col in channel_names) {
      data.table::set(dat, j = col, value = as.integer(dat[[col]]))
    }
  }

  # Apply per-channel gain and baseline corrections when the caller requests
  # physical units. Each column is adjusted independently using the metadata
  # stored in the header record.
  if (identical(units, "physical") && (ncol(dat) > 1)) {
    gain <- as.numeric(selected_header$ADC_gain)
    baseline <- as.numeric(selected_header$ADC_baseline)
    zero <- as.numeric(selected_header$ADC_zero)

    baseline[is.na(baseline)] <- zero[is.na(baseline)]
    gain[gain %in% c(0, NA)] <- 1

    for (j in seq_along(channel_names)) {
      col <- channel_names[[j]]
      dat[[col]] <- (dat[[col]] - baseline[[j]]) / gain[[j]]
    }
  }

  signal_table(dat)
}

#' @rdname read_signal_native
#' @export
read_wfdb_native <- function(record,
                             record_dir = ".",
                             annotator = NA_character_,
                             begin = 0,
                             end = NA_real_,
                             interval = NA_real_,
                             units = c("digital", "physical"),
                             channels = character(),
                             ...) {

  sig <- read_signal_native(
    record = record,
    record_dir = record_dir,
    begin = begin,
    end = end,
    interval = interval,
    units = units,
    channels = channels,
    ...
  )

  hea <- read_header(record, record_dir = record_dir)

  if (!is.na(annotator)) {
    rlang::warn("Annotation reading is not yet available in the native WFDB backend; returning an empty table")
  }

  egm(
    signal = sig,
    header = hea,
    annotation = annotation_table()
  )
}
