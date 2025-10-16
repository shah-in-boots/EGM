#' Native WFDB readers and writers without external utilities
#'
#' @description
#' These functions provide native replacements for the existing WFDB helpers
#' that rely on the external `wfdb` command line utilities. The implementation
#' operates entirely within R and C++ via [cpp11], avoiding any dependency on
#' the WFDB C library while maintaining compatibility with WFDB-formatted
#' records.
#'
#' @details
#' The native backend mirrors the record-selection semantics of [read_signal()]
#' and the WFDB `rdsamp` utility while supporting the three most common WFDB
#' storage formats—16-bit integers, 32-bit integers and the packed 12-bit
#' `212` format—for both reading and writing. The writer complements
#' [write_wfdb()] by emitting WFDB-compatible `.hea` and `.dat` files without
#' invoking external binaries.
#'
#' @param record Character scalar giving the record name (without extension).
#' @param record_dir Directory that contains the WFDB files.
#' @param annotator Optional annotator to read or write.
#' @param begin,end,interval Numeric values expressed in seconds that describe
#'   the portion of the record that should be read. Only one of `end` or
#'   `interval` should be supplied.
#' @param units Either "digital" (default) or "physical" indicating whether to
#'   return raw integer samples or scaled values.
#' @param channels Optional character or numeric vector specifying the channels
#'   to retain.
#' @param header A [header_table] describing the channels. Required for writing
#'   signal data unless `data` is an [egm] object, and used when writing
#'   annotations to match channel names to their indices.
#' @param data When writing, either an [egm], a [signal_table], or a data frame
#'   that can be coerced with [signal_table()].
#' @param storage_format Optional integer WFDB storage format (one of 16, 32,
#'   or 212) to use when writing. Defaults to the format declared in `header`.
#' @param overwrite Logical flag indicating whether existing WFDB files may be
#'   replaced.
#' @param info_strings Named list of info strings to append to the header when
#'   writing.
#' @param ... Reserved for future extensions.
#'
#' @return
#' * `read_signal_native()` returns a [signal_table] that mirrors the structure
#'   produced by [read_signal()].
#' * `read_wfdb_native()` returns an [egm] object composed of the native signal
#'   data, the parsed header, and any decoded annotations.
#' * `write_wfdb_native()` invisibly returns a named list containing the written
#'   `signal_path`, `header_path`, and the normalised `header`.
#' * `read_annotation_native()` returns an [annotation_table] mirroring the
#'   structure produced by [read_annotation()].
#' * `write_annotation_native()` invisibly returns the path to the WFDB
#'   annotation file that was written.
#'
#' @seealso [read_signal()], [read_wfdb()]
#'
#' @name wfdb_native
#' @export

#' @rdname wfdb_native
#' @export
read_signal_native <- function(
  record,
  record_dir = ".",
  begin = 0,
  end = NA_real_,
  interval = NA_real_,
  units = c("digital", "physical"),
  channels = character(),
  ...
) {
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
    stop(
      "Header is missing the sampling frequency; native reading is not available",
      call. = FALSE
    )
  }

  if (is.null(samples) || is.na(samples)) {
    stop(
      "Header is missing the total number of samples; native reading is not available",
      call. = FALSE
    )
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
    stop(
      "Native reader currently supports records stored in a single signal file",
      call. = FALSE
    )
  }

  file_name <- file_names[[1]]
  if (is.na(file_name) || !nzchar(file_name)) {
    stop("Header does not declare a signal file name", call. = FALSE)
  }

  storage_format <- unique(header$storage_format)
  if (length(storage_format) != 1L) {
    stop(
      "Native reader requires a single storage format across channels",
      call. = FALSE
    )
  }
  storage_format <- native_validate_storage_format(storage_format[[1]])

  signal_path <- fs::path(record_dir, file_name)
  if (!fs::file_exists(signal_path)) {
    stop("Signal file not found at ", signal_path, call. = FALSE)
  }
  n_channels <- nrow(header)

  raw_matrix <- read_wfdb_dat_cpp(
    path = signal_path,
    n_channels = n_channels,
    start_sample = start_sample,
    n_samples = n_samples,
    storage_format = storage_format
  )

  channel_names <- native_canonicalize_labels(header$label)
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

  dat <- data.table::as.data.table(raw_matrix)
  if (ncol(dat) > 0) {
    data.table::setnames(dat, channel_names)
  }
  dat[, sample := seq.int(from = start_sample, length.out = n_samples)]
  data.table::setcolorder(dat, c("sample", setdiff(names(dat), "sample")))

  if (identical(units, "digital") && length(channel_names) > 0) {
    for (col in channel_names) {
      data.table::set(dat, j = col, value = as.integer(dat[[col]]))
    }
  }

  if (identical(units, "physical") && length(channel_names) > 0) {
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

#' @rdname wfdb_native
#' @export
read_wfdb_native <- function(
  record,
  record_dir = ".",
  annotator = NA_character_,
  begin = 0,
  end = NA_real_,
  interval = NA_real_,
  units = c("digital", "physical"),
  channels = character(),
  ...
) {
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

  # Align signal columns to header labels
  sig <- native_align_signal_to_header(sig, hea)

  annotation <- annotation_table()
  if (!is.na(annotator) && length(annotator) == 1 && nzchar(annotator)) {
    annotation <- read_annotation_native(
      record = record,
      record_dir = record_dir,
      annotator = annotator,
      begin = begin,
      end = end,
      interval = interval,
      ...
    )
  }

  egm(
    signal = sig,
    header = hea,
    annotation = annotation
  )
}

#' @rdname wfdb_native
#' @export
write_wfdb_native <- function(
  data,
  record,
  record_dir = ".",
  units = c("digital", "physical"),
  header = NULL,
  storage_format = NULL,
  info_strings = list(),
  overwrite = TRUE,
  ...
) {
  units <- match.arg(units)
  if (missing(record) || !nzchar(record)) {
    stop("`record` must be provided", call. = FALSE)
  }

  if (inherits(data, "egm")) {
    signal <- data$signal
    header_input <- data$header
  } else if (is_signal_table(data)) {
    signal <- data
    header_input <- header
  } else if (inherits(data, "data.frame")) {
    signal <- signal_table(data)
    header_input <- header
  } else {
    stop(
      "`data` must be an `egm`, `signal_table`, or data frame",
      call. = FALSE
    )
  }

  if (is.null(header_input)) {
    stop(
      "`header` must be supplied when `data` is not an `egm` object",
      call. = FALSE
    )
  }

  if (!is_signal_table(signal)) {
    signal <- signal_table(signal)
  }

  if (!is_header_table(header_input)) {
    sig_dt <- data.table::as.data.table(signal)
    channel_cols <- setdiff(names(sig_dt), "sample")
    if (length(channel_cols) == 0L) {
      stop("Signal table does not contain any channels", call. = FALSE)
    }
    n_channels <- length(channel_cols)

    freq <- native_coalesce(header_input$frequency, 250)
    gain <- native_coalesce(header_input$gain, header_input$ADC_gain)
    gain <- native_coalesce(gain, 200L)
    baseline <- native_coalesce(
      header_input$baseline,
      header_input$ADC_baseline
    )
    baseline <- native_coalesce(baseline, 0L)
    units_col <- native_coalesce(header_input$units, header_input$ADC_units)
    units_col <- native_coalesce(units_col, "mV")
    labels <- native_coalesce(header_input$label, channel_cols)
    storage_vec <- native_coalesce(header_input$storage_format, storage_format)
    storage_vec <- native_coalesce(storage_vec, 16L)

    header_input <- header_table(
      record_name = record,
      number_of_channels = n_channels,
      frequency = freq,
      samples = nrow(sig_dt),
      storage_format = rep_len(storage_vec, n_channels),
      ADC_gain = rep_len(gain, n_channels),
      ADC_baseline = rep_len(baseline, n_channels),
      ADC_units = rep_len(units_col, n_channels),
      label = rep_len(labels, n_channels),
      info_strings = info_strings
    )
  }

  sig_dt <- data.table::as.data.table(signal)
  channel_cols <- setdiff(names(sig_dt), "sample")
  if (length(channel_cols) == 0L) {
    stop("Signal table does not contain any channels", call. = FALSE)
  }

  n_samples <- nrow(sig_dt)
  n_channels <- length(channel_cols)

  header_copy <- data.table::copy(header_input)
  storage_format <- native_resolve_storage_format(storage_format, header_copy)
  header_copy$storage_format <- rep(storage_format, n_channels)

  if (nrow(header_copy) != n_channels) {
    stop("Header must contain one row per channel", call. = FALSE)
  }

  header_copy$label <- native_canonicalize_labels(header_copy$label)
  header_labels <- as.character(header_copy$label)
  if (
    length(header_labels) != n_channels ||
      any(is.na(header_labels)) ||
      any(!nzchar(header_labels))
  ) {
    header_labels <- channel_cols
    header_copy$label <- header_labels
  }

  alignment <- native_align_signal_columns(channel_cols, header_labels)
  channel_cols <- alignment$channel_cols
  header_labels <- alignment$header_labels
  header_copy$label <- header_labels

  channel_mat <- as.matrix(sig_dt[, ..channel_cols])
  colnames(channel_mat) <- header_labels

  if (!is.numeric(channel_mat)) {
    stop("Signal data must be numeric", call. = FALSE)
  }

  if (identical(units, "physical")) {
    gain <- as.numeric(header_copy$ADC_gain)
    baseline <- as.numeric(header_copy$ADC_baseline)
    zero <- as.numeric(header_copy$ADC_zero)

    baseline[is.na(baseline)] <- zero[is.na(baseline)]
    if (any(is.na(gain) | gain == 0)) {
      stop(
        "Gain values are required to convert physical units back to digital samples",
        call. = FALSE
      )
    }

    for (j in seq_len(n_channels)) {
      channel_mat[, j] <- round(channel_mat[, j] * gain[[j]] + baseline[[j]])
    }
  }

  if (any(!is.finite(channel_mat))) {
    stop("Signal data contains non-finite values", call. = FALSE)
  }

  storage_mat <- round(channel_mat)
  storage_mat <- matrix(
    as.integer(storage_mat),
    nrow = n_samples,
    ncol = n_channels
  )

  if (anyNA(storage_mat)) {
    stop("Signal data contains missing values", call. = FALSE)
  }

  if (storage_format == 16L) {
    if (any(storage_mat < -32768L) || any(storage_mat > 32767L)) {
      stop("Signal values exceed the 16-bit integer range", call. = FALSE)
    }
  } else if (storage_format == 212L) {
    if (any(storage_mat < -2048L) || any(storage_mat > 2047L)) {
      stop(
        "Signal values exceed the 12-bit range used by format 212",
        call. = FALSE
      )
    }
  }

  record_line <- attributes(header_copy)$record_line
  if (is.null(record_line)) {
    record_line <- list()
  }
  record_line$record_name <- record
  record_line$number_of_channels <- n_channels
  record_line$samples <- n_samples
  if (is.null(record_line$frequency) || is.na(record_line$frequency)) {
    stop(
      "Header record line must include the sampling frequency",
      call. = FALSE
    )
  }
  attributes(header_copy)$record_line <- record_line

  file_name <- unique(as.character(header_copy$file_name))
  if (
    length(file_name) == 0L || is.na(file_name[[1]]) || !nzchar(file_name[[1]])
  ) {
    file_name <- paste0(record, ".dat")
  } else if (length(file_name) != 1L) {
    stop("Native writer requires a single signal file name", call. = FALSE)
  } else {
    file_name <- file_name[[1]]
  }
  header_copy$file_name <- rep(file_name, n_channels)

  header_copy$ADC_units <- ifelse(
    is.na(header_copy$ADC_units) | !nzchar(as.character(header_copy$ADC_units)),
    "mV",
    as.character(header_copy$ADC_units)
  )
  header_copy$ADC_gain <- as.numeric(header_copy$ADC_gain)
  header_copy$ADC_gain[is.na(header_copy$ADC_gain)] <- 200

  baseline <- as.numeric(header_copy$ADC_baseline)
  zero <- as.numeric(header_copy$ADC_zero)
  baseline[is.na(baseline)] <- zero[is.na(baseline)]
  baseline[is.na(baseline)] <- 0
  zero[is.na(zero)] <- 0
  header_copy$ADC_baseline <- as.integer(round(baseline))
  header_copy$ADC_zero <- as.integer(round(zero))

  header_copy$ADC_resolution <- as.integer(round(ifelse(
    is.na(header_copy$ADC_resolution),
    storage_format,
    header_copy$ADC_resolution
  )))
  header_copy$initial_value <- as.integer(round(ifelse(
    is.na(header_copy$initial_value),
    header_copy$ADC_baseline,
    header_copy$initial_value
  )))
  header_copy$checksum <- as.integer(round(ifelse(
    is.na(header_copy$checksum),
    0L,
    header_copy$checksum
  )))
  header_copy$blocksize <- as.integer(round(ifelse(
    is.na(header_copy$blocksize),
    0L,
    header_copy$blocksize
  )))
  header_copy$label <- as.character(header_copy$label)

  existing_info <- attributes(header_copy)$info_strings
  if (is.null(existing_info)) {
    existing_info <- list()
  }
  if (length(info_strings) > 0) {
    existing_info <- utils::modifyList(existing_info, info_strings)
  }
  attributes(header_copy)$info_strings <- existing_info

  if (!fs::dir_exists(record_dir)) {
    fs::dir_create(record_dir, recurse = TRUE)
  }
  signal_path <- fs::path(record_dir, file_name)
  header_path <- fs::path(record_dir, paste0(record, ".hea"))

  if (
    !overwrite && (fs::file_exists(signal_path) || fs::file_exists(header_path))
  ) {
    stop(
      "WFDB files already exist for this record; set `overwrite = TRUE` to replace them",
      call. = FALSE
    )
  }

  values <- as.integer(c(t(storage_mat)))
  write_wfdb_dat_cpp(
    path = signal_path,
    samples = values,
    n_channels = as.integer(n_channels),
    storage_format = storage_format
  )

  header_lines <- native_header_lines(header_copy)
  writeLines(header_lines, header_path)

  if (inherits(data, "egm")) {
    annotation_tbl <- data$annotation
    if (
      is_annotation_table(annotation_tbl) &&
        nrow(annotation_tbl) > 0 &&
        length(attr(annotation_tbl, "annotator")) > 0 &&
        nzchar(attr(annotation_tbl, "annotator"))
    ) {
      annotator_name <- attr(annotation_tbl, "annotator")
      write_annotation_native(
        data = annotation_tbl,
        record = record,
        annotator = annotator_name,
        record_dir = record_dir,
        overwrite = TRUE,
        header = header_copy
      )
      message("Annotation file updated to reflect header channel names.")
    }
  }

  invisible(list(
    signal_path = signal_path,
    header_path = header_path,
    header = header_copy
  ))
}

#' @rdname wfdb_native
#' @export
read_annotation_native <- function(
  record,
  annotator,
  record_dir = ".",
  begin = 0,
  end = NA_real_,
  interval = NA_real_,
  ...
) {
  if (missing(annotator) || !nzchar(annotator)) {
    stop("`annotator` must be supplied", call. = FALSE)
  }
  if (begin < 0) {
    stop("`begin` must be non-negative", call. = FALSE)
  }
  if (!is.na(end) && !is.na(interval)) {
    stop("Specify only one of `end` or `interval`", call. = FALSE)
  }

  # If the annotation is inadequate, need to check here before going further
  # Example would be a file that is very small
  annPath <- fs::path(record_dir, record, ext = annotator)
  fileSize <- fs::file_size(annPath) # returns in bytes

  if (fileSize < 8) {
    # Unlikely to be readable?
    # If its only a few bytes then annotation is unlikely
    message(
      'The annotation for ',
      record,
      ' is unlikely to be legible. ',
      'An empty annotation table was returned instead.'
    )
    return(annotation_table())
  }

  header <- read_header(record, record_dir = record_dir)
  record_line <- attributes(header)$record_line

  frequency <- record_line$frequency
  if (is.null(frequency) || is.na(frequency)) {
    stop(
      "Header is missing the sampling frequency; cannot decode annotations",
      call. = FALSE
    )
  }

  samples_total <- record_line$samples

  start_sample <- as.integer(round(begin * frequency))
  if (!is.na(interval)) {
    stopifnot("`interval` must be non-negative" = interval >= 0)
    end_sample <- start_sample + as.integer(round(interval * frequency))
  } else if (!is.na(end)) {
    stopifnot("`end` must be greater than or equal to `begin`" = end >= begin)
    end_sample <- as.integer(round(end * frequency))
  } else if (!is.null(samples_total) && !is.na(samples_total)) {
    end_sample <- as.integer(samples_total)
  } else {
    end_sample <- NA_integer_
  }

  if (!is.na(end_sample) && end_sample < start_sample) {
    stop("Requested sample range is empty", call. = FALSE)
  }

  annotation_path <- fs::path(record_dir, record, ext = annotator)
  if (!fs::file_exists(annotation_path)) {
    stop("Annotation file not found at ", annotation_path, call. = FALSE)
  }

  raw <- read_wfdb_ann_cpp(annotation_path)
  samples <- as.integer(raw$sample)
  type_codes <- as.integer(raw$type)
  subtype <- as.integer(raw$subtype)
  channel <- as.integer(raw$channel)
  number <- as.integer(raw$number)
  aux <- as.character(raw$aux)

  include <- samples >= start_sample
  if (!is.na(end_sample)) {
    include <- include & samples <= end_sample
  }

  if (!any(include)) {
    return(new_annotation_table(annotator = annotator))
  }

  samples <- samples[include]
  type_codes <- type_codes[include]
  subtype <- subtype[include]
  channel <- channel[include]
  number <- number[include]
  aux <- aux[include]

  times <- native_format_annotation_time(samples, frequency)
  types <- native_annotation_code_to_mnemonic(type_codes)

  header_labels <- native_canonicalize_labels(header$label)
  channel_names <- native_annotation_channels_to_labels(channel, header_labels)

  dat <- df_list(
    time = times,
    sample = samples,
    type = types,
    subtype = subtype,
    channel = channel_names,
    number = number
  )

  result <- new_annotation_table(dat, annotator)
  if (length(aux) && any(nzchar(aux))) {
    attr(result, "aux") <- aux
  }
  result
}

#' @rdname wfdb_native
#' @export
write_annotation_native <- function(
  data,
  record,
  annotator,
  record_dir = ".",
  overwrite = FALSE,
  header = NULL,
  ...
) {
  if (missing(annotator) || !nzchar(annotator)) {
    stop("`annotator` must be supplied", call. = FALSE)
  }
  if (missing(record) || !nzchar(record)) {
    stop("`record` must be supplied", call. = FALSE)
  }

  if (inherits(data, "annotation_table")) {
    ann <- data.table::as.data.table(data)
  } else if (inherits(data, "data.frame")) {
    ann <- data.table::as.data.table(data)
  } else {
    stop("`data` must be an annotation_table or data.frame", call. = FALSE)
  }

  required <- c("sample", "type", "subtype", "channel", "number")
  if (!all(required %in% names(ann))) {
    stop(
      "Annotation data must include columns: ",
      paste(required, collapse = ", "),
      call. = FALSE
    )
  }

  sample_values <- ann[["sample"]]
  if (!is.numeric(sample_values)) {
    stop("`sample` column must be numeric", call. = FALSE)
  }
  if (anyNA(sample_values)) {
    stop("`sample` column contains missing values", call. = FALSE)
  }
  if (any(abs(sample_values - round(sample_values)) > 1e-6)) {
    stop("`sample` column must contain integer values", call. = FALSE)
  }
  samples <- as.integer(round(sample_values))

  type_values <- as.character(ann[["type"]])
  type_codes <- native_annotation_mnemonic_to_code(type_values)
  if (any(is.na(type_codes))) {
    stop(
      "Encountered unknown annotation types that cannot be encoded",
      call. = FALSE
    )
  }

  subtype_values <- ann[["subtype"]]
  channel_values <- ann[["channel"]]
  number_values <- ann[["number"]]

  subtype_values <- native_annotation_normalise_integer(subtype_values)

  if (is.null(header)) {
    header_path <- fs::path(record_dir, record, ext = "hea")
    if (fs::file_exists(header_path)) {
      header <- tryCatch(
        read_header(record, record_dir = record_dir),
        error = function(e) NULL
      )
    }
  }

  header_labels <- character()
  if (!is.null(header) && "label" %in% names(header)) {
    header_labels <- native_canonicalize_labels(header$label)
  }

  if (is.character(channel_values) || is.factor(channel_values)) {
    if (!length(header_labels)) {
      stop(
        "Channel names provided but header labels are unavailable for matching",
        call. = FALSE
      )
    }
    channel_chr <- as.character(channel_values)
    idx <- native_annotation_labels_to_indices(channel_chr, header_labels)
    unresolved <- is.na(idx) & !is.na(channel_chr) & nzchar(channel_chr)
    if (any(unresolved)) {
      stop(
        "Annotation channels could not be matched to header labels",
        call. = FALSE
      )
    }
    idx[is.na(idx)] <- 0L
    channel_values <- as.integer(idx)
  }

  channel_values <- native_annotation_normalise_integer(channel_values)
  number_values <- native_annotation_normalise_integer(number_values)

  aux_values <- if ("aux" %in% names(ann)) {
    as.character(ann[["aux"]])
  } else {
    rep("", length(samples))
  }
  aux_values[is.na(aux_values)] <- ""

  if (!fs::dir_exists(record_dir)) {
    fs::dir_create(record_dir, recurse = TRUE)
  }

  annotation_path <- fs::path(record_dir, record, ext = annotator)
  if (fs::file_exists(annotation_path) && !overwrite) {
    stop(
      "Annotation file already exists; set `overwrite = TRUE` to replace it",
      call. = FALSE
    )
  }

  write_wfdb_ann_cpp(
    path = annotation_path,
    samples = samples,
    types = type_codes,
    subtypes = subtype_values,
    channels = channel_values,
    numbers = number_values,
    aux = aux_values
  )

  invisible(annotation_path)
}

# Helper functions ----
native_supported_storage_formats <- function() {
  c(16L, 32L, 212L)
}

native_validate_storage_format <- function(storage_format) {
  fmt <- as.integer(storage_format)[1]
  if (is.na(fmt)) {
    stop("Storage format must be specified", call. = FALSE)
  }
  if (!fmt %in% native_supported_storage_formats()) {
    stop(
      "Native backend supports storage formats 16, 32, and 212",
      call. = FALSE
    )
  }
  fmt
}

native_resolve_storage_format <- function(storage_format, header) {
  if (!is.null(storage_format)) {
    return(native_validate_storage_format(storage_format))
  }

  formats <- unique(as.integer(header$storage_format))
  formats <- formats[!is.na(formats)]
  if (length(formats) == 0L) {
    return(16L)
  }
  if (length(formats) != 1L) {
    stop(
      "Native backend requires a single storage format across channels",
      call. = FALSE
    )
  }
  native_validate_storage_format(formats[[1]])
}

native_format_number <- function(x) {
  if (length(x) == 0 || is.na(x)) {
    return("0")
  }
  value <- suppressWarnings(as.numeric(x))
  if (is.na(value)) {
    return(as.character(x))
  }
  if (isTRUE(all.equal(value, round(value)))) {
    sprintf("%d", as.integer(round(value)))
  } else {
    formatC(value, digits = 10, format = "fg", drop0trailing = TRUE)
  }
}

native_normalise_channel_name <- function(x) {
  if (!length(x)) {
    return(character())
  }
  x <- toupper(trimws(as.character(x)))
  gsub("[^A-Z0-9]", "", x, perl = TRUE)
}

native_canonicalize_labels <- function(labels) {
  if (!length(labels)) {
    return(as.character(labels))
  }

  labels_chr <- as.character(labels)
  if (!exists(".labels", inherits = TRUE)) {
    return(labels_chr)
  }

  canonical_labels <- as.character(.labels)
  canonical_norm <- native_normalise_channel_name(canonical_labels)
  label_norm <- native_normalise_channel_name(labels_chr)

  matched_idx <- match(label_norm, canonical_norm)
  result <- labels_chr

  matched <- !is.na(matched_idx)
  result[matched] <- canonical_labels[matched_idx[matched]]

  remaining <- which(!matched & !is.na(label_norm) & nzchar(label_norm))
  if (length(remaining)) {
    for (i in remaining) {
      distances <- utils::adist(label_norm[[i]], canonical_norm)
      if (!length(distances) || !is.finite(distances[[1]])) {
        next
      }
      min_dist <- min(distances)
      if (!is.finite(min_dist) || min_dist > 2) {
        next
      }
      candidates <- which(distances == min_dist)
      if (length(candidates) == 1L) {
        result[[i]] <- canonical_labels[[candidates]]
      }
    }
  }

  result
}

native_annotation_channels_to_labels <- function(
  channel_indices,
  header_labels
) {
  if (!length(channel_indices) || !length(header_labels)) {
    return(channel_indices)
  }

  header_labels <- native_canonicalize_labels(header_labels)
  idx <- suppressWarnings(as.integer(channel_indices))
  result <- as.character(channel_indices)

  valid <- !is.na(idx) & idx >= 1 & idx <= length(header_labels)
  if (any(valid)) {
    result[valid] <- header_labels[idx[valid]]
  }

  result
}

native_annotation_labels_to_indices <- function(channel_labels, header_labels) {
  if (!length(channel_labels)) {
    return(integer())
  }

  header_labels <- native_canonicalize_labels(header_labels)
  header_norm <- native_normalise_channel_name(header_labels)

  if (!length(header_norm)) {
    return(rep(NA_integer_, length(channel_labels)))
  }

  labels_chr <- native_canonicalize_labels(channel_labels)
  label_norm <- native_normalise_channel_name(labels_chr)

  idx <- match(label_norm, header_norm)
  unresolved <- which(is.na(idx) & !is.na(label_norm) & nzchar(label_norm))
  if (length(unresolved)) {
    for (i in unresolved) {
      distances <- utils::adist(label_norm[[i]], header_norm)
      if (!length(distances) || !is.finite(distances[[1]])) {
        next
      }
      min_dist <- min(distances)
      if (!is.finite(min_dist) || min_dist > 2) {
        next
      }
      candidates <- which(distances == min_dist)
      if (length(candidates) == 1L) {
        idx[[i]] <- candidates
      }
    }
  }

  idx
}

native_align_signal_columns <- function(signal_cols, header_labels) {
  signal_cols <- as.character(signal_cols)
  header_labels <- as.character(header_labels)

  if (!length(signal_cols)) {
    return(list(channel_cols = signal_cols, header_labels = header_labels))
  }

  if (!length(header_labels)) {
    return(list(channel_cols = signal_cols, header_labels = signal_cols))
  }

  sig_norm <- native_normalise_channel_name(signal_cols)
  hdr_norm <- native_normalise_channel_name(header_labels)

  order <- rep(NA_integer_, length(hdr_norm))
  used <- rep(FALSE, length(sig_norm))

  for (i in seq_along(hdr_norm)) {
    candidates <- which(sig_norm == hdr_norm[[i]] & !used)
    if (length(candidates)) {
      order[[i]] <- candidates[[1]]
      used[[candidates[[1]]]] <- TRUE
    }
  }

  if (any(is.na(order))) {
    missing <- header_labels[is.na(order)]
    stop(
      "Header channel labels do not match the signal table columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  list(
    channel_cols = signal_cols[order],
    header_labels = header_labels
  )
}

native_collect_info_strings <- function(header) {
  info <- attributes(header)$info_strings
  if (is.null(info)) {
    info <- list()
  }

  if (length(info) && is.null(names(info))) {
    names(info) <- rep("", length(info))
  }

  fields <- c(
    "source",
    "lead",
    "additional_gain",
    "low_pass",
    "high_pass",
    "color",
    "scale"
  )

  for (field in fields) {
    if (!field %in% names(info) && field %in% names(header)) {
      values <- header[[field]]
      if (!all(is.na(values))) {
        info[[field]] <- values
      }
    }
  }

  info
}

native_coalesce <- function(x, fallback) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    fallback
  } else {
    x
  }
}

native_header_info_lines <- function(header) {
  info <- native_collect_info_strings(header)
  if (!length(info)) {
    return(character())
  }

  if (is.null(names(info))) {
    names(info) <- paste0("info_", seq_along(info))
  }

  missing_names <- names(info) == "" | is.na(names(info))
  if (any(missing_names)) {
    names(info)[missing_names] <- paste0(
      "info_",
      seq_along(info)
    )[missing_names]
  }

  vapply(
    seq_along(info),
    function(i) {
      name <- names(info)[[i]]
      values <- info[[i]]
      paste("#", name, paste(as.character(values), collapse = " "))
    },
    character(1),
    USE.NAMES = FALSE
  )
}

native_header_lines <- function(header) {
  rec <- attributes(header)$record_line
  if (is.null(rec)) {
    stop("Header is missing record metadata", call. = FALSE)
  }

  record_name <- rec$record_name
  if (is.null(record_name) || is.na(record_name) || !nzchar(record_name)) {
    stop("Header record name must be specified", call. = FALSE)
  }

  n_channels <- rec$number_of_channels
  if (is.null(n_channels) || is.na(n_channels)) {
    n_channels <- nrow(header)
  }

  frequency <- rec$frequency
  if (is.null(frequency) || is.na(frequency)) {
    stop("Header is missing the sampling frequency", call. = FALSE)
  }

  samples <- rec$samples
  if (is.null(samples) || is.na(samples)) {
    stop("Header is missing the total number of samples", call. = FALSE)
  }

  start_time <- rec$start_time
  components <- c(
    record_name,
    native_format_number(n_channels),
    native_format_number(frequency),
    native_format_number(samples)
  )

  if (!is.null(start_time) && !is.na(start_time)) {
    components <- c(
      components,
      format(start_time, "%H:%M:%OS"),
      format(start_time, "%d/%m/%Y")
    )
  }

  lines <- paste(components, collapse = " ")

  if (nrow(header) > 0) {
    signal_lines <- vapply(
      seq_len(nrow(header)),
      function(i) {
        units <- header$ADC_units[[i]]
        if (is.null(units) || is.na(units) || !nzchar(units)) {
          units <- "mV"
        }
        adc <- sprintf(
          "%s(%s)/%s",
          native_format_number(header$ADC_gain[[i]]),
          native_format_number(header$ADC_baseline[[i]]),
          units
        )
        paste(
          header$file_name[[i]],
          native_format_number(header$storage_format[[i]]),
          adc,
          native_format_number(header$ADC_resolution[[i]]),
          native_format_number(header$ADC_zero[[i]]),
          native_format_number(header$initial_value[[i]]),
          native_format_number(header$checksum[[i]]),
          native_format_number(header$blocksize[[i]]),
          as.character(header$label[[i]]),
          sep = "\t"
        )
      },
      character(1),
      USE.NAMES = FALSE
    )
    lines <- c(lines, signal_lines)
  }

  info_lines <- native_header_info_lines(header)
  if (length(info_lines) > 0) {
    lines <- c(lines, info_lines)
  }

  lines
}

native_annotation_mnemonics <- function() {
  c(
    " ",
    "N",
    "L",
    "R",
    "a",
    "V",
    "F",
    "J",
    "A",
    "S",
    "E",
    "j",
    "/",
    "Q",
    "~",
    "[15]",
    "|",
    "[17]",
    "s",
    "T",
    "*",
    "D",
    "\"",
    "=",
    "p",
    "B",
    "^",
    "t",
    "+",
    "u",
    "?",
    "!",
    "[",
    "]",
    "e",
    "n",
    "@",
    "x",
    "f",
    "(",
    ")",
    "r",
    "[42]",
    "[43]",
    "[44]",
    "[45]",
    "[46]",
    "[47]",
    "[48]",
    "[49]"
  )
}

native_annotation_code_to_mnemonic <- function(code) {
  mnemonics <- native_annotation_mnemonics()
  code <- as.integer(code)
  result <- rep(NA_character_, length(code))
  valid <- !is.na(code) & code >= 0 & code < length(mnemonics)
  if (any(valid)) {
    result[valid] <- mnemonics[code[valid] + 1L]
  }
  invalid <- !is.na(code) & !valid
  if (any(invalid)) {
    result[invalid] <- sprintf("[%d]", code[invalid])
  }
  result
}

native_annotation_mnemonic_to_code <- function(mnemonic) {
  mnemonics <- native_annotation_mnemonics()
  lookup <- stats::setNames(seq_along(mnemonics) - 1L, mnemonics)
  mnemonic <- as.character(mnemonic)
  result <- rep(NA_integer_, length(mnemonic))
  valid <- !is.na(mnemonic)
  if (any(valid)) {
    values <- mnemonic[valid]
    codes <- lookup[values]
    missing <- is.na(codes)
    if (any(missing)) {
      bracket_mask <- grepl("^\\[(\\d+)\\]$", values[missing])
      if (any(bracket_mask)) {
        missing_idx <- which(missing)
        codes[missing_idx[bracket_mask]] <- as.integer(sub(
          "^\\[(\\d+)\\]$",
          "\\1",
          values[missing][bracket_mask]
        ))
      }
    }
    result[valid] <- as.integer(codes)
  }
  result
}

native_format_annotation_time <- function(samples, frequency) {
  stopifnot(length(frequency) == 1L, !is.na(frequency), frequency > 0)
  samples <- as.numeric(samples)
  total_seconds <- samples / frequency
  hours <- floor(total_seconds / 3600)
  remaining <- total_seconds - hours * 3600
  minutes <- floor(remaining / 60)
  seconds <- remaining - minutes * 60
  seconds <- round(seconds, 3)

  overflow_sec <- seconds >= 60
  if (any(overflow_sec, na.rm = TRUE)) {
    seconds[overflow_sec] <- seconds[overflow_sec] - 60
    minutes[overflow_sec] <- minutes[overflow_sec] + 1
  }

  overflow_min <- minutes >= 60
  if (any(overflow_min, na.rm = TRUE)) {
    minutes[overflow_min] <- minutes[overflow_min] - 60
    hours[overflow_min] <- hours[overflow_min] + 1
  }

  sprintf("%02d:%02d:%06.3f", hours, minutes, seconds)
}

native_annotation_normalise_integer <- function(x) {
  if (length(x) == 0L) {
    return(integer())
  }
  if (!is.numeric(x)) {
    stop("Annotation columns must be numeric", call. = FALSE)
  }
  x[is.na(x)] <- 0
  values <- as.integer(round(x))
  values[!is.finite(values)] <- 0L
  values
}

# Normalize label for matching: uppercase and replace spaces/hyphens with underscores
native_normalize_label <- function(x) {
  toupper(gsub("[ -]", "_", x))
}

# Match and reorder signal columns to header labels
native_align_signal_to_header <- function(signal, header) {
  sig_dt <- data.table::as.data.table(signal)
  channel_cols <- setdiff(names(sig_dt), "sample")
  header_labels <- native_canonicalize_labels(header$label)

  if (length(channel_cols) == 0 || length(header_labels) == 0) {
    return(signal)
  }

  normalized_header <- native_normalize_label(header_labels)
  normalized_cols <- native_normalize_label(channel_cols)

  match_idx <- match(normalized_header, normalized_cols)
  if (anyNA(match_idx)) {
    return(signal)
  }

  # Reorder and rename columns to match header
  ordered_cols <- channel_cols[match_idx]
  sig_dt <- sig_dt[, c("sample", ordered_cols), with = FALSE]
  data.table::setnames(sig_dt, ordered_cols, header_labels)

  signal_table(sig_dt)
}
