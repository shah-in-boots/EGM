# WFDB native I/O --------------------------------------------------------------

#' Native WFDB readers and writers
#'
#' @description
#' These functions provide native replacements for the WFDB `rdsamp` and
#' `wrsamp` utilities so that WFDB header and signal files can be read and
#' written without requiring the external WFDB software. The helpers
#' [read_header_native()] and [read_signal_native()] return the individual
#' components used by [read_wfdb_native()], while [write_wfdb_native()] mirrors
#' the behaviour of `wrsamp` using R and C++ implementations within this
#' package.
#'
#' @inheritParams read_wfdb
#' @param header Optional header information that has already been parsed by
#'   [read_header_native()]. If `NULL`, the header file is read automatically.
#' @param data Signal data to be written. This can be an `egm` object, a
#'   `signal_table`, or any data frame coercible to those classes. When an
#'   `egm` object is supplied, header information is retrieved from the object.
#' @param info_strings Optional named list of character vectors to append to the
#'   info string section of the output header file when writing.
#'
#' @return
#' * `read_wfdb_native()` returns an [egm()] object containing the signal,
#'   header, and an empty annotation table.
#' * `read_header_native()` returns a [header_table()] object.
#' * `read_signal_native()` returns a [signal_table()] object.
#' * `write_wfdb_native()` invisibly returns the path to the written record.
#'
#' @export
read_wfdb_native <- function(record,
                             record_dir = ".",
                             begin = 0,
                             end = NA_integer_,
                             interval = NA_integer_,
                             units = c("digital", "physical"),
                             channels = character(),
                             ...) {

        header <- read_header_native(record = record, record_dir = record_dir)
        signal <- read_signal_native(
                record = record,
                record_dir = record_dir,
                header = header,
                begin = begin,
                end = end,
                interval = interval,
                units = units,
                channels = channels
        )

        egm(signal = signal, header = header, annotation = annotation_table())
}

#' @rdname read_wfdb_native
#' @export
read_header_native <- function(record, record_dir = ".", ...) {

        header_path <- fs::path(record_dir, record, ext = "hea")
        if (!fs::file_exists(header_path)) {
                stop(record, " not found in ", record_dir)
        }

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
        if (length(record_name) == 0 || is.na(record_name) || record_name == "") {
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

#' @rdname read_wfdb_native
#' @export
read_signal_native <- function(record,
                               record_dir = ".",
                               header = NULL,
                               begin = 0,
                               end = NA_integer_,
                               interval = NA_integer_,
                               units = c("digital", "physical"),
                               channels = character(),
                               ...) {

        units <- match.arg(units)

        if (is.null(header)) {
                header <- read_header_native(record = record, record_dir = record_dir)
        }
        record_line <- attr(header, "record_line")
        frequency <- record_line$frequency
        total_samples <- record_line$samples
        number_of_channels <- record_line$number_of_channels

        begin_sample <- as.integer(round(begin * frequency))
        if (is.na(begin_sample) || begin_sample < 0) {
                begin_sample <- 0L
        }

        if (!is.na(interval)) {
                end_sample <- begin_sample + as.integer(round(interval * frequency))
        } else if (!is.na(end)) {
                end_sample <- as.integer(round(end * frequency))
        } else {
                end_sample <- total_samples
        }
        if (is.na(end_sample) || end_sample <= 0) {
                end_sample <- total_samples
        }

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
                        selection[missing] <- match(toupper(channels[missing]), toupper(labels))
                }
        }

        if (any(is.na(selection))) {
                stop("Requested channels could not be matched to the header definition")
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
                stop("Multiple signal files per record are not currently supported")
        }

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

#' @rdname read_wfdb_native
#' @export
write_wfdb_native <- function(data,
                              record,
                              record_dir = ".",
                              header = NULL,
                              info_strings = list(),
                              ...) {

        if (!fs::dir_exists(record_dir)) {
                fs::dir_create(record_dir, recurse = TRUE)
        }

        if (inherits(data, "egm")) {
                signal <- data$signal
                header <- data$header
        } else {
                signal <- signal_table(data)
        }

        if (is.null(header)) {
                stop("A header_table must be supplied when `data` is not an `egm` object")
        }
        if (!inherits(header, "header_table")) {
                stop("`header` must be a `header_table` object")
        }

        record_line <- attr(header, "record_line")
        frequency <- record_line$frequency
        start_time <- record_line$start_time
        if (!inherits(start_time, "POSIXt") || is.na(start_time)) {
                start_time_str <- ""
        } else {
                start_time_str <- format(start_time, "%H:%M:%OS %d/%m/%Y")
        }

        record_name <- record_line$record_name
        if (length(record_name) == 0 || is.na(record_name) || record_name == "") {
                record_name <- record
        }

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
        label_output[label_output == ""] <- paste0("CH", seq_along(label_output))
        signal_matrix <- as.matrix(as.data.frame(signal_dt[, channel_cols, with = FALSE]))

        file_names <- as.character(header$file_name)
        file_names[file_names == ""] <- paste0(record_name, ".dat")
        unique_files <- unique(file_names)
        if (length(unique_files) > 1) {
                stop("Multiple signal files per record are not currently supported")
        }

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
        combined_info <- c(existing_info, info_strings)
        if (length(combined_info) > 0) {
                if (is.null(names(combined_info)) || any(names(combined_info) == "")) {
                        stop("All info strings must be named")
                }
                combined_info <- lapply(combined_info, as.character)
        }

        data_path <- fs::path(record_dir, unique_files[[1]])
        header_path <- fs::path(record_dir, record, ext = "hea")

        write_wfdb_native_cpp(
                data_path = data_path,
                header_path = header_path,
                signal_matrix = signal_matrix,
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
                info_strings = combined_info
        )

        invisible(header_path)
}
