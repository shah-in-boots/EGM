#' Read Prucka System Files
#'
#' @description
#' `read_prucka()` reads both the signal data (*.txt) and header information
#' (*.inf) exported from the Prucka cardiac electrophysiology system, which is
#' the underlying recording software used in GE Healthcare's CardioLab EP
#' system.
#'
#' @details
#' ## Exporting from GE CardioLab/Prucka
#'
#' To export data from the GE CardioLab system:
#'
#' 1. Open the study/recording in CardioLab
#' 2. Select the time segment you want to export
#' 3. Navigate to **File > Export** or **Tools > Export**
#' 4. Choose **ASCII Export** or **Text Export** format
#' 5. Select the channels to export
#' 6. Choose export location and filename
#' 7. The system will create two files:
#'    - **X####.txt**: Space-delimited signal data
#'    - **X####.inf**: Header file with metadata
#'
#' ## File Format Details
#'
#' **Signal file (*.txt)**:
#' - Space-delimited numeric data
#' - Each row represents one time point
#' - First column: sample index/time marker
#' - Subsequent columns: channel data in mV
#' - All channels sampled at the same rate
#'
#' **Header file (*.inf)**:
#' - Key-value pairs with "=" delimiter
#' - Patient information (name, date, description)
#' - Recording parameters (sampling rate, duration, channel count)
#' - Channel mapping section listing channel numbers and labels
#' - Channel numbers may be non-sequential (e.g., 1-12, 49-50, 75-76)
#'
#' Both files must have the same base name (e.g., X001.txt and X001.inf).
#'
#' ## Notes
#'
#' - Default units are mV for electrical signals and mmHg for pressure
#' - The system typically uses 16-bit ADC resolution
#' - Channel labels may include surface ECG leads (I, II, III, aVR, aVL, aVF,
#' V1-V6) and intracardiac catheters (ABL, His, CS, RV, etc.)
#' - Export may be limited by system memory for very long recordings
#'
#' @param signal_file Path to the *.txt signal data file
#'
#' @param header_file Path to the *.inf header file. If NULL, will look for a
#'   file with the same base name as signal_file but with .inf extension.
#'
#' @param n Number of signal values to return (this will be the same for each
#'   channel of data). Defaults to all values.
#'
#' @return An `egm` class object that is a list of EP signals the format of a
#'   `data.table`, with an attached __header__ attribute that contains
#'   additional recording data.
#'
#' @name prucka
NULL

#' @rdname prucka
#' @export
read_prucka <- function(signal_file, header_file = NULL, n = Inf) {
  # If header_file not provided, construct from signal_file
  if (is.null(header_file)) {
    header_file <- sub("\\.txt$", ".inf", signal_file, ignore.case = TRUE)
    if (!file.exists(header_file)) {
      stop("Cannot find corresponding .inf file: ", header_file)
    }
  }

  # Read signal and header
  sig <- read_prucka_signal(signal_file, n = n)
  hea <- read_prucka_header(header_file)

  # Ensure that the number of channels labeled matches number in signal file
  if ((ncol(sig) - 1) != attributes(hea)$record_line$number_of_channels) {
    warning(
      "Number of channels in signal file (",
      ncol(sig) - 1,
      ") does not match number in header (",
      attributes(hea)$record_line$number_of_channels,
      ")"
    )
  }

  # Assign channel names from header
  names(sig) <- c('sample', as.character(hea$label))

  # Return as egm object
  egm(signal = sig, header = hea)
}


#' @rdname prucka
#' @export
read_prucka_header <- function(header_file) {
  if (!file.exists(header_file)) {
    stop("File not found: ", header_file)
  }

  # Read all lines
  lines <- readLines(header_file)

  # Extract record name from filename
  record_name <- fs::path_ext_remove(fs::path_file(header_file))
  file_name <- paste0(record_name, '.dat')

  # Parse key-value pairs (lines with "=")
  # Creates a list of metadata (split at the `=` sign)
  metadata <- lines[grepl("=", lines)]
  meta_list <- lapply(metadata, function(x) {
    parts <- strsplit(trimws(x), "\\s*=\\s*")[[1]]
    if (length(parts) == 2) {
      list(key = trimws(parts[1]), value = trimws(parts[2]))
    } else {
      NULL
    }
  })
  # Trim away null values
  meta_list <- Filter(Negate(is.null), meta_list)

  # Extract key metadata
  get_meta <- function(key) {
    idx <- which(sapply(meta_list, function(x) x$key == key))
    if (length(idx) > 0) meta_list[[idx[1]]]$value else NA
  }

  number_of_channels <- as.integer(get_meta("Number of Channel"))
  samples <- as.integer(get_meta("Points for Each Channel"))

  # Parse sampling rate (e.g., "977 points/second")
  rate_str <- get_meta("Data Sampling Rate")
  frequency <- as.numeric(sub("\\s*points.*", "", rate_str))

  # Parse date and time
  start_str <- get_meta("Start Time")
  start_time <- tryCatch(
    as.POSIXct(start_str, format = "%m/%d/%Y %I:%M:%S %p"),
    error = function(e) as.POSIXct(NA)
  )

  # ADC saturation (Prucka typically uses 16-bit)
  # This is an estimate, would have to clarify with CardioLab
  ADC_saturation <- 32768

  # Find channel information section
  ch_start <- grep("^Channel Number", lines)
  if (length(ch_start) == 0) {
    stop("Cannot find channel information in .inf file")
  }

  # Parse channel data (starts after "Channel Number  Channel Label" line)
  ch_lines <- lines[(ch_start + 1):length(lines)]
  ch_lines <- ch_lines[nchar(trimws(ch_lines)) > 0] # Remove empty lines

  # Parse channel number and label
  # Creates a paired value list
  ch_data <- lapply(ch_lines, function(line) {
    parts <- strsplit(trimws(line), "\\s+")[[1]]
    if (length(parts) >= 2) {
      list(
        number = as.integer(parts[1]),
        label = paste(parts[-1], collapse = " ")
      )
    } else {
      NULL
    }
  })
  ch_data <- Filter(Negate(is.null), ch_data)

  # Create channel vectors
  channel_numbers <- sapply(ch_data, function(x) x$number)
  channel_labels <- sapply(ch_data, function(x) x$label)

  # Sort by channel number to ensure correct order
  sort_idx <- order(channel_numbers)
  channel_labels <- channel_labels[sort_idx]

  # Verify we have the expected number of channels
  if (length(channel_labels) != number_of_channels) {
    warning(
      "Number of channel labels (",
      length(channel_labels),
      ") doesn't match declared channel count (",
      number_of_channels,
      ")"
    )
  }

  # Return header_table
  header_table(
    record_name = record_name,
    file_name = file_name,
    number_of_channels = number_of_channels,
    samples = samples,
    start_time = start_time,
    frequency = frequency,
    ADC_saturation = ADC_saturation,
    label = channel_labels,
    additional_gain = rep(1.0, length(channel_labels)),
    low_pass = integer(length(channel_labels)),
    high_pass = integer(length(channel_labels)),
    color = rep('#000000', length(channel_labels)) # Default black for Prucka
  )
}


#' @rdname prucka
#' @export
read_prucka_signal <- function(signal_file, n = Inf) {
  if (!file.exists(signal_file)) {
    stop("File not found: ", signal_file)
  }

  # Read space-delimited signal data
  # The data appears to have:
  # - First column: sample number/index
  # - Remaining columns: channel data
  sig <- data.table::fread(
    signal_file,
    header = FALSE,
    nrows = n,
    sep = " "
  )

  # Convert to signal_table
  signal_table(sig)
}
