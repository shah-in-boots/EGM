# WFDB system backend wrappers -------------------------------------------------

write_wfdb_system <- function(
  data,
  record,
  record_dir,
  wfdb_path = getOption('wfdb_path'),
  header = list(frequency = 250, gain = 200L, label = character()),
  info_strings = list(),
  ...
) {
  # Options for `wrsamp`
  #       -F <numeric>            sampling frequency, default is 250
  #       -G <numeric>            gain in analog/digital units per milivolt, default is 200
  #               -i <string>                     input file (default standard input)
  #               -o <string>                     Write the signal file in current directory as 'record'
  #                                                                                               record.dat
  #                                                                                               record.hea
  #               -x <numeric>            Scaling factor if needed

  # Validation of paths
  wrsamp <- find_wfdb_command('wrsamp', wfdb_path)
  if (fs::dir_exists(record_dir)) {
    wd <- fs::path(record_dir)
  } else {
    wd <- getwd()
  }

  # Set up data, checking to see whether its using a `egm` set or not
  if (inherits(data, 'egm')) {
    signal <- data$signal
    header <- data$header
  } else if (inherits(data, 'data.frame')) {
    signal <- signal_table(data)
    if (length(header$label) == 0) {
      header$label <- colnames(data)
    }

    header <- header_table(
      record_name = record,
      number_of_channels = length(signal),
      frequency = header$frequency,
      ADC_gain = header$gain,
      label = header$label,
      info_strings = info_strings
    )
  }

  # Write out temporary CSV file for WFDB to use
  tmpFile <- fs::file_temp('wfdb_', ext = 'csv')
  withr::defer(fs::file_delete(tmpFile))
  data.table::fwrite(signal, file = tmpFile, col.names = FALSE)

  # Options for `wrsamp`
  #       -F <numeric>            sampling frequency, default is 250
  #       -G <numeric>            gain in analog/digital units per milivolt, default is 200
  #               -i <string>                     input file (default standard input)
  #               -o <string>                     Write the signal file in current directory as 'record'
  #                                                                                               record.dat
  #                                                                                               record.hea
  #               -x <numeric>            Scaling factor if needed

  # Frequency
  hz <- paste('-F', attributes(header)$record_line$frequency)

  # ADC = -G "adc adc adc adc" format
  adc <- paste(
    '-G',
    paste0('"', paste(header$ADC_gain, collapse = " "), '"')
  )

  # Input (full file path)
  ip <- paste('-i', tmpFile)

  # Output
  op <- paste('-o', record)

  # Additional specifications
  col0 <- paste('-z') # Ignore 'sample' column (first column = 0)

  # Write with `wrsamp`
  #       Change into correct folder/directory (the writing directory)
  #       Then reset to base directory
  #       Cleanup and remove temporary CSV file immediately
  withr::local_dir(new = wd)
  system2(command = wrsamp, args = c(hz, adc, ip, op, col0))

  # Modify header file with more data
  #       Record line (first one) needs a date and time appended
  #       Then handle the signal specification files
  headLine <-
    readLines(con = paste0(record, '.hea'), n = 1) |>
    paste(format(
      attributes(header)$record_line$start_time,
      '%H:%M:%OS %d/%m/%Y'
    ))

  # 10 columns:
  #       >= V9 and V10 are descriptive fields
  #               Should be a tab-delim field
  #                       Can contain spaces internal to it
  #       V3 is ADC units
  #                       Can be appended with baseline value '(0)'
  #               Can be appended with '/mV' to specify units
  headerFile <-
    utils::read.table(file = paste0(record, '.hea'), skip = 1)
  headerFile[[3]] <- paste0(headerFile[[3]], '(0)', '/mV', sep = '')
  headerFile <- headerFile[1:9]
  headerFile[9] <- header$label

  # Write header back in place
  writeLines(text = headLine, con = paste0(record, '.hea'))

  utils::write.table(
    headerFile,
    file = paste0(record, '.hea'),
    sep = ' ',
    quote = FALSE,
    col.names = FALSE,
    row.names = FALSE,
    append = TRUE
  )

  # Info strings are additional elements that may be available
  # Are placed after a `#` at end of header file
  # If there are additional lines in the header, can be placed in info section
  #       e.g. color, bandpass
  #       Otherwise uses named specific parameters like MRN or AGE

  additional_info <-
    header[, (ncol(header) - 5):ncol(header)] |>
    {
      \(.x) Filter(f = function(.y) !all(is.na(.y)), x = .x)
    }() |>
    as.list()

  info <- append(header$info_strings, additional_info)
  text <- lapply(info, function(.x) paste(.x, collapse = ' '))
  lines <- paste('#', names(info), text)
  write(lines, file = paste0(record, '.hea'), append = TRUE, sep = '\n')
}

read_wfdb_system <- function(
  record,
  record_dir = ".",
  annotator = NA_character_,
  wfdb_path = getOption("wfdb_path"),
  begin = 0,
  end = NA_integer_,
  interval = NA_integer_,
  units = "digital",
  channels = character(),
  ...
) {
  # Read signal
  sig <- read_signal_system(
    record = record,
    record_dir = record_dir,
    wfdb_path = wfdb_path,
    begin = begin,
    end = end,
    interval = interval,
    units = units,
    channels = channels
  )

  # Read header
  hea <- read_header(
    record = record,
    record_dir = record_dir,
    wfdb_path = wfdb_path
  )

  # Read annotation
  if (!is.na(annotator)) {
    ann <- read_annotation_system(
      record = record,
      record_dir = record_dir,
      annotator = annotator,
      wfdb_path = wfdb_path,
      begin = begin,
      end = end,
      interval = interval
    )
  } else {
    ann <- annotation_table()
  }

  # Resulting `egm` object
  egm(
    signal = sig,
    header = hea,
    annotation = ann
  )
}

read_signal_system <- function(
  record,
  record_dir = ".",
  wfdb_path = getOption("wfdb_path"),
  begin = 0L,
  end = NA_integer_,
  interval = NA_integer_,
  units = "digital",
  channels = character(),
  ...
) {
  # Validate:
  #               WFDB software command
  #       Current or parent working directory
  #       Directory of the record/WFDB files
  #       Variable definitions
  rdsamp <- find_wfdb_command("rdsamp", wfdb_path)

  if (fs::dir_exists(record_dir)) {
    wd <- fs::path(record_dir)
  } else {
    wd <- getwd()
  }

  stopifnot("Expected `integer`" = is.numeric(begin))
  stopifnot("Expected `integer`" = is.numeric(end))
  stopifnot("Expected `integer`" = is.numeric(interval))
  stopifnot(
    "Expected to be in c('digital', 'physical')" = units %in%
      c("digital", "physical")
  )

  # Create all the necessary parameters for rdsamp
  #               -f                      Start time
  #               -l, -t  Interval length OR end time ... one or other, not both
  #               -H                      High resolution mode for high-sampling frequencies
  #               -p                      Uses physical units instead of digital
  #                                                       Miliseconds/physical (mV) units
  #                                                       default is sample interval (index) and A/D units
  #       -P                      Capital P gives back higher number of decimal places
  #               -s                      Select which signals to print out (can duplicate + re-order)
  #                                                       Name or Number, separated by spaces
  #                                                       Default prints all signals
  cmd <-
    paste(rdsamp, "-r", record) |>
    {
      \(.) {
        if (begin != 0) {
          paste(., "-f", begin)
        } else {
          .
        }
      }
    }() |>
    {
      \(.) {
        if (!is.na(end)) {
          paste(., "-t", end)
        } else if (!is.na(interval)) {
          paste(., "-l", interval)
        } else {
          .
        }
      }
    }() |>
    {
      \(.) {
        if (identical(units, "physical")) {
          paste(., "-p")
        } else {
          .
        }
      }
    }() |>
    {
      \(.) {
        if (length(channels) > 0) {
          paste(
            .,
            "-s",
            paste(channels, collapse = " ")
          )
        } else {
          .
        }
      }
    }() |>
    paste("-v")

  # Temporary local/working directory, to reset at end of function
  withr::with_dir(new = wd, code = {
    dat <- data.table::fread(cmd = cmd)
  })

  # Return data after cleaning names
  names(dat)[1] <- "sample"
  signal_table(dat)
}

read_annotation_system <- function(
  record,
  record_dir = ".",
  annotator,
  wfdb_path = getOption("wfdb_path"),
  begin = "00:00:00",
  end = NA_character_,
  ...
) {
  # Validate:
  #               WFDB software command
  #       Current or parent working directory
  #       Directory of the record/WFDB files
  #       Variable definitions
  rdann <- find_wfdb_command('rdann', wfdb_path)

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

  # Ensure appropriate working directory
  if (fs::dir_exists(record_dir)) {
    wd <- fs::path(record_dir)
  } else {
    wd <- getwd()
  }

  stopifnot('Expected `character`' = is.character(begin))
  stopifnot('Expected `character`' = is.character(end))

  # Create all the necessary parameters for rdann
  #               -f                      Start time
  #               -t                      End time
  #               -v                      Column headings
  #               -e                      Elapsed time as (versus absolute time)
  # TODO filtering flags not yet included
  cmd <-
    paste(rdann, '-r', record, '-a', annotator) |>
    {
      \(.) {
        if (begin != 0) {
          paste(., '-f', begin)
        } else {
          .
        }
      }
    }() |>
    {
      \(.) {
        if (!is.na(end)) {
          paste(., '-t', end)
        } else {
          .
        }
      }
    }() |>
    paste('-e')

  # Temporary local/working directory, to reset at end of function
  #               Use system command and pipe into data.table
  withr::with_dir(new = wd, code = {
    dat <- data.table::fread(cmd = paste(cmd, '-v'))
  })

  annotation_table(dat)
}

write_annotation_system <- function(
  data,
  annotator,
  record,
  record_dir = '.',
  wfdb_path = getOption('wfdb_path'),
  overwrite = FALSE,
  header = NULL,
  ...
) {
  # Validate:
  #               WFDB software command
  #       Current or parent working directory
  #       Variable definitions
  wrann <- find_wfdb_command('wrann', wfdb_path)

  if (!fs::dir_exists(record_dir)) {
    fs::dir_create(record_dir, recurse = TRUE)
  }

  wd <- fs::path(record_dir)

  aux_attr <- attr(data, 'aux')

  if (inherits(data, 'annotation_table')) {
    data <- data.table::as.data.table(data)
  } else {
    stopifnot('Expected `data.frame`' = inherits(data, 'data.frame'))
  }

  data <- data.table::copy(data)

  annPath <- fs::path(record_dir, record, ext = annotator)
  if (isTRUE(overwrite) && fs::file_exists(annPath)) {
    fs::file_delete(annPath)
  }

  if (is.null(header)) {
    header_path <- fs::path(record_dir, record, ext = 'hea')
    if (fs::file_exists(header_path)) {
      header <- tryCatch(
        read_header(record, record_dir = record_dir),
        error = function(e) NULL
      )
    }
  }

  header_labels <- character()
  if (!is.null(header) && 'label' %in% names(header)) {
    header_labels <- native_canonicalize_labels(header$label)
  }

  channel_values <- data[['channel']]
  resolved <- annotation_resolve_channel_indices(channel_values, header_labels)
  if (length(resolved$changes)) {
    message(
      'Annotation channel labels were updated to match the header: ',
      paste(resolved$changes, collapse = ', ')
    )
  }
  data[['channel']] <- resolved$values

  if (!'aux' %in% names(data) && length(aux_attr)) {
    if (length(aux_attr) == nrow(data)) {
      data[['aux']] <- as.character(aux_attr)
    }
  }

  if ('aux' %in% names(data)) {
    aux_values <- as.character(data[['aux']])
    aux_values[is.na(aux_values)] <- ''
    data[['aux']] <- aux_values
  }

  # Take annotation data and write to temporary file
  #       This later is sent to `wrann` through `cat` with a pipe
  #               The temp file must be deleted after
  tmpFile <- fs::file_temp('annotation_', ext = 'txt')
  withr::defer(fs::file_delete(tmpFile))

  data |>
    annotation_table_to_lines() |>
    writeLines(tmpFile)

  # Prepare the command for writing this into a WFDB format
  #               Cat annotation file
  #               Pipe
  #       Write out file
  cat_cmd <- paste('cat', shQuote(tmpFile))
  wfdb_cmd <- paste(
    shQuote(wrann),
    '-r', shQuote(record),
    '-a', shQuote(annotator)
  )
  cmd <- paste(cat_cmd, wfdb_cmd, sep = ' | ')
  exit_status <- withr::with_dir(new = wd, code = system(cmd))

  if (!identical(exit_status, 0L)) {
    stop('wrann command failed with status ', exit_status, call. = FALSE)
  }

  invisible(annPath)
}
