# WFDB helpers -----------------------------------------------------------------

# Small utilities shared by the WFDB readers and writers: turning an annotation
# table back into the fixed-width text a `wrann`-style file holds, reading a
# record's start date and time out of a header line, and normalising the
# `begin`/`end`/`interval` window every reader takes.

#' @keywords internal
#' @noRd
annotation_table_to_lines <- function(data) {
  # Each annotation file has a string length of 42 characters
  # Each annotation `rdann -e` has 4 characters of spaces up front
  # When using the `-e` option for rdann, gives an elapsed time
  # That assumption leads to spaces before the time starts

  # Columns are... n = 6
  #		Time
  #		Sample
  #		Annotation
  #		Type
  #		Subtype
  #		Channel
  #		Number
  #		Auxillary (7th, ignored)

  # The spacing is as such...
  # 	[TIME] = 12
  # 	[SAMPLE] = 9
  # 	[TYPE] = 6
  # 	[SUBTYPE] = 5
  # 	[CHANNEL] = 5
  # 	[NUMBER] = 5

  # Each column can get appropriately padded back into lines
  v1 <- stringr::str_pad(data[[1]], width = 12, side = "left")
  v2 <- stringr::str_pad(data[[2]], width = 9, side = "left")
  v3 <- stringr::str_pad(data[[3]], width = 6, side = "left")
  v4 <- stringr::str_pad(data[[4]], width = 5, side = "left")
  v5 <- stringr::str_pad(data[[5]], width = 5, side = "left")
  v6 <- stringr::str_pad(data[[6]], width = 5, side = "left")

  # Output will be put back into `wrann` compatible lines
  # 	base::sprintf() is 2-3 faster than paste
  # 	lines <- paste0(v1, v2, v3, v4, v5, v6)
  lines <- sprintf(paste0(rep("%s", 6), collapse = ""), v1, v2, v3, v4, v5, v6)

  # Return
  lines
}

#' Evaluates a character string and extracts first date and time objects
#' Internally contains different matches for different WFDB formats
#' Requires that string can be broken into components via a space
#' @keywords internal
#' @noRd
parse_date_and_time <- function(x) {
  stopifnot('Requires `x` to be a `character`' = is.character(x))

  # Time
  # 	Assumes HH:MM:SS.SSS
  tm <- stringr::str_extract(x, '\\d\\d:\\d\\d:\\d\\d')

  # Dates are more varied
  # 	DD/MM/YYYY
  dt <- stringr::str_extract(x, '\\d+/\\d+/\\d+')

  # Create date time
  as.POSIXct(strptime(paste(tm[1], dt[1]), "%H:%M:%OS %d/%m/%Y"))
}

#' Validate WFDB time parameters
#'
#' `validate_time_parameters()` validates and normalizes the time window used
#' by the WFDB readers. Time-only `begin` and `end` values are elapsed from the
#' record start, consistent with WFDB standard time. Dated values are absolute.
#' `interval` is a duration measured from `begin`.
#'
#' @param begin,end A single time value. Character values may use
#'   `"HH:MM:SS"`, `"YYYY-MM-DD HH:MM:SS"`, or the WFDB-style
#'   `"HH:MM:SS DD/MM/YYYY"` format, with optional fractional seconds and
#'   optional square brackets around a dated WFDB timestamp.
#'   `POSIXt`, `hms`, `ITime`, and `difftime` objects are also supported.
#'   A `difftime` value is interpreted as elapsed time from the record start.
#' @param interval A single duration. Numeric values are seconds. Character
#'   values combine a number and unit, such as `"30s"`, `"100 ms"`, `"2 min"`,
#'   or `"1.5 hours"`. A `difftime` value is also accepted.
#' @param start_time The record start as a `POSIXt` value. This is required for
#'   absolute `begin` and `end` values, but not for elapsed time-only values.
#' @param study_duration The available record duration in seconds. When known,
#'   requested endpoints are clamped to this duration. Records longer than one
#'   day require dated `begin` and `end` values instead of time-only clocks.
#'
#' @returns A list containing normalized `begin`, `end`, and `interval` values
#'   in elapsed seconds from the beginning of the record.
#'
#' @export
validate_time_parameters <- function(
  begin = NULL,
  end = NULL,
  interval = NULL,
  start_time = as.POSIXct(NA),
  study_duration = NA_real_
) {
  duration_known <- length(study_duration) == 1L && !is.na(study_duration)
  if (!duration_known) {
    study_duration <- NA_real_
  } else {
    study_duration <- as.numeric(study_duration)
    if (!is.finite(study_duration) || study_duration < 0) {
      stop("`study_duration` must be finite and non-negative", call. = FALSE)
    }
  }

  start_known <- inherits(start_time, "POSIXt") &&
    length(start_time) == 1L && !is.na(start_time)
  date_required <- duration_known && study_duration > 24 * 60 * 60

  require_date <- function(argument) {
    if (date_required) {
      stop(
        "`", argument,
        "` must include a date because the study is longer than one day",
        call. = FALSE
      )
    }
  }

  is_missing <- function(x, argument) {
    if (is.null(x)) {
      return(TRUE)
    }
    if (length(x) != 1L) {
      stop("`", argument, "` must contain exactly one value", call. = FALSE)
    }
    is.atomic(x) && length(x) == 1L && is.na(x)
  }

  parse_clock_string <- function(x, argument) {
    x <- trimws(x)
    if (grepl("^\\[.*\\]$", x)) {
      x <- substring(x, 2L, nchar(x) - 1L)
    }
    time_pattern <- "(?:[01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](?:\\.[0-9]+)?"
    time_only <- paste0("^", time_pattern, "$")
    iso_datetime <- paste0("^[0-9]{4}-[0-9]{2}-[0-9]{2}[ T]", time_pattern, "$")
    wfdb_datetime <- paste0("^", time_pattern, " [0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$")
    dated <- grepl(iso_datetime, x) || grepl(wfdb_datetime, x)

    if (!grepl(time_only, x) && !dated) {
      stop(
        "`", argument,
        "` must use HH:MM:SS, YYYY-MM-DD HH:MM:SS, or HH:MM:SS DD/MM/YYYY",
        call. = FALSE
      )
    }
    if (!dated) {
      require_date(argument)
      return(parse_clock_seconds(x))
    }
    if (!start_known) {
      stop(
        "`", argument,
        "` includes a clock time, but the WFDB header has no start time",
        call. = FALSE
      )
    }

    timezone <- attr(start_time, "tzone")
    if (is.null(timezone) || length(timezone) == 0L || !nzchar(timezone[[1]])) {
      timezone <- ""
    } else {
      timezone <- timezone[[1]]
    }
    formats <- if (grepl("^[0-9]{4}-", x)) {
      c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%dT%H:%M:%OS")
    } else {
      "%H:%M:%OS %d/%m/%Y"
    }
    parsed <- as.POSIXct(NA)
    for (format in formats) {
      parsed <- as.POSIXct(strptime(x, format = format, tz = timezone))
      if (!is.na(parsed)) {
        break
      }
    }
    if (is.na(parsed)) {
      stop("`", argument, "` is not a valid date and time", call. = FALSE)
    }
    as.numeric(difftime(parsed, start_time, units = "secs"))
  }

  parse_clock_seconds <- function(x) {
    pieces <- strsplit(x, ":", fixed = TRUE)[[1]]
    as.numeric(pieces[[1]]) * 3600 +
      as.numeric(pieces[[2]]) * 60 +
      as.numeric(pieces[[3]])
  }

  parse_time <- function(x, argument) {
    if (is_missing(x, argument)) {
      return(NA_real_)
    }
    if (inherits(x, "POSIXt")) {
      if (!start_known) {
        stop(
          "`", argument,
          "` is an absolute time, but the WFDB header has no start time",
          call. = FALSE
        )
      }
      value <- as.numeric(difftime(x, start_time, units = "secs"))
    } else if (inherits(x, "hms") || inherits(x, "ITime")) {
      require_date(argument)
      value <- as.numeric(x)
    } else if (inherits(x, "difftime")) {
      require_date(argument)
      value <- as.numeric(x, units = "secs")
    } else if (is.character(x)) {
      value <- parse_clock_string(x, argument)
    } else {
      stop(
        "`", argument,
        "` must be a character timestamp or a supported R time object",
        call. = FALSE
      )
    }
    if (!is.finite(value) || value < 0) {
      stop("`", argument, "` cannot be before the study begins", call. = FALSE)
    }
    # Clock arithmetic can leave residue such as 0.020000000004 seconds,
    # which would otherwise advance an exact sample boundary by one position.
    round(value, digits = 9L)
  }

  parse_interval <- function(x) {
    if (is_missing(x, "interval")) {
      return(NA_real_)
    }
    if (inherits(x, "difftime")) {
      value <- as.numeric(x, units = "secs")
    } else if (is.numeric(x)) {
      value <- as.numeric(x)
    } else if (is.character(x)) {
      match <- regexec(
        "^\\s*([0-9]+(?:\\.[0-9]+)?)\\s*([^[:space:]]+)\\s*$",
        x,
        perl = TRUE
      )
      parts <- regmatches(x, match)[[1]]
      if (length(parts) != 3L) {
        stop(
          "`interval` must be numeric seconds or a duration such as '30s' or '100 ms'",
          call. = FALSE
        )
      }
      unit <- tolower(parts[[3]])
      if (unit %in% c("\u00b5s", "\u03bcs")) {
        unit <- "us"
      }
      multipliers <- c(
        ns = 1e-9, nanosecond = 1e-9, nanoseconds = 1e-9,
        us = 1e-6,
        microsecond = 1e-6, microseconds = 1e-6,
        ms = 1e-3, millisecond = 1e-3, milliseconds = 1e-3,
        s = 1, sec = 1, secs = 1, second = 1, seconds = 1,
        m = 60, min = 60, mins = 60, minute = 60, minutes = 60,
        h = 3600, hr = 3600, hrs = 3600, hour = 3600, hours = 3600,
        d = 86400, day = 86400, days = 86400
      )
      if (!unit %in% names(multipliers)) {
        stop("Unsupported `interval` unit: ", parts[[3]], call. = FALSE)
      }
      value <- as.numeric(parts[[2]]) * multipliers[[unit]]
    } else {
      stop(
        "`interval` must be numeric seconds, a duration string, or a difftime",
        call. = FALSE
      )
    }
    if (!is.finite(value) || value < 0) {
      stop("`interval` must be finite and non-negative", call. = FALSE)
    }
    value
  }

  begin_seconds <- parse_time(begin, "begin")
  if (is.na(begin_seconds)) {
    begin_seconds <- 0
  }
  end_seconds <- parse_time(end, "end")
  interval_seconds <- parse_interval(interval)

  if (!is.na(interval_seconds)) {
    end_seconds <- begin_seconds + interval_seconds
  } else if (is.na(end_seconds) && duration_known) {
    end_seconds <- study_duration
  }

  if (duration_known) {
    begin_seconds <- min(begin_seconds, study_duration)
    if (!is.na(end_seconds)) {
      end_seconds <- min(end_seconds, study_duration)
    }
  }
  if (!is.na(end_seconds)) {
    end_seconds <- max(begin_seconds, end_seconds)
  }

  list(
    begin = begin_seconds,
    end = end_seconds,
    interval = interval_seconds
  )
}

wfdb_seconds_to_sample <- function(x, frequency, argument) {
  if (is.na(x)) {
    return(NA_integer_)
  }
  raw_sample <- x * as.numeric(frequency)
  tolerance <- .Machine$double.eps * max(1, abs(raw_sample)) * 8
  sample <- ceiling(raw_sample - tolerance)
  if (sample > .Machine$integer.max) {
    stop(
      "`", argument, "` exceeds the sample range supported by this reader",
      call. = FALSE
    )
  }
  as.integer(sample)
}

wfdb_sample_range <- function(
  begin = NULL,
  end = NULL,
  interval = NULL,
  frequency,
  total_samples,
  start_time = as.POSIXct(NA)
) {
  if (length(frequency) != 1L || is.na(frequency) || frequency <= 0) {
    stop("The WFDB header must contain a positive sampling frequency", call. = FALSE)
  }
  duration <- if (
    length(total_samples) == 1L && !is.na(total_samples) && total_samples >= 0
  ) {
    as.numeric(total_samples) / as.numeric(frequency)
  } else {
    NA_real_
  }
  validated <- validate_time_parameters(
    begin = begin,
    end = end,
    interval = interval,
    start_time = start_time,
    study_duration = duration
  )
  begin_sample <- wfdb_seconds_to_sample(
    validated$begin,
    frequency,
    "begin"
  )
  end_sample <- wfdb_seconds_to_sample(validated$end, frequency, "end")
  if (is.na(end_sample)) {
    end_sample <- if (
      length(total_samples) == 1L && !is.na(total_samples) &&
        total_samples >= 0
    ) {
      as.integer(total_samples)
    } else {
      .Machine$integer.max
    }
  }

  list(begin = as.integer(begin_sample), end = as.integer(end_sample))
}

#' Whether a path is absolute
#'
#' @description The one thing base R does not ship a predicate for, and the
#'   reason the readers and writers need it: a header may name its signal file
#'   either relative to the record directory or by an absolute path, and joining
#'   an absolute path onto `record_dir` silently addresses a file that is not
#'   there.
#'
#' @details Matches a POSIX root, a home-relative `~`, a Windows drive letter,
#'   and a UNC share, so a header written on one platform is read the same way
#'   on another.
#'
#' @param x A `character` vector of paths.
#'
#' @return A `logical` vector the same length as `x`.
#'
#' @keywords internal
#' @noRd
is_absolute_path <- function(x) {
  grepl("^(/|~|[A-Za-z]:[/\\\\]|\\\\\\\\)", x)
}
