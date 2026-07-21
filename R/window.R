# Class definition for `windowed` objects -------------------------------------

#' Create a `windowed` object containing a list of EGM windows
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `windowed` objects are lists of `EGM` objects that represent segments or
#' windows of the original signal. This allows for specialized methods to be
#' applied to collections of signal windows. This function primarily serves as
#' the class generation function, and only applies class attributes. It is used
#' by the [window()] function to ensure appropriate class and properties.
#'
#' @param x A list of `EGM` objects
#' @param window_method The windowing method used to create the list
#' @param source_record The name of the original record
#' @param ... Additional arguments passed to methods
#'
#' @return An object of class `windowed` which inherits from `list`
#'
#' @export
windowed <- function(
  x = list(),
  window_method = "rhythm",
  source_record = character(),
  ...
) {
  # Validate input
  if (!is.list(x)) {
    stop("x must be a list")
  }

  if (length(x) > 0) {
    # Check that all elements are EGM objects
    is_EGM_list <- all(sapply(x, inherits, "EGM"))
    if (!is_EGM_list) {
      stop("All elements of x must be of class 'EGM'")
    }
  }

  # Create the windowed object
  structure(
    x,
    class = c("windowed", "list"),
    window_method = window_method,
    source_record = source_record,
    window_count = length(x),
    creation_time = Sys.time()
  )
}

#' Test if an object is a windowed object
#'
#' @param x An object to test
#'
#' @return TRUE if x is a windowed object, FALSE otherwise
#'
#' @export
is_windowed <- function(x) {
  inherits(x, "windowed")
}

#' Format a windowed object for printing
#'
#' @param x A windowed object
#' @param ... Additional arguments passed to methods
#'
#' @return Invisibly returns x
#'
#' @export
format.windowed <- function(x, ...) {
  cat("<windowed: ", length(x), " EGM windows>\n", sep = "")
  # NB: the constructor stores this attribute as `window_method`; earlier
  # versions read a non-existent `method` attribute here and printed a blank.
  cat("Method: ", attr(x, "window_method"), "\n", sep = "")
  cat("Source: ", attr(x, "source_record"), "\n", sep = "")
  cat("Created: ", format(attr(x, "creation_time")), "\n", sep = "")

  invisible(x)
}

#' Print a windowed object
#'
#' @param x A windowed object
#' @param ... Additional arguments passed to methods
#'
#' @return Invisibly returns x
#'
#' @export
print.windowed <- function(x, ...) {
  format(x, ...)
  invisible(x)
}

#' Subset a windowed object
#'
#' @param x A windowed object
#' @param i Index to subset
#' @param ... Additional arguments passed to methods
#'
#' @return A windowed object with the specified subset of elements
#'
#' @export
`[.windowed` <- function(x, i, ...) {
  # Get original attributes
  attrs <- attributes(x)
  class_val <- attrs$class
  window_method_val <- attrs$window_method
  source_record_val <- attrs$source_record
  creation_time_val <- attrs$creation_time

  # Subset the list
  result <- NextMethod()

  # Restore the windowed class and update attributes. The method attribute is
  # `window_method` to match the windowed() constructor.
  structure(
    result,
    class = class_val,
    window_method = window_method_val,
    source_record = source_record_val,
    window_count = length(result),
    creation_time = creation_time_val
  )
}

#' Concatenate windowed objects
#'
#' @param ... windowed objects to concatenate
#'
#' @return A windowed object containing all the elements of the input objects
#'
#' @export
c.windowed <- function(...) {
  args <- list(...)

  # Check that all arguments are windowed objects
  if (!all(sapply(args, is_windowed))) {
    stop("All arguments must be `windowed` objects")
  }

  # TODO
  # Consider additional validation methods for making sure windows are similar

  # Get the first non-empty object's attributes
  first_non_empty <- which(sapply(args, length) > 0)[1]
  if (is.na(first_non_empty)) {
    first_non_empty <- 1 # All objects are empty, use first object's attributes
  }

  window_method_val <- attr(args[[first_non_empty]], "window_method")
  source_record_val <- attr(args[[first_non_empty]], "source_record")

  # Concatenate the lists
  result <- do.call(c, lapply(args, unclass))

  # Create the windowed object
  structure(
    result,
    class = c("windowed", "list"),
    window_method = window_method_val,
    source_record = source_record_val,
    window_count = length(result),
    creation_time = Sys.time()
  )
}

#' Apply a function to each element of a windowed object
#'
#' @param X A windowed object
#' @param FUN A function to apply to each element
#' @param ... Additional arguments passed to FUN
#'
#' @return A list of the results of applying FUN to each element of X,
#'   or a new windowed object if all results are EGM objects
#'
#' @export
lapply.windowed <- function(X, FUN, ...) {
  # Apply the function to each element
  results <- NextMethod()

  # Check if results are all EGM objects
  if (all(sapply(results, inherits, "EGM"))) {
    # Return a new windowed object (the constructor argument is `window_method`)
    return(windowed(
      results,
      window_method = attr(X, "window_method"),
      source_record = attr(X, "source_record")
    ))
  } else {
    # Return the results as a regular list
    return(results)
  }
}

# Windowing function(s) --------------------------------------------------------

#' Window signal data based on different methods
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Creates windows of signal data using various methods, such as rhythm patterns,
#' time intervals, or reference points. Each window is returned as an individual
#' `EGM` object for further analysis.
#'
#' @details
#' This function provides a modular approach to windowing electrophysiological
#' signals. The method parameter determines the windowing strategy, with each
#' method requiring its own set of additional parameters.
#'
#' @param object Object of the `EGM` class, which includes header, signal
#'   information, and annotation information.
#'
#' @param window_method A `character` string specifying the windowing method.
#'   Options include:
#'
#'   - **rhythm** - Windows based on rhythm patterns (requires rhythm_type and criteria)
#'
#' @param rhythm_type A `character` string specifying the rhythm type (e.g.,
#'   "sinus"). Currently supported: "sinus" (requires reference check).
#'
#' @param onset_criteria A named list of criteria to identify onset points.
#'   Names should match column names in the annotation table, with the addition
#'   of a virtual `wave` field (`"P"`, `"QRS"`, or `"T"`) that is inferred
#'   positionally from the enclosed peak symbol (see [label_waves()]). For
#'   `rhythm_type = "sinus"` this defaults to `list(type = "(", wave = "P")`
#'   (P-wave onset) when omitted.
#'
#' @param offset_criteria A named list of criteria to identify offset points.
#'   Supports the same fields as `onset_criteria`. For `rhythm_type = "sinus"`
#'   this defaults to `list(type = ")", wave = "T")` (T-wave offset) when
#'   omitted. Override either criterion for alternative segmentations (e.g.
#'   `offset_criteria = list(type = "(", wave = "P")` for P-onset to next
#'   P-onset).
#'
#' @param reference_criteria A named list of criteria to identify reference
#'   points that must exist between onset and offset. Set to NULL to skip
#'   reference validation. For `rhythm_type = "sinus"` this defaults to
#'   `list(type = "N")` (the QRS peak) when NULL.
#'
#' @param adjust_sample_indices Logical, whether to adjust signal and annotation
#'   sample indices in the returned windows to be zero-based and relative to the
#'   window start. Default is TRUE.
#'
#' @param resample_frequency Optional target sampling rate in Hz. When supplied,
#'   each returned window is resampled to this frequency (up- or down-sampling as
#'   needed) while preserving its duration, harmonising records captured at
#'   different rates. `NULL` (default) accepts each record's native rate. This is
#'   the inline equivalent of calling [resample_window()] on the result.
#'
#' @param ... Additional arguments passed to specific windowing methods.
#'
#' @return A list of `EGM` objects, each representing a window of the original
#'   signal.
#'
#' @export
window <- function(
  object,
  window_method = c("rhythm"),
  resample_frequency = NULL,
  ...
) {
  # Validate input
  stopifnot(
    "Requires object of <EGM> class for evaluation" = inherits(object, 'EGM')
  )

  # Match the method argument
  window_method <- match.arg(window_method)

  # Dispatch to the appropriate method handler
  windows <-
    # Can add specific methods here in the future
    switch(
      window_method,
      rhythm = window_by_rhythm(object, ...),
      stop("Unsupported windowing method: ", window_method)
    )

  # Optionally harmonise the sampling rate across windows. Done here (after
  # boundary detection, which relies on native-rate annotation samples) so each
  # extracted window is resampled independently and consistently.
  if (!is.null(resample_frequency) && length(windows) > 0) {
    windows <- lapply(windows, resample_egm, target_frequency = resample_frequency)
  }

  # Extract source record name
  source_record <-
    if (!is.null(object$header$record_name)) {
      object$header$record_name
    } else if (!is.null(attributes(object$header)$record_line$record_name)) {
      attributes(object$header)$record_line$record_name
    } else {
      "unknown"
    }

  # Return as `windowed` object
  # This is an internal class to allow for lists of `EGM` objects
  windowed(
    windows,
    window_method = window_method,
    source_record = source_record
  )
}

#' @rdname window
#' @param channel_criteria An optional channel that guides multi-lead windowing.
#'   When the annotation table spans multiple channels (e.g. an `ecgpuwave`-style
#'   file run per lead, where each lead is kept apart by the `channel` column),
#'   set this to the channel number whose annotations should define the window
#'   boundaries. Annotations on the global channel (`0`) are always retained. The
#'   returned windows still contain the signal for all channels. Default `NULL`
#'   uses every annotation, which is correct for single-channel annotation files.
#' @export
window_by_rhythm <- function(
  object,
  rhythm_type = "sinus",
  onset_criteria,
  offset_criteria,
  reference_criteria = NULL,
  channel_criteria = NULL,
  adjust_sample_indices = TRUE,
  ...
) {
  # Apply sensible, overridable defaults for sinus rhythm. Wave identity is
  # recovered positionally (see label_waves()), so a P-onset -> T-offset beat
  # with the QRS as reference can be windowed with just a guiding channel.
  if (rhythm_type == "sinus") {
    if (missing(onset_criteria)) {
      onset_criteria <- list(type = "(", wave = "P")
    }
    if (missing(offset_criteria)) {
      offset_criteria <- list(type = ")", wave = "T")
    }
    if (is.null(reference_criteria)) {
      reference_criteria <- list(type = "N")
    }
  }

  # Validate required parameters
  if (missing(onset_criteria)) {
    stop("onset_criteria is required for rhythm-based windowing")
  }
  if (missing(offset_criteria)) {
    stop("offset_criteria is required for rhythm-based windowing")
  }

  # Get signal, header, and annotation data
  sig <- data.table::copy(object$signal)
  hea <- data.table::copy(object$header)
  # Get first annotation from list (or empty if none)
  ann_list <- object$annotation
  if (length(ann_list) > 0) {
    ann <- data.table::copy(ann_list[[1]])
    if (length(ann_list) > 1) {
      message(
        "Multiple annotators found: ",
        paste(names(ann_list), collapse = ", "),
        ". Using '",
        names(ann_list)[1],
        "' for windowing. ",
        "Use get_annotation() to access other annotators."
      )
    }
  } else {
    ann <- annotation_table()
  }

  # Build a working copy used only for boundary detection. It carries an extra
  # `wave` column (P/QRS/T) inferred positionally, and is optionally restricted
  # to a single guiding channel. The pristine `ann` is left untouched so the
  # annotations stored in returned windows keep the strict annotation_table
  # column set.
  ann_work <- label_waves(ann)

  has_channel <- "channel" %in% colnames(ann_work)
  if (!is.null(channel_criteria) && has_channel) {
    # Use a local vector (not named `channel`) so data.table's non-standard
    # evaluation of the `i` expression does not capture the `channel` column.
    keep_channels <- c(as.integer(channel_criteria), 0L)
    ann_work <- ann_work[ann_work$channel %in% keep_channels, ]
  } else if (is.null(channel_criteria) && has_channel) {
    leads <- unique(ann_work$channel[ann_work$channel != 0L])
    if (length(leads) > 1) {
      warning(
        "Annotations span multiple channels (",
        paste(sort(leads), collapse = ", "),
        "); window boundaries may mix leads. ",
        "Specify `channel_criteria` to select a guiding lead."
      )
    }
  }

  # Helper function to filter annotations by criteria
  filter_annotations <- function(ann, criteria) {
    result <- ann
    for (col_name in names(criteria)) {
      if (!col_name %in% colnames(ann)) {
        stop("Column '", col_name, "' not found in annotation table")
      }
      result <- result[result[[col_name]] == criteria[[col_name]], ]
    }
    return(result)
  }

  # Get onset, offset, and reference points
  onset_points <- filter_annotations(ann_work, onset_criteria)
  offset_points <- filter_annotations(ann_work, offset_criteria)

  if (nrow(onset_points) == 0) {
    warning("No onset points found with specified criteria")
    return(list())
  }

  if (nrow(offset_points) == 0) {
    warning("No offset points found with specified criteria")
    return(list())
  }

  if (!is.null(reference_criteria)) {
    reference_points <- filter_annotations(ann_work, reference_criteria)
    if (nrow(reference_points) == 0) {
      warning("No reference points found with specified criteria")
      return(list())
    }
  } else {
    reference_points <- NULL
  }

  # Identify rhythm windows
  windows <- list()
  window_count <- 0

  # Extract sample points
  onset_samples <- onset_points$sample
  offset_samples <- offset_points$sample
  if (!is.null(reference_points)) {
    reference_samples <- reference_points$sample
  }

  for (i in seq_along(onset_samples)) {
    onset <- onset_samples[i]

    # Find the next offset after this onset
    # If there are no further offsets, this will not be a window
    next_offsets <- offset_samples[offset_samples > onset]
    if (length(next_offsets) == 0) {
      break
    }

    offset <- next_offsets[1]

    # Check if there's a reference point between onset and offset (if required)
    if (!is.null(reference_points)) {
      refs_between <- reference_samples[
        reference_samples > onset &
          reference_samples < offset
      ]
      if (length(refs_between) == 0) {
        # No reference point between onset and offset, skip this window
        next
      }
      # For sinus rhythm, store the reference point (QRS complex)
      if (rhythm_type == "sinus") {
        qrs <- refs_between[1]
      }
    }

    # Apply rhythm-specific validation
    if (rhythm_type == "sinus") {
      # For sinus, check if there's another onset between this onset and offset
      # (which might indicate overlap)
      onset_between <- onset_samples[
        onset_samples > onset &
          onset_samples < offset
      ]
      if (length(onset_between) > 0) {
        # For sinus, we typically want clean non-overlapping beats
        next
      }
    }

    # We have a valid window
    # C++ style increment
    window_count <- window_count + 1

    # Create window for this rhythm segment
    window_signal <- sig[sample >= onset & sample <= offset, ]

    # A window is a new WFDB record, so its sample coordinate starts at zero.
    # Callers can retain the source record's absolute indices by disabling this
    # adjustment explicitly.
    if (adjust_sample_indices) {
      window_signal$sample <- window_signal$sample - onset
    }

    source_record <- attributes(hea)$record_line
    window_start_time <- source_record$start_time
    if (adjust_sample_indices && inherits(window_start_time, "POSIXt") &&
        length(window_start_time) == 1L && !is.na(window_start_time)) {
      window_start_time <- window_start_time + onset / source_record$frequency
    }

    # Create header for this window
    info_string <- paste0(
      rhythm_type,
      " window ",
      window_count,
      ", onset: ",
      onset,
      ", offset: ",
      offset
    )

    if (rhythm_type == "sinus") {
      info_string <- paste0(info_string, ", QRS: ", qrs)
    }

    window_header <- header_table(
      record_name = paste0(
        attributes(hea)$record_line$record_name,
        "_",
        rhythm_type,
        window_count
      ),
      number_of_channels = attributes(hea)$record_line$number_of_channels,
      frequency = attributes(hea)$record_line$frequency,
      samples = nrow(window_signal),
      start_time = window_start_time,
      storage_format = hea$storage_format,
      ADC_gain = hea$ADC_gain,
      ADC_baseline = hea$ADC_baseline,
      ADC_units = hea$ADC_units,
      ADC_zero = hea$ADC_zero,
      ADC_resolution = hea$ADC_resolution,
      label = hea$label,
      info_strings = c(attributes(hea)$info_strings, window_info = info_string)
    )

    # Create annotation for this window
    window_annotation <- ann[sample >= onset & sample <= offset, ]

    # Adjust annotation sample indices to be relative to window start if requested
    if (adjust_sample_indices) {
      window_annotation$sample <- window_annotation$sample - onset
    }

    # Add to list of windows
    windows[[window_count]] <- EGM(
      signal = window_signal,
      header = window_header,
      annotation = window_annotation
    )
  }

  if (length(windows) == 0) {
    warning(
      "No complete ",
      rhythm_type,
      " windows found with the specified criteria"
    )
    return(list())
  }

  # Return list of windows
  windows
}

#' Label annotation waves positionally
#'
#' @description Adds a working `wave` column (one of `"P"`, `"QRS"`, `"T"`, or
#'   `NA`) to an annotation table by recovering wave identity from the peak
#'   symbol enclosed within each `(`/`)` waveform bracket. This is used by
#'   [window_by_rhythm()] to isolate P-onset -> T-offset beats even when the
#'   WFDB `number` column is unpopulated (e.g. `ecgpuwave` run per lead).
#'
#' @details Peaks are mapped directly by their `type` symbol (`p` -> `"P"`, `N`
#'   -> `"QRS"`, `t` -> `"T"`). Brackets are labelled per channel, in sample
#'   order: each onset `(` and its matching offset `)` inherit the wave of the
#'   single peak that falls between them (the first peak if several are present,
#'   `NA` if none). The returned table is a copy; the input is not modified.
#'
#' @param ann An `annotation_table` (or compatible `data.table`).
#'
#' @return A copy of `ann` with an additional `wave` column.
#'
#' @keywords internal
label_waves <- function(ann) {
  out <- data.table::as.data.table(data.table::copy(ann))
  out$wave <- NA_character_

  if (nrow(out) == 0 || !all(c("type", "sample") %in% colnames(out))) {
    return(out)
  }

  # Direct symbol -> wave mapping for peak annotations
  peak_map <- c(p = "P", N = "QRS", t = "T")
  is_peak <- out$type %in% names(peak_map)
  out$wave[is_peak] <- unname(peak_map[out$type[is_peak]])

  # Channel grouping for bracket inference; treat missing channel as a single
  # global group so single-channel files still work.
  channels <- if ("channel" %in% colnames(out)) out$channel else rep(0L, nrow(out))

  for (ch in unique(channels)) {
    idx <- which(channels == ch)
    idx <- idx[order(out$sample[idx])] # sample order within channel

    open_pos <- NA_integer_ # row index (in idx space) of pending onset
    enclosed_wave <- NA_character_ # wave of peak seen since the onset

    for (k in seq_along(idx)) {
      row <- idx[k]
      sym <- out$type[row]

      if (sym == "(") {
        open_pos <- k
        enclosed_wave <- NA_character_
      } else if (sym %in% names(peak_map)) {
        if (!is.na(open_pos) && is.na(enclosed_wave)) {
          enclosed_wave <- unname(peak_map[sym])
        }
      } else if (sym == ")") {
        if (!is.na(open_pos)) {
          out$wave[idx[open_pos]] <- enclosed_wave
          out$wave[row] <- enclosed_wave
        }
        open_pos <- NA_integer_
        enclosed_wave <- NA_character_
      }
    }
  }

  out
}

# Standardization and normalization of windows ---------------------------------

#' Standardize windows of signal data
#'
#' @description Standardizes `windowed` objects by applying various
#' transformations to each window. This function converts each `EGM` object in a
#' `windowed` list to a standardized data frame with uniform properties,
#' facilitating comparison and analysis.
#'
#' @details Currently supported standardization methods:
#'
#' * `time_normalize` - Resamples each window to a standard length by either
#' dilating or contracting the signal. The result is a signal with a consistent
#' number of samples regardless of the original window duration.
#'
#' Additional options:
#'
#' * `align_feature` - If provided, windows will be aligned to center around this
#' feature (e.g., a specific annotation type like "N" for R-peak). Can be a
#' character string matching an annotation type or a list of criteria for
#' annotation matching.
#'
#' * `preserve_amplitude` - If TRUE (default), maintains the original amplitude
#' range after resampling. If FALSE, the amplitudes may change due to
#' interpolation.
#'
#' @param x A `windowed` object to standardize
#' @param standardization_method A `character` string specifying the standardization method.
#'   Currently supported: "time_normalize".
#' @param target_samples The desired number of samples for each standardized
#'   window. Default is 500 samples. This parameter takes precedence if both
#'   target_samples and target_ms are provided.
#' @param target_ms Alternative specification in milliseconds. If provided and
#'   target_samples is NULL, the function will convert this to samples based on
#'   the signal's sampling frequency.
#' @param interpolation_method The method used for interpolation when
#'   resampling. Options are "linear" (default), "spline", or "step".
#' @param align_feature Feature to align windows around, either a character
#'   string matching an annotation type or a list of criteria for finding a
#'   specific annotation. Default is NULL (no alignment).
#' @param channel_criteria An optional channel that guides which lead's
#'   annotation is used when locating `align_feature`. Multi-lead annotation
#'   files (e.g. an `ecgpuwave`-style run per lead) carry one fiducial per lead
#'   at slightly different samples, so a bare `align_feature = "N"` would center
#'   on whichever lead sorts first. Set this to the channel number whose
#'   annotations should drive alignment (annotations on the global channel `0`
#'   are always retained). This mirrors the `channel_criteria` argument of
#'   [window()]. Default `NULL` uses every annotation. Ignored when
#'   `align_feature` is `NULL` or already specifies a `channel`.
#' @param preserve_amplitude Logical. If TRUE (default), maintains original
#'   amplitude range after resampling.
#' @param preserve_class Logical. If TRUE, returns a `windowed` object of
#'   standardized `EGM` objects. If FALSE (default), returns a plain list of
#'   `EGM` objects.
#' @param ... Additional arguments passed to specific standardization methods.
#'
#' @return A list of standardized `EGM` objects, one per window. Each carries a
#'   resampled `signal_table`, the window's own `header_table` (with `samples`
#'   updated to the standardized length and the per-beat record/file name
#'   preserved), and its annotations remapped onto the resampled time base. If
#'   `preserve_class=TRUE`, the list is wrapped as a `windowed` object.
#'
#' @examples
#' \dontrun{
#' # Read in ECG data
#' ecg <- read_wfdb("ecg", test_path(), "ecgpuwave")
#'
#' # Create windows based on sinus rhythm
#' windows <- window_signal(
#'   ecg,
#'   method = "rhythm",
#'   rhythm_type = "sinus",
#'   onset_criteria = list(type = "(", number = 0),
#'   offset_criteria = list(type = ")", number = 2),
#'   reference_criteria = list(type = "N")
#' )
#'
#' # Standardize windows to exactly 500 samples
#' std_windows <- standardize_windows(
#'   windows,
#'   method = "time_normalize",
#'   target_samples = 500
#' )
#'
#' # Alternatively, standardize to 500 milliseconds (depends on sampling frequency)
#' std_windows_ms <- standardize_windows(
#'   windows,
#'   method = "time_normalize",
#'   target_ms = 500
#' )
#'
#' # Standardize windows with QRS alignment
#' aligned_windows <- standardize_windows(
#'   windows,
#'   method = "time_normalize",
#'   target_samples = 500,
#'   align_feature = "N"  # Align on QRS complexes
#' )
#' }
#'
#' @export
standardize_windows <- function(
  x,
  standardization_method = c("time_normalize"),
  target_samples = 500,
  target_ms = NULL,
  interpolation_method = c("linear", "spline", "step"),
  align_feature = NULL,
  channel_criteria = NULL,
  preserve_amplitude = TRUE,
  preserve_class = FALSE,
  ...
) {
  # Validate input
  if (!is_windowed(x)) {
    stop("Input must be a windowed object")
  }

  # Match the method argument
  # Allows for multiple matches if we want to expand this in future
  standardization_method <- match.arg(standardization_method)
  interpolation_method <- match.arg(interpolation_method)

  # Dispatch to the appropriate standardization method
  # Keep the dots to pass additional features in the future
  standardized <- switch(
    standardization_method,
    time_normalize = time_normalize_windows(
      x,
      target_samples = target_samples,
      target_ms = target_ms,
      interpolation_method = interpolation_method,
      align_feature = align_feature,
      channel_criteria = channel_criteria,
      preserve_amplitude = preserve_amplitude,
      ...
    ),
    # Add additional methods here in the future
    stop("Unsupported standardization method: ", standardization_method)
  )

  # Return as appropriate class
  if (preserve_class) {
    return(windowed(
      standardized,
      window_method = paste0("standardized_", standardization_method),
      source_record = attr(x, "source_record")
    ))
  } else {
    return(standardized)
  }
}

#' Time normalize windows to a standard length
#' @keywords internal
time_normalize_windows <- function(
  x,
  target_samples = 500,
  target_ms = NULL,
  interpolation_method = "linear",
  align_feature = NULL,
  channel_criteria = NULL,
  preserve_amplitude = TRUE,
  ...
) {
  if (length(x) == 0) {
    return(list())
  }

  # Get the sampling frequency from the first window
  first_window <- x[[1]]
  if (!inherits(first_window, "EGM")) {
    stop("Windows must be EGM objects")
  }

  frequency <- attributes(first_window$header)$record_line$frequency

  # Determine target samples - either directly specified or converted from ms
  if (is.null(target_samples) && !is.null(target_ms)) {
    target_samples <- ceiling((target_ms / 1000) * frequency)
  } else if (is.null(target_samples)) {
    target_samples <- 500 # Default fallback
  }

  # Process each window
  standardized <- lapply(x, function(window) {
    # Extract the signal data
    signal_data <- window$signal

    # Collapse the (possibly multi-annotator) annotation list to a single
    # working table, used both for feature alignment and for carrying the
    # annotations forward into the standardized EGM.
    window_ann <- get_single_annotation(window)

    # Find the sample column index
    sample_col_idx <- which(names(signal_data) == "sample")
    signal_cols <- setdiff(1:ncol(signal_data), sample_col_idx)

    # Create a data frame to store the resampled data
    output_samples <- seq_len(target_samples) - 1L
    resampled_data <- data.frame(sample = output_samples)

    # Feature alignment (if requested). Restrict the lead used to *locate* the
    # alignment feature to the guiding channel, so multi-lead annotation files
    # (one fiducial per lead, at slightly different samples) center on a single
    # lead rather than whichever lead happens to sort first. The full
    # `window_ann` is still carried forward into the standardized EGM below.
    align_ann <- window_ann
    if (
      !is.null(channel_criteria) &&
        "channel" %in% names(align_ann) &&
        !(is.list(align_feature) && "channel" %in% names(align_feature))
    ) {
      keep_channels <- c(as.integer(channel_criteria), 0L)
      align_ann <- align_ann[align_ann$channel %in% keep_channels, ]
    }

    if (!is.null(align_feature) && nrow(align_ann) > 0) {
      # Find the feature in the annotations
      feature_idx <- NULL

      if (is.list(align_feature)) {
        # Filter annotations by criteria
        filtered_ann <- align_ann
        for (col_name in names(align_feature)) {
          if (col_name %in% names(filtered_ann)) {
            filtered_ann <- filtered_ann[
              filtered_ann[[col_name]] == align_feature[[col_name]],
            ]
          }
        }

        if (nrow(filtered_ann) > 0) {
          feature_idx <- filtered_ann$sample[1]
        }
      } else if (is.character(align_feature)) {
        # Check for a specific annotation type
        if ("type" %in% names(align_ann)) {
          type_match <- align_ann[
            align_ann$type == align_feature,
          ]
          if (nrow(type_match) > 0) {
            feature_idx <- type_match$sample[1]
          }
        }
      }

      if (!is.null(feature_idx)) {
        # Center the feature at native resolution: each output sample maps to a
        # single original sample, so output index `center_point` lands exactly
        # on `feature_idx`. Positions that fall outside the window are clamped
        # to the signal edges (rule = 2), padding rather than time-warping the
        # beat. This keeps the fiducial's true morphology timing intact.
        center_point <- floor((target_samples - 1L) / 2L)
        original_samples <- nrow(signal_data)
        original_indices <- signal_data$sample

        new_samples <- feature_idx + (output_samples - center_point)

        # Proceed with interpolation
        for (col in signal_cols) {
          col_name <- names(signal_data)[col]
          original_values <- signal_data[[col]]

          # Use the specified interpolation method
          if (interpolation_method == "linear") {
            resampled_values <- stats::approx(
              x = original_indices,
              y = original_values,
              xout = new_samples,
              method = "linear",
              rule = 2
            )$y
          } else if (interpolation_method == "spline") {
            resampled_values <- stats::spline(
              x = original_indices,
              y = original_values,
              xout = new_samples,
              method = "natural"
            )$y
          } else if (interpolation_method == "step") {
            resampled_values <- stats::approx(
              x = original_indices,
              y = original_values,
              xout = new_samples,
              method = "constant",
              rule = 2
            )$y
          }

          # Add to output
          resampled_data[[col_name]] <- resampled_values
        }
      } else {
        # Feature not found, fall back to regular resampling
        warning(
          "Specified alignment feature not found in annotations, using standard resampling"
        )
        original_indices <- signal_data$sample
        new_samples <- seq(
          min(original_indices),
          max(original_indices),
          length.out = target_samples
        )

        for (col in signal_cols) {
          col_name <- names(signal_data)[col]
          original_values <- signal_data[[col]]

          # Apply interpolation method
          resampled_values <- interpolate_signal(
            original_indices,
            original_values,
            new_samples,
            interpolation_method
          )

          # Add the resampled column to the output data frame
          resampled_data[[col_name]] <- resampled_values
        }
      }
    } else {
      # No feature alignment, standard resampling
      original_indices <- signal_data$sample
      new_samples <- seq(
        min(original_indices),
        max(original_indices),
        length.out = target_samples
      )

      for (col in signal_cols) {
        col_name <- names(signal_data)[col]
        original_values <- signal_data[[col]]

        # Apply interpolation method
        resampled_values <- interpolate_signal(
          original_indices,
          original_values,
          new_samples,
          interpolation_method
        )

        # Add the resampled column to the output data frame
        resampled_data[[col_name]] <- resampled_values
      }
    }

    # Preserve amplitude scale if requested
    if (preserve_amplitude) {
      for (col in signal_cols) {
        col_name <- names(signal_data)[col]
        original_range <- range(signal_data[[col]], na.rm = TRUE)
        resampled_range <- range(resampled_data[[col_name]], na.rm = TRUE)

        # Rescale to match original amplitude range
        if (diff(resampled_range) != 0) {
          # Avoid division by zero
          resampled_data[[col_name]] <-
            ((resampled_data[[col_name]] - resampled_range[1]) /
              diff(resampled_range)) *
            diff(original_range) +
            original_range[1]
        }
      }
    }

    # Reassemble the resampled columns into a signal_table
    std_signal <- do.call(signal_table, as.list(resampled_data))

    # Carry the per-beat header forward, updating only the sample count to
    # reflect the resampled length. The window's record_name/file_name stay
    # intact, so each standardized beat round-trips to disk under its own name
    # rather than inheriting the source record.
    std_header <- data.table::copy(window$header)
    record_line <- attributes(std_header)$record_line
    record_line$samples <- target_samples
    attr(std_header, "record_line") <- record_line

    # Map annotation sample indices onto the resampled time base. `new_samples`
    # holds the original positions sampled at each output index, so inverting it
    # gives the new location of any annotation. This handles both the aligned
    # and unaligned resampling paths.
    if (nrow(window_ann) > 0) {
      mapped <- stats::approx(
        x = new_samples,
        y = output_samples,
        xout = window_ann$sample,
        rule = 2
      )$y
      window_ann <- data.table::copy(window_ann)
      window_ann$sample <- as.integer(
        pmin(pmax(round(mapped), 0L), target_samples - 1L)
      )
    }

    # Return a fully-formed EGM for this beat
    new_EGM(
      signal = std_signal,
      header = std_header,
      annotation = window_ann
    )
  })

  # Return the list of standardized windows
  standardized
}

#' Helper function to apply interpolation
#' @keywords internal
interpolate_signal <- function(
  original_indices,
  original_values,
  new_samples,
  interpolation_method
) {
  if (interpolation_method == "linear") {
    return(
      stats::approx(
        x = original_indices,
        y = original_values,
        xout = new_samples,
        method = "linear",
        rule = 2
      )$y
    )
  } else if (interpolation_method == "spline") {
    return(
      stats::spline(
        x = original_indices,
        y = original_values,
        xout = new_samples,
        method = "natural"
      )$y
    )
  } else if (interpolation_method == "step") {
    return(
      stats::approx(
        x = original_indices,
        y = original_values,
        xout = new_samples,
        method = "constant",
        rule = 2
      )$y
    )
  }
}

# Shared window helpers --------------------------------------------------------

# The functions below (resampling, padding, median, warping) all operate on the
# same raw material: a `windowed` object (or a bare list of `EGM` objects). They
# repeatedly need to (a) accept either container, (b) recover the source record
# label so derived `windowed` objects stay traceable, (c) read the sampling
# frequency off a window, and (d) locate a fiducial (e.g. the QRS peak) within a
# single window. Factoring these here keeps the public functions small and their
# behaviour consistent with one another.

#' Coerce window input to a plain list of `EGM` objects
#'
#' @description Accepts either a `windowed` object or a bare list of `EGM`
#'   objects and returns it unchanged after validating that every element is an
#'   `EGM`. The window-transform functions iterate with base `lapply()`/`vapply()`
#'   (which treat a `windowed` object as an ordinary list), so no unclassing is
#'   required here.
#'
#' @param x A `windowed` object or list of `EGM` objects.
#' @param arg Name of the calling argument, used for a clearer error message.
#'
#' @return The input, validated as a list of `EGM` objects.
#'
#' @keywords internal
as_window_list <- function(x, arg = "x") {
  if (is_windowed(x)) {
    return(x)
  }
  if (is.list(x) && (length(x) == 0 || all(vapply(x, inherits, logical(1), "EGM")))) {
    return(x)
  }
  stop("`", arg, "` must be a `windowed` object or a list of `EGM` objects")
}

#' Recover the source record label from a window collection
#'
#' @description Reads the `source_record` attribute set by [windowed()]. Bare
#'   lists carry no such attribute, so an empty character vector is returned,
#'   which [windowed()] accepts as its default.
#'
#' @param x A `windowed` object or list of `EGM` objects.
#'
#' @return A `character` scalar (or empty character vector).
#'
#' @keywords internal
window_source_record <- function(x) {
  sr <- attr(x, "source_record")
  if (is.null(sr)) character() else sr
}

#' Sampling frequency of a single window
#' @keywords internal
window_frequency <- function(egm) {
  attributes(egm$header)$record_line$frequency
}

matches_feature_criterion <- function(x, criterion) {
  if (inherits(criterion, "feature_range")) {
    if (!is.numeric(x)) {
      return(rep(FALSE, length(x)))
    }
    if (criterion$inclusive) {
      return(!is.na(x) & x >= criterion$lower & x <= criterion$upper)
    }
    return(!is.na(x) & x > criterion$lower & x < criterion$upper)
  }
  if (is.function(criterion)) {
    keep <- criterion(x)
    if (!is.logical(keep) || length(keep) != length(x) || anyNA(keep)) {
      stop("A functional feature criterion must return one non-missing logical per row")
    }
    return(keep)
  }
  !is.na(x) & x %in% criterion
}

#' Specify a range-valued annotation criterion
#'
#' Creates a criterion for numeric annotation fields that can be placed in a
#' landmark criteria list, for example
#' `list(voltage = feature_range(-1, 1))`.
#'
#' @param lower,upper Finite numeric interval limits.
#' @param inclusive Whether both limits are included.
#' @return A `feature_range` criterion object.
#' @export
feature_range <- function(lower, upper, inclusive = TRUE) {
  if (length(lower) != 1L || length(upper) != 1L ||
    !is.finite(lower) || !is.finite(upper) || lower > upper) {
    stop("`lower` and `upper` must be finite scalars with lower <= upper")
  }
  if (length(inclusive) != 1L || is.na(inclusive)) {
    stop("`inclusive` must be TRUE or FALSE")
  }
  structure(
    list(
      lower = as.numeric(lower),
      upper = as.numeric(upper),
      inclusive = as.logical(inclusive)
    ),
    class = "feature_range"
  )
}

#' Locate fiducial samples within an annotation table
#'
#' @description Vector-returning engine behind [locate_feature()]. It preserves
#'   ambiguity so template learning and warping can reject or explicitly resolve
#'   multiple matches.
#'
#' @inheritParams locate_feature
#' @return An integer vector of every matching sample, in annotation order.
#' @keywords internal
locate_features <- function(ann, feature, channel_criteria = NULL) {
  if (is.null(ann) || nrow(ann) == 0) {
    return(integer())
  }
  criteria <- if (is.list(feature)) feature else list(type = feature)
  if (
    length(criteria) > 0 &&
      (is.null(names(criteria)) || any(!nzchar(names(criteria))))
  ) {
    stop("Feature criteria must be a named list")
  }

  work <- ann
  if ("wave" %in% names(criteria) && !("wave" %in% colnames(work))) {
    work <- label_waves(work)
  }

  filter_criteria <- function(rows) {
    for (col_name in names(criteria)) {
      if (!col_name %in% colnames(rows)) {
        return(rows[0, ])
      }
      keep <- matches_feature_criterion(rows[[col_name]], criteria[[col_name]])
      rows <- rows[keep, ]
    }
    rows
  }

  # A requested channel is exact when that channel contains a matching feature.
  # Global channel 0 is a feature-level fallback only; including both and taking
  # the first can accidentally select a global event instead of the requested
  # lead.
  if (
    !is.null(channel_criteria) && "channel" %in% colnames(work) &&
      !("channel" %in% names(criteria))
  ) {
    requested <- filter_criteria(
      work[work$channel == as.integer(channel_criteria), ]
    )
    if (nrow(requested) > 0) {
      return(as.integer(requested$sample))
    }
    global <- filter_criteria(work[work$channel == 0L, ])
    return(as.integer(global$sample))
  }

  work <- filter_criteria(work)
  as.integer(work$sample)
}

#' Locate a fiducial sample within an annotation table
#'
#' @description Returns the sample index of the first annotation that matches a
#'   feature specification. This is the common lookup used to anchor padding
#'   ([pad_window()]), aligned medians ([median_window()]) and landmark
#'   template learning/warping ([learn_template()], [warp_window()]) on a fiducial
#'   such as the QRS peak.
#'
#' @details The feature may be given as a bare `character` (matched against the
#'   `type` column, e.g. `"N"` for the QRS peak) or as a named `list` of
#'   criteria (matched column-by-column, e.g. `list(type = "(", wave = "P")`).
#'   When a `wave` criterion is requested the positional wave labels are recovered
#'   on demand via [label_waves()], mirroring how [window_by_rhythm()] resolves
#'   P/QRS/T identity. When `channel_criteria` is supplied and the annotations
#'   carry a `channel` column, the search is restricted to that channel plus the
#'   global channel `0`, unless the feature itself already pins a `channel`.
#'
#' @param ann An `annotation_table` (or compatible `data.table`).
#' @param feature A `character` type symbol or a named list of criteria.
#' @param channel_criteria Optional guiding channel number.
#'
#' @param multiple How to handle multiple matches: return the `"first"`
#'   (default), or raise an `"error"`.
#'
#' @return An integer sample index, or `NA_integer_` when no match is found.
#'
#' @keywords internal
locate_feature <- function(
  ann,
  feature,
  channel_criteria = NULL,
  multiple = c("first", "error")
) {
  multiple <- match.arg(multiple)
  matches <- locate_features(ann, feature, channel_criteria)
  if (length(matches) == 0) {
    return(NA_integer_)
  }
  if (length(matches) > 1 && multiple == "error") {
    stop("Feature matched more than one annotation")
  }
  matches[[1]]
}

#' Resample a single `EGM` to a target sampling frequency
#'
#' @description Duration-preserving resample of one window. The signal is
#'   re-interpolated onto a new grid whose length scales with the frequency
#'   ratio, the header frequency/sample-count are updated, and annotation sample
#'   indices are rescaled to the new grid. This is the per-window engine behind
#'   [resample_window()] and the `resample_frequency` argument of [window()].
#'
#' @details A window read at 250 Hz and resampled to 500 Hz keeps the same
#'   wall-clock duration but gains (approximately) twice the samples; morphology
#'   timing is preserved because each output sample maps back to a real source
#'   position (`output / ratio`). Frequencies that already match are a no-op.
#'
#' @param object An `EGM` window.
#' @param target_frequency Target sampling rate in Hz (positive).
#' @param interpolation_method One of "linear", "spline", or "step".
#'
#' @return A resampled `EGM` object.
#'
#' @keywords internal
resample_egm <- function(
  object,
  target_frequency,
  interpolation_method = "linear"
) {
  stopifnot(inherits(object, "EGM"))

  record_line <- attributes(object$header)$record_line
  orig_freq <- record_line$frequency
  if (
    is.null(orig_freq) || length(orig_freq) == 0 ||
      is.na(orig_freq) || orig_freq <= 0
  ) {
    stop("Cannot resample: the window's source frequency is unknown")
  }
  if (
    is.null(target_frequency) || is.na(target_frequency) ||
      target_frequency <= 0
  ) {
    stop("`resample_frequency` must be a positive value in Hz (or NULL)")
  }

  # Matching frequencies need no work; return the window untouched
  if (isTRUE(all.equal(as.numeric(orig_freq), as.numeric(target_frequency)))) {
    return(object)
  }

  sig <- object$signal
  n_orig <- nrow(sig)
  if (n_orig < 2) {
    return(object)
  }

  orig_idx <- sig$sample
  ratio <- as.numeric(target_frequency) / as.numeric(orig_freq)

  # New length keeps the endpoints fixed: (n-1) source steps become (n-1)*ratio
  # output steps. Each output index is projected back to a source position so
  # the interpolation samples true morphology rather than a warped copy.
  n_new <- max(2L, as.integer(round((n_orig - 1L) * ratio)) + 1L)
  output_samples <- seq_len(n_new) - 1L
  src_pos <- orig_idx[1] + output_samples / ratio

  signal_cols <- setdiff(names(sig), "sample")
  resampled_data <- data.frame(sample = output_samples)
  for (col in signal_cols) {
    resampled_data[[col]] <- interpolate_signal(
      orig_idx,
      sig[[col]],
      src_pos,
      interpolation_method
    )
  }
  new_signal <- do.call(signal_table, as.list(resampled_data))

  # Header carries forward; only frequency and sample count change
  new_header <- data.table::copy(object$header)
  rl <- attributes(new_header)$record_line
  rl$frequency <- target_frequency
  rl$samples <- n_new
  attr(new_header, "record_line") <- rl

  # Annotations move with the grid: positions rescale by the same ratio
  ann <- get_single_annotation(object)
  if (nrow(ann) > 0) {
    ann <- data.table::copy(ann)
    ann$sample <- as.integer(round((ann$sample - orig_idx[1]) * ratio))
    ann$sample <- pmin(pmax(ann$sample, 0L), n_new - 1L)
  }

  new_EGM(signal = new_signal, header = new_header, annotation = ann)
}

# Resampling windows -----------------------------------------------------------

#' Resample windows to a common sampling frequency
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Resamples every window in a `windowed` object to a shared sampling frequency,
#' up- or down-sampling as needed while preserving each window's duration. This
#' harmonises records captured at different rates (e.g. mixing 250 Hz and 500 Hz
#' ECGs) so that downstream operations such as [median_window()] or
#' [standardize_windows()] compare like with like.
#'
#' @details Resampling is duration-preserving: a 0.8 s beat stays 0.8 s long but
#'   its sample count scales with the frequency ratio. Signal values are
#'   re-interpolated onto the new grid, the per-window header frequency and
#'   sample count are updated, and annotation sample indices are rescaled to the
#'   new grid. The same machinery is available inline via the
#'   `resample_frequency` argument of [window()].
#'
#' @param x A `windowed` object (or list of `EGM` objects) to resample.
#' @param resample_frequency Target sampling rate in Hz. `NULL` accepts the
#'   current sampling rate and returns the windows unchanged, which lets callers
#'   pass a possibly-`NULL` rate straight through.
#' @param interpolation_method The interpolation method used when resampling.
#'   One of "linear" (default), "spline", or "step".
#' @param preserve_class Logical. If TRUE (default), returns a `windowed` object;
#'   if FALSE, returns a plain list of `EGM` objects.
#' @param ... Additional arguments (currently unused).
#'
#' @return A `windowed` object (or list) of resampled `EGM` objects.
#'
#' @export
resample_window <- function(
  x,
  resample_frequency = NULL,
  interpolation_method = c("linear", "spline", "step"),
  preserve_class = TRUE,
  ...
) {
  windows <- as_window_list(x)
  interpolation_method <- match.arg(interpolation_method)

  if (is.null(resample_frequency)) {
    # NULL means "accept the current sampling rate": pass through untouched
    resampled <- windows
  } else {
    resampled <- lapply(
      windows,
      resample_egm,
      target_frequency = resample_frequency,
      interpolation_method = interpolation_method
    )
  }

  if (preserve_class) {
    return(windowed(
      resampled,
      window_method = "resampled",
      source_record = window_source_record(x)
    ))
  }
  resampled
}

# Padding windows --------------------------------------------------------------

#' Pad windows to a common length
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Pads each window with a constant value (zero by default) so that every window
#' shares a common length. Unlike time normalization, padding is
#' *non-destructive*: the original samples are preserved and only flat padding is
#' added at the edges. This is primarily used to anchor a fiducial - typically
#' the QRS peak - at a fixed index across windows, or simply to make ragged
#' windows rectangular for stacking (e.g. before [median_window()]).
#'
#' @details The `align` argument controls where the real signal sits within the
#'   padded window:
#'
#'   * `center` - equal padding on both edges.
#'   * `left` - signal at the start, padding appended after ("pad after").
#'   * `right` - padding prepended before, signal at the end ("pad before").
#'   * `feature` - each window is shifted so that `align_feature` lands on a
#'     shared anchor index, padding the remainder. This is the alignment used to
#'     stack beats on the R wave.
#'
#'   For `feature` alignment with no explicit `target_samples`, the output length
#'   is derived so that no window is truncated: it spans the largest pre-feature
#'   extent plus the largest post-feature extent seen across the collection, and
#'   the feature is anchored at that common pre-feature offset. When
#'   `target_samples` is supplied, the feature is anchored at `feature_position`
#'   (default: the window centre) and any samples that fall outside the target
#'   are truncated.
#'
#' @param x A `windowed` object (or list of `EGM` objects) to pad.
#' @param target_samples Desired output length in samples. If `NULL` (default),
#'   the length is derived from the data (the longest window, or the feature-span
#'   for `align = "feature"`).
#' @param target_ms Alternative target length in milliseconds, converted to
#'   samples using the first window's sampling frequency. Ignored when
#'   `target_samples` is supplied.
#' @param align One of "center" (default), "left", "right", or "feature". See
#'   details.
#' @param align_feature Feature used for `align = "feature"`, given as a
#'   `character` type symbol (default "N", the QRS peak) or a named list of
#'   annotation criteria. Ignored for other alignments.
#' @param feature_position Target index (0-based) for the aligned feature when
#'   `align = "feature"` and `target_samples` is supplied. Defaults to the window
#'   centre.
#' @param channel_criteria Optional guiding channel used when locating
#'   `align_feature` in multi-lead annotation tables. Mirrors the argument of the
#'   same name in [window()].
#' @param pad_value The value used for padding. Defaults to `0`.
#' @param preserve_class Logical. If TRUE (default), returns a `windowed` object;
#'   if FALSE, returns a plain list of `EGM` objects.
#' @param ... Additional arguments (currently unused).
#'
#' @return A `windowed` object (or list) of padded `EGM` objects, all sharing the
#'   same sample length.
#'
#' @export
pad_window <- function(
  x,
  target_samples = NULL,
  target_ms = NULL,
  align = c("center", "left", "right", "feature"),
  align_feature = "N",
  feature_position = NULL,
  channel_criteria = NULL,
  pad_value = 0,
  preserve_class = TRUE,
  ...
) {
  windows <- as_window_list(x)
  align <- match.arg(align)

  if (length(windows) == 0) {
    if (preserve_class) {
      return(windowed(
        list(),
        window_method = "padded",
        source_record = window_source_record(x)
      ))
    }
    return(list())
  }

  # Resolve a millisecond target to samples using the first window's frequency
  if (is.null(target_samples) && !is.null(target_ms)) {
    freq <- window_frequency(windows[[1]])
    target_samples <- ceiling((target_ms / 1000) * freq)
  }

  window_lengths <- vapply(windows, function(w) nrow(w$signal), integer(1))

  # For feature alignment, locate the fiducial in each window and derive the
  # shared anchor. `feature_pos[i]` is the 0-based position of the feature within
  # window i; windows lacking the feature fall back to their own centre so they
  # are still stacked sensibly rather than dropped.
  if (align == "feature") {
    feature_pos <- vapply(
      windows,
      function(w) {
        as.numeric(locate_feature(
          get_single_annotation(w),
          align_feature,
          channel_criteria
        ))
      },
      numeric(1)
    )
    if (all(is.na(feature_pos))) {
      stop("Alignment feature not found in any window; cannot align on it")
    }
    missing_feat <- is.na(feature_pos)
    if (any(missing_feat)) {
      warning(
        sum(missing_feat),
        " window(s) lack the alignment feature; centring those on the window itself"
      )
      feature_pos[missing_feat] <- floor((window_lengths[missing_feat] - 1) / 2)
    }

    pre_extent <- feature_pos # samples before the feature
    post_extent <- (window_lengths - 1) - feature_pos # samples after the feature

    if (is.null(target_samples)) {
      # Auto length: span the worst case on each side so nothing is truncated
      anchor <- max(pre_extent)
      total <- as.integer(anchor + max(post_extent) + 1L)
    } else {
      total <- as.integer(target_samples)
      anchor <- if (!is.null(feature_position)) {
        as.integer(feature_position)
      } else {
        floor((total - 1L) / 2L)
      }
    }
    # `place[i]` is where window i's sample 0 lands in the output grid
    place <- anchor - feature_pos
  } else {
    total <- if (!is.null(target_samples)) {
      as.integer(target_samples)
    } else {
      max(window_lengths)
    }
    place <- switch(
      align,
      left = rep(0, length(windows)), # signal first, pad appended
      right = total - window_lengths, # pad prepended, signal last
      center = floor((total - window_lengths) / 2)
    )
  }

  padded <- lapply(seq_along(windows), function(i) {
    window <- windows[[i]]
    signal_data <- window$signal
    signal_cols <- setdiff(names(signal_data), "sample")
    place_i <- place[i]

    # Fill an all-`pad_value` grid, copying source samples into their placed
    # positions. Positions outside [0, total-1] are silently dropped, which is
    # how truncation is expressed when a target length is too small.
    out <- data.frame(sample = seq_len(total) - 1L)
    src_idx <- seq_len(nrow(signal_data))
    dst_idx <- place_i + (src_idx - 1L) # 0-based output positions
    keep <- dst_idx >= 0 & dst_idx <= (total - 1L)
    for (col in signal_cols) {
      values <- rep(pad_value, total)
      values[dst_idx[keep] + 1L] <- signal_data[[col]][src_idx[keep]]
      out[[col]] <- values
    }
    padded_signal <- do.call(signal_table, as.list(out))

    # Header: same record, new sample count
    padded_header <- data.table::copy(window$header)
    rl <- attributes(padded_header)$record_line
    rl$samples <- total
    attr(padded_header, "record_line") <- rl

    # Annotations shift with the signal by the same placement offset; those that
    # land outside the padded grid are dropped rather than clamped, to avoid
    # inventing fiducials at the edges.
    padded_ann <- get_single_annotation(window)
    if (nrow(padded_ann) > 0) {
      padded_ann <- data.table::copy(padded_ann)
      padded_ann$sample <- as.integer(padded_ann$sample + place_i)
      padded_ann <- padded_ann[
        padded_ann$sample >= 0L & padded_ann$sample <= (total - 1L),
      ]
    }

    new_EGM(
      signal = padded_signal,
      header = padded_header,
      annotation = padded_ann
    )
  })

  if (preserve_class) {
    return(windowed(
      padded,
      window_method = "padded",
      source_record = window_source_record(x)
    ))
  }
  padded
}

# Median beats -----------------------------------------------------------------

#' Collapse windows to a single median beat
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Reduces a `windowed` object to a single representative `EGM` by taking the
#' element-wise median across all windows, lead by lead. Given an ECG windowed
#' into its individual beats, this returns the "median beat" - a robust template
#' that suppresses beat-to-beat noise and ectopy while preserving the dominant
#' morphology.
#'
#' @details The median is computed per sample and per lead, so every window must
#'   share the same length and the same set of leads. Beats windowed from raw
#'   rhythm are generally *not* the same length (see [window()]), so this
#'   function offers to align them first: when `align_feature` is supplied and
#'   the windows differ in length, they are padded and anchored on that feature
#'   via [pad_window()] before the median is taken. If the windows differ in
#'   length and no `align_feature` is given, an error is raised pointing to
#'   [pad_window()] or [normalize_window()] so the caller chooses the alignment
#'   explicitly. The returned beat carries an empty annotation table - a median
#'   of many beats has no single set of fiducials - and a header named
#'   `<source>_median`.
#'
#' @param x A `windowed` object (or list of `EGM` objects) to reduce.
#' @param align_feature Optional feature used to pad-align windows of unequal
#'   length before averaging, given as a `character` type symbol (e.g. "N", the
#'   QRS peak) or a named list of annotation criteria. If `NULL` (default), the
#'   windows must already share a common length.
#' @param channel_criteria Optional guiding channel used when locating
#'   `align_feature`. Mirrors the argument of the same name in [window()].
#' @param na.rm Logical passed to [stats::median()]; if TRUE (default), missing
#'   values are ignored when averaging.
#' @param ... Additional arguments (currently unused).
#'
#' @return A single `EGM` object representing the median beat.
#'
#' @export
median_window <- function(
  x,
  align_feature = NULL,
  channel_criteria = NULL,
  na.rm = TRUE,
  ...
) {
  windows <- as_window_list(x)
  if (length(windows) == 0) {
    stop("Cannot compute a median beat from zero windows")
  }

  window_lengths <- vapply(windows, function(w) nrow(w$signal), integer(1))

  # Windows must be rectangular to stack. If they are not, either align on the
  # requested feature or send the caller to an explicit standardization step.
  if (length(unique(window_lengths)) > 1) {
    if (is.null(align_feature)) {
      stop(
        "Windows have differing lengths (",
        min(window_lengths),
        "-",
        max(window_lengths),
        " samples). Supply `align_feature` to pad-align them on a fiducial, or ",
        "pre-process with pad_window()/normalize_window() so all windows share a length."
      )
    }
    windows <- pad_window(
      windows,
      align = "feature",
      align_feature = align_feature,
      channel_criteria = channel_criteria,
      preserve_class = FALSE
    )
    window_lengths <- vapply(windows, function(w) nrow(w$signal), integer(1))
  }

  n <- window_lengths[1]

  # Require an identical lead set across windows. Because windows come from one
  # source record they normally share leads exactly; enforcing this keeps the
  # header handling trivial and avoids silently averaging mismatched channels.
  lead_sets <- lapply(windows, function(w) setdiff(names(w$signal), "sample"))
  leads <- lead_sets[[1]]
  same_leads <- all(vapply(
    lead_sets,
    function(l) identical(l, leads),
    logical(1)
  ))
  if (!same_leads) {
    stop("All windows must share the same set of leads to compute a median beat")
  }
  if (length(leads) == 0) {
    stop("Windows contain no signal leads")
  }

  # Median per lead: assemble an (n x n_windows) matrix and take the row-wise
  # median. as.numeric() guards against integer-typed signal columns tripping
  # vapply's strict type check.
  median_data <- data.frame(sample = seq_len(n) - 1L)
  for (lead in leads) {
    mat <- vapply(
      windows,
      function(w) as.numeric(w$signal[[lead]]),
      numeric(n)
    )
    median_data[[lead]] <- apply(mat, 1, stats::median, na.rm = na.rm)
  }
  median_signal <- do.call(signal_table, as.list(median_data))

  # Header from the first window, renamed and re-sized. A median beat has no
  # single true fiducial set, so annotations are intentionally left empty.
  median_header <- data.table::copy(windows[[1]]$header)
  rl <- attributes(median_header)$record_line
  rl$samples <- n
  src <- window_source_record(x)
  base_name <- if (length(src) > 0 && nzchar(src[1])) src[1] else "window"
  rl$record_name <- paste0(base_name, "_median")
  attr(median_header, "record_line") <- rl

  new_EGM(
    signal = median_signal,
    header = median_header,
    annotation = annotation_table()
  )
}

# Whole-window time normalization ----------------------------------------------

#' Time-normalize whole windows to a fixed length
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Stretches or compresses each window as a whole so that every window spans the
#' same number of samples, mapping the left and right borders (e.g. P-onset and
#' T-offset for a sinus beat) onto a common `[0, target_samples)` grid. This is a
#' *destructive* standardization - the time axis is warped, so absolute intervals
#' are no longer comparable - but it is a fast, simple way to bring ragged beats
#' onto a shared basis for averaging or matrix operations.
#'
#' @details This is the whole-window (border-to-border) counterpart to the
#'   feature-anchored resampling in [standardize_windows()]; internally it reuses
#'   the same interpolation engine with alignment disabled. For alignment on a
#'   fiducial without warping the time axis, use [pad_window()] instead; to change
#'   the physical sampling rate while preserving duration, use [resample_window()].
#'
#' @param x A `windowed` object (or list of `EGM` objects) to normalize.
#' @param target_samples Desired output length in samples. Default 500.
#' @param target_ms Alternative target length in milliseconds, converted using
#'   the first window's sampling frequency. Used only when `target_samples` is
#'   `NULL`.
#' @param interpolation_method Interpolation method for the stretch. One of
#'   "linear" (default), "spline", or "step".
#' @param preserve_amplitude Logical. If TRUE, rescales each lead back to its
#'   original amplitude range after interpolation. Defaults to FALSE, since a
#'   pure time stretch should leave amplitudes to the interpolation.
#' @param preserve_class Logical. If TRUE (default), returns a `windowed` object;
#'   if FALSE, returns a plain list of `EGM` objects.
#' @param ... Additional arguments (currently unused).
#'
#' @return A `windowed` object (or list) of time-normalized `EGM` objects, each
#'   exactly `target_samples` long.
#'
#' @export
normalize_window <- function(
  x,
  target_samples = 500L,
  target_ms = NULL,
  interpolation_method = c("linear", "spline", "step"),
  preserve_amplitude = FALSE,
  preserve_class = TRUE,
  ...
) {
  windows <- as_window_list(x)
  interpolation_method <- match.arg(interpolation_method)

  # Reuse the existing whole-window stretch engine. Passing align_feature = NULL
  # selects its border-to-border resampling path, which is exactly the "stretch
  # the two edges onto a fixed grid" behaviour described here.
  normalized <- time_normalize_windows(
    windows,
    target_samples = target_samples,
    target_ms = target_ms,
    interpolation_method = interpolation_method,
    align_feature = NULL,
    preserve_amplitude = preserve_amplitude
  )

  if (preserve_class) {
    return(windowed(
      normalized,
      window_method = "normalized",
      source_record = window_source_record(x)
    ))
  }
  normalized
}

# Landmark warping ------------------------------------------------------------

#' Landmark-warp windows onto a template
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Warps each window so that its landmarks (located from annotations) align to
#' the positions held in a [template], typically created by [learn_template()].
#' Between consecutive landmarks the time axis is stretched or compressed
#' piecewise-linearly, giving a landmark-aligned beat on the template's
#' common time base. This is the most flexible - and most destructive - of the
#' standardization methods, aligning multiple fiducials at once rather than a
#' single anchor.
#'
#' @details Before warping, each landmark defined by the template is
#'   confirmed against the windows; landmarks missing from some windows raise a
#'   warning (those windows warp on their remaining anchors) and landmarks absent
#'   from every window are dropped. For every window the landmark samples are
#'   located - each on its own channel, so a multi-channel template is
#'   honoured - and combined with the window's own start/end as edge anchors,
#'   forming a set of `(source -> target)` breakpoints. Output samples are mapped
#'   back to source positions by inverting this piecewise-linear correspondence,
#'   and each lead is interpolated there; annotations are carried forward through
#'   the same map. Any breakpoints that are non-increasing in either axis are
#'   collapsed to keep the mapping monotonic. All leads are warped regardless of
#'   which channel a landmark was detected on.
#'
#' @param x A `windowed` object (or list of `EGM` objects) to warp.
#' @param template A [template] created manually with [template()] or learned
#'   with [learn_template()]. Plain lists are not accepted.
#' @param interpolation_method Interpolation method for the warp. One of "linear"
#'   (default), "spline", or "step".
#' @param channel_criteria Optional fallback guiding channel used when locating a
#'   landmark whose own spec does not name a channel. A landmark's own channel
#'   always takes precedence.
#' @param preserve_amplitude Logical. If TRUE, rescales each lead back to its
#'   original amplitude range after warping. Defaults to FALSE.
#' @param preserve_class Logical. If TRUE (default), returns a `windowed` object;
#'   if FALSE, returns a plain list of `EGM` objects.
#' @param missing What to do when a required landmark is absent: warp with the
#'   remaining landmarks, drop that window, or error.
#' @param ambiguous What to do when a landmark matches multiple annotations:
#'   error, use the first, or drop that window.
#' @param order_policy What to do when landmarks are crossed or duplicated:
#'   error or drop that window.
#' @param ... Additional arguments (currently unused).
#'
#' @return A `windowed` object (or list) of landmark-warped `EGM` objects, each
#'   `target_samples` long (as defined by the template).
#'
#' @seealso [learn_template()], [template()], [landmark()]
#'
#' @export
warp_window <- function(
  x,
  template,
  interpolation_method = c("linear", "spline", "step"),
  channel_criteria = NULL,
  preserve_amplitude = FALSE,
  preserve_class = TRUE,
  missing = c("partial", "drop", "error"),
  ambiguous = c("error", "first", "drop"),
  order_policy = c("error", "drop"),
  ...
) {
  windows <- as_window_list(x)
  if (!is_template(template)) {
    stop("`template` must be a template object")
  }
  interpolation_method <- match.arg(interpolation_method)
  missing <- match.arg(missing)
  ambiguous <- match.arg(ambiguous)
  order_policy <- match.arg(order_policy)
  target_samples <- template@target_samples
  output_samples <- seq_len(target_samples) - 1L
  landmarks <- template@landmarks
  landmark_names <- vapply(landmarks, function(x) x@name, character(1))
  target_positions <- stats::setNames(
    vapply(landmarks, function(x) x@position, numeric(1)),
    landmark_names
  )
  required <- vapply(landmarks, function(x) x@required, logical(1))

  if (length(windows) == 0) {
    if (preserve_class) {
      return(windowed(
        list(),
        window_method = "warped",
        source_record = window_source_record(x)
      ))
    }
    return(list())
  }

  mappings <- lapply(seq_along(windows), function(i) {
    window <- windows[[i]]
    signal_samples <- as.numeric(window$signal$sample)
    if (length(signal_samples) < 2L || any(!is.finite(signal_samples)) ||
      any(diff(signal_samples) <= 0)) {
      stop("Window ", i, " must contain at least two increasing signal samples")
    }

    annotations <- get_single_annotation(window)
    source_positions <- rep(NA_real_, length(landmarks))
    found <- rep(FALSE, length(landmarks))
    issues <- character()
    invalid <- FALSE

    for (j in seq_along(landmarks)) {
      point <- landmarks[[j]]
      channel <- if (!channel_is_unset(point@channel)) {
        point@channel
      } else {
        channel_criteria
      }
      matches <- locate_features(
        annotations,
        point@criteria,
        resolve_channel_spec(window, channel)
      )
      if (length(matches) > 1L) {
        issue <- paste0(
          landmark_names[j], " matched ", length(matches), " annotations"
        )
        if (ambiguous == "error") {
          stop("Window ", i, ": ", issue)
        }
        issues <- c(issues, issue)
        if (ambiguous == "drop") {
          invalid <- TRUE
          next
        }
      }
      if (length(matches) > 0L) {
        source_positions[j] <- as.numeric(matches[1])
        found[j] <- TRUE
      }
    }

    missing_required <- required & !found
    if (any(missing_required)) {
      issue <- paste0(
        "missing required: ",
        paste(landmark_names[missing_required], collapse = ", ")
      )
      issues <- c(issues, issue)
      if (missing == "error") {
        stop("Window ", i, ": ", issue)
      }
      if (missing == "drop") {
        invalid <- TRUE
      }
    }
    missing_optional <- !required & !found
    if (any(missing_optional)) {
      issues <- c(
        issues,
        paste0(
          "missing optional: ",
          paste(landmark_names[missing_optional], collapse = ", ")
        )
      )
    }

    # Window edges provide the phase boundaries. Landmarks at the two target
    # edges are descriptive but redundant as interpolation breakpoints.
    used <- found & target_positions > 0 &
      target_positions < target_samples - 1L
    used_indices <- which(used)
    if (length(used_indices) > 1L &&
      any(diff(source_positions[used_indices]) <= 0)) {
      issue <- "crossed or duplicate warp anchors"
      issues <- c(issues, issue)
      if (order_policy == "error") {
        stop("Window ", i, ": ", issue)
      }
      invalid <- TRUE
    }

    source_breaks <- c(
      signal_samples[1],
      source_positions[used],
      signal_samples[length(signal_samples)]
    )
    target_breaks <- c(
      0,
      target_positions[used],
      target_samples - 1L
    )
    if (any(diff(source_breaks) <= 0) || any(diff(target_breaks) <= 0)) {
      issue <- "warp anchor coincides with or crosses a window edge"
      issues <- c(issues, issue)
      if (order_policy == "error") {
        stop("Window ", i, ": ", issue)
      }
      invalid <- TRUE
      source_breaks <- c(
        signal_samples[1],
        signal_samples[length(signal_samples)]
      )
      target_breaks <- c(0, target_samples - 1L)
      used[] <- FALSE
    }

    list(
      index = as.integer(i),
      source_positions = stats::setNames(source_positions, landmark_names),
      target_positions = target_positions,
      found = stats::setNames(found, landmark_names),
      used = stats::setNames(used, landmark_names),
      source_breaks = source_breaks,
      target_breaks = target_breaks,
      status = if (invalid) {
        "invalid"
      } else if (length(issues) > 0L) {
        "partial"
      } else {
        "ok"
      },
      issues = issues
    )
  })

  found_matrix <- do.call(rbind, lapply(mappings, function(x) x$found))
  found_per_landmark <- colSums(found_matrix)
  if (any(found_per_landmark == 0L)) {
    warning(
      "Landmark(s) not found in any window: ",
      paste(landmark_names[found_per_landmark == 0L], collapse = ", ")
    )
  }
  partial <- found_per_landmark > 0L & found_per_landmark < length(windows)
  if (any(partial)) {
    warning(
      "Landmark(s) missing from some windows: ",
      paste(landmark_names[partial], collapse = ", ")
    )
  }

  keep <- vapply(
    mappings,
    function(mapping) mapping$status != "invalid",
    logical(1)
  )
  windows_to_warp <- windows[keep]
  mappings <- mappings[keep]

  warped <- lapply(seq_along(windows_to_warp), function(i) {
    window <- windows_to_warp[[i]]
    mapping <- mappings[[i]]
    signal_data <- window$signal
    signal_cols <- setdiff(names(signal_data), "sample")
    src_idx <- signal_data$sample
    ann <- get_single_annotation(window)
    src_break <- mapping$source_breaks
    tgt_break <- mapping$target_breaks

    # Inverse map (target -> source) gives the source position sampled at each
    # output index; rule = 2 clamps beyond the outer anchors.
    src_pos <- stats::approx(
      x = tgt_break,
      y = src_break,
      xout = output_samples,
      rule = 2
    )$y

    out <- data.frame(sample = output_samples)
    for (col in signal_cols) {
      out[[col]] <- interpolate_signal(
        src_idx,
        signal_data[[col]],
        src_pos,
        interpolation_method
      )
    }

    if (preserve_amplitude) {
      for (col in signal_cols) {
        original_range <- range(signal_data[[col]], na.rm = TRUE)
        warped_range <- range(out[[col]], na.rm = TRUE)
        if (diff(warped_range) != 0) {
          out[[col]] <- ((out[[col]] - warped_range[1]) / diff(warped_range)) *
            diff(original_range) +
            original_range[1]
        }
      }
    }

    warped_signal <- do.call(signal_table, as.list(out))

    warped_header <- data.table::copy(window$header)
    rl <- attributes(warped_header)$record_line
    # A phase warp has no single physical sampling interval. Retain the original
    # frequency for compatibility, but label its provenance explicitly and keep
    # the exact physical mapping in the attached warp mapping.
    rl$source_frequency <- rl$frequency
    rl$samples <- target_samples
    rl$time_scale <- "normalized_phase"
    rl$time_warped <- TRUE
    attr(warped_header, "record_line") <- rl

    # Annotations move forward through the same (source -> target) map
    if (nrow(ann) > 0) {
      ann <- data.table::copy(ann)
      forward <- stats::approx(
        x = src_break,
        y = tgt_break,
        xout = ann$sample,
        rule = 2
      )$y
      ann$sample <- as.integer(
        pmin(pmax(round(forward), 0L), target_samples - 1L)
      )
    }

    new_EGM(
      signal = warped_signal,
      header = warped_header,
      annotation = ann
    )
  })

  attr(warped, "warp_mappings") <- mappings

  if (preserve_class) {
    out <- windowed(
      warped,
      window_method = "warped",
      source_record = window_source_record(x)
    )
    attr(out, "warp_mappings") <- mappings
    return(out)
  }
  warped
}
