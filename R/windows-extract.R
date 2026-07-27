# Window extraction ------------------------------------------------------------

# Extracting windows is a search over a whole study: find every span of signal
# that satisfies some criterion, and return each one as a self-contained `EGM`.
# The criterion is expressed as a *strategy* object rather than as a pile of
# arguments on the entry point. That separation matters because each strategy
# needs a different set of arguments - rhythm windowing needs onset/offset
# fiducials, an interval strategy would need a duration and an overlap, a
# window-of-interest strategy would need a reference and two offsets - and a
# single flat argument list cannot validate any of them. With strategies, the
# arguments are checked where they are written, and each strategy documents
# itself.
#
# Adding a strategy is three steps: write a `by_*()` constructor, write a
# `windows_by_*()` engine, and add one line to `window_registry()`.

# Strategy class ---------------------------------------------------------------

#' A window extraction strategy
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Describes *how* windows should be found within a record, without performing
#' the search. Strategy objects are values: build one once and reuse it across
#' every record in a study. Pass one to [get_windows()].
#'
#' Strategies are created by the `by_*()` constructors, which validate their own
#' arguments; this class only carries the resolved result. [by_rhythm()] follows
#' the signal and returns windows of differing length; [by_beat()] cuts a fixed
#' span around a fiducial and returns windows that are all the same length.
#'
#' @param method The registry key naming the extraction engine, e.g. `"rhythm"`.
#' @param params A named list of resolved, strategy-specific parameters.
#'
#' @return A `window_strategy` S7 object.
#'
#' @seealso [by_rhythm()], [by_beat()], [get_windows()]
#'
#' @export
window_strategy <- S7::new_class(
  "window_strategy",
  properties = list(
    method = S7::class_character,
    params = S7::class_list
  ),
  validator = function(self) {
    if (!valid_scalar_string(self@method)) {
      return("`method` must be a single string")
    }
    if (
      length(self@params) > 0L &&
        (is.null(names(self@params)) || any(!nzchar(names(self@params))))
    ) {
      return("`params` must be a named list")
    }
    NULL
  }
)

#' Test whether an object is a window strategy
#'
#' @param x An object to test.
#' @return A single logical value.
#' @export
is_window_strategy <- function(x) S7::S7_inherits(x, window_strategy)

S7::method(print, window_strategy) <- function(x, ...) {
  cat("<window_strategy: ", x@method, ">\n", sep = "")
  for (nm in names(x@params)) {
    value <- x@params[[nm]]
    shown <- if (is.null(value)) {
      "any"
    } else if (is.list(value)) {
      # Criteria lists print as `column = value` pairs, which is how they were
      # written at the call site
      paste(
        paste0(names(value), " = ", vapply(value, format_criterion, character(1))),
        collapse = ", "
      )
    } else {
      paste(format(value), collapse = ", ")
    }
    cat("  ", nm, ": ", shown, "\n", sep = "")
  }
  invisible(x)
}

# Argument checks shared by the `by_*()` constructors, so that a mistyped channel
# or flag is reported the same way whichever strategy it was written on.
valid_channel <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (
    length(x) != 1L || !is.numeric(x) || is.na(x) || !is.finite(x) ||
      x < 0 || x != as.integer(x)
  ) {
    stop("`channel` must be NULL or one non-negative whole number")
  }
  as.integer(x)
}

valid_flag <- function(x, arg) {
  if (length(x) != 1L || !is.logical(x) || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE")
  }
  x
}

# Criteria values are usually scalars, but may be a `feature_range` or a
# predicate function, neither of which has a useful `format()` method.
format_criterion <- function(x) {
  if (inherits(x, "feature_range")) {
    return(paste0(
      if (x$inclusive) "[" else "(",
      x$lower, ", ", x$upper,
      if (x$inclusive) "]" else ")"
    ))
  }
  if (is.function(x)) {
    return("<function>")
  }
  paste(format(x), collapse = "/")
}

# The single place that knows which strategies exist. A function rather than a
# top-level list so that it does not depend on definition order within the file.
window_registry <- function() {
  list(
    rhythm = list(strategy = by_rhythm, engine = windows_by_rhythm),
    beat = list(strategy = by_beat, engine = windows_by_beat)
  )
}

# Accept either a fully specified strategy object or the bare name of one. The
# name path routes through the same constructor, so both forms are validated
# identically and neither can drift from the other.
as_window_strategy <- function(by) {
  if (is_window_strategy(by)) {
    return(by)
  }
  registry <- window_registry()
  if (valid_scalar_string(by)) {
    entry <- registry[[by]]
    if (is.null(entry)) {
      stop(
        "Unsupported windowing strategy: ",
        by,
        ". Available: ",
        paste(names(registry), collapse = ", ")
      )
    }
    return(entry$strategy())
  }
  stop(
    "`by` must be a `window_strategy` object or a single strategy name (",
    paste(names(registry), collapse = ", "),
    ")"
  )
}

# Rhythm strategy --------------------------------------------------------------

#' Window by rhythm
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Builds a [window_strategy] that finds windows bounded by a pair of annotation
#' fiducials - an onset and the next matching offset - optionally requiring a
#' reference fiducial to fall between them. For sinus rhythm this yields one
#' window per beat, from P-onset to T-offset with the QRS as the reference.
#'
#' @details Criteria are named lists matched column-by-column against the
#'   annotation table, with the addition of a virtual `wave` field (`"P"`,
#'   `"QRS"`, or `"T"`) inferred positionally from the peak symbol enclosed by
#'   each `(`/`)` bracket pair (see [label_waves()]). This recovers wave identity
#'   even when the WFDB `number` column is unpopulated, as with `ecgpuwave` run
#'   per lead.
#'
#'   When `rhythm = "sinus"` the criteria default to a full beat and windows that
#'   contain a second onset are rejected, so that only clean non-overlapping
#'   beats are returned. Other rhythm names require `onset` and `offset` to be
#'   given explicitly.
#'
#' @param rhythm A `character` naming the rhythm type. Currently `"sinus"` is
#'   the only value with built-in defaults and overlap rejection.
#' @param onset A named list of criteria identifying window onsets. Defaults to
#'   `list(type = "(", wave = "P")` (P-wave onset) for sinus.
#' @param offset A named list of criteria identifying window offsets. Defaults to
#'   `list(type = ")", wave = "T")` (T-wave offset) for sinus. Override either
#'   for alternative segmentations, e.g. `offset = list(type = "(", wave = "P")`
#'   for P-onset to next P-onset.
#' @param reference A named list of criteria for a fiducial that must exist
#'   between onset and offset, or `NULL` to skip the check. Defaults to
#'   `list(type = "N")` (the QRS peak) for sinus.
#' @param channel An optional channel number guiding multi-lead windowing. When
#'   the annotation table spans multiple channels (e.g. an `ecgpuwave`-style file
#'   run per lead, where each lead is kept apart by the `channel` column), set
#'   this to the channel whose annotations should define the window boundaries.
#'   Annotations on the global channel (`0`) are always retained, and the
#'   returned windows still contain the signal for all channels. `NULL` (default)
#'   uses every annotation, which is correct for single-channel annotation files.
#' @param adjust_sample_indices Logical, whether to rebase signal and annotation
#'   sample indices in the returned windows to be zero-based and relative to the
#'   window start. Default `TRUE`, since each window is a new WFDB record.
#'
#' @return A [window_strategy] object.
#'
#' @examples
#' # Sinus defaults: one clean P-onset -> T-offset beat per window
#' by_rhythm()
#'
#' # Guided by a single lead in a multi-lead annotation file
#' by_rhythm(channel = 2)
#'
#' # P-onset to next P-onset instead of a full beat
#' by_rhythm(offset = list(type = "(", wave = "P"))
#'
#' @seealso [get_windows()], [label_waves()]
#'
#' @export
by_rhythm <- function(
  rhythm = "sinus",
  onset = NULL,
  offset = NULL,
  reference = NULL,
  channel = NULL,
  adjust_sample_indices = TRUE
) {
  if (!valid_scalar_string(rhythm)) {
    stop("`rhythm` must be a single non-empty string")
  }

  # Sensible, overridable defaults for sinus rhythm. Wave identity is recovered
  # positionally (see `label_waves()`), so a P-onset -> T-offset beat with the
  # QRS as reference can be windowed with just a guiding channel.
  if (rhythm == "sinus") {
    if (is.null(onset)) {
      onset <- list(type = "(", wave = "P")
    }
    if (is.null(offset)) {
      offset <- list(type = ")", wave = "T")
    }
    if (is.null(reference)) {
      reference <- list(type = "N")
    }
  }

  if (!valid_feature_criteria(onset)) {
    stop("`onset` must be a non-empty named list of annotation criteria")
  }
  if (!valid_feature_criteria(offset)) {
    stop("`offset` must be a non-empty named list of annotation criteria")
  }
  if (!is.null(reference) && !valid_feature_criteria(reference)) {
    stop("`reference` must be NULL or a non-empty named list of annotation criteria")
  }
  window_strategy(
    method = "rhythm",
    params = list(
      rhythm = rhythm,
      onset = onset,
      offset = offset,
      reference = reference,
      channel = valid_channel(channel),
      adjust_sample_indices = valid_flag(
        adjust_sample_indices,
        "adjust_sample_indices"
      )
    )
  )
}

# Beat strategy ----------------------------------------------------------------

#' Window a fixed span around each beat
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Builds a [window_strategy] that cuts the same span of signal around every
#' occurrence of a fiducial, by default the QRS peak. Every window is the same
#' length by construction, which is what [by_rhythm()] cannot promise.
#'
#' @details That guarantee is what a representative beat needs. Reducing ragged
#'   windows means padding them onto a common grid first, and a padded sample is
#'   a fabricated one: it pulls [median_window()] toward whatever the padding
#'   says, wherever the windows do not all reach. Cutting a fixed span out of the
#'   continuous recording leaves nothing to pad, which is how the standard
#'   representative beat is derived (Kligfield et al. 2007).
#'
#'   Beats too near either end of the record for the full span to be cut are
#'   dropped rather than truncated, since a short window gives up the guarantee
#'   the strategy exists for. How many were dropped is reported.
#'
#'   The span is given in milliseconds so that one strategy can be reused across
#'   records of differing sampling frequency.
#'
#' @param before,after Milliseconds of signal kept before and after the fiducial.
#'   The defaults span a full PQRST at ordinary rates.
#' @param feature The fiducial each window is built around, given as a
#'   `character` type symbol (default `"N"`, the QRS peak) or a named list of
#'   annotation criteria. Resolved the same way as a [by_rhythm()] criterion.
#' @param channel An optional channel number guiding multi-lead annotations,
#'   as in [by_rhythm()].
#' @param adjust_sample_indices Logical, whether to rebase sample indices in the
#'   returned windows to be zero-based and relative to the window start.
#'
#' @return A [window_strategy] object.
#'
#' @references
#'
#' Kligfield P, Gettes LS, Bailey JJ, et al. Recommendations for the
#' standardization and interpretation of the electrocardiogram: part I.
#' *Circulation*. 2007;115(10):1306-1324.
#' \doi{10.1161/CIRCULATIONAHA.106.180200}
#'
#' @examples
#' # A fixed PQRST window around every QRS peak
#' by_beat()
#'
#' # Tight around the P wave instead, guided by lead II
#' by_beat(before = 200, after = 200, feature = "p", channel = 2)
#'
#' @seealso [get_windows()], [median_window()], [by_rhythm()]
#'
#' @export
by_beat <- function(
  before = 300,
  after = 500,
  feature = "N",
  channel = NULL,
  adjust_sample_indices = TRUE
) {
  for (span in list(before = before, after = after)) {
    if (length(span) != 1L || !is.numeric(span) || !is.finite(span) || span < 0) {
      stop("`before` and `after` must each be one non-negative number of milliseconds")
    }
  }
  if (!valid_scalar_string(feature) && !valid_feature_criteria(feature)) {
    stop("`feature` must be a type symbol or a named list of annotation criteria")
  }

  window_strategy(
    method = "beat",
    params = list(
      before = as.numeric(before),
      after = as.numeric(after),
      feature = feature,
      channel = valid_channel(channel),
      adjust_sample_indices = valid_flag(adjust_sample_indices, "adjust_sample_indices")
    )
  )
}

# Beat engine ------------------------------------------------------------------

#' Fixed-span windowing engine
#'
#' @description Performs the search described by a [by_beat()] strategy. Called
#'   by [get_windows()]; returns a bare list, which the caller wraps.
#'
#' @param object An `EGM` object.
#' @param params The resolved parameter list from a beat [window_strategy].
#'
#' @return A list of `EGM` objects, all of the same length.
#'
#' @keywords internal
windows_by_beat <- function(object, params) {
  record <- attributes(object$header)$record_line
  ann <- get_single_annotation(object)

  before <- ceiling(params$before / 1000 * record$frequency)
  after <- ceiling(params$after / 1000 * record$frequency)

  centres <- locate_features(ann, params$feature, params$channel)
  if (length(centres) == 0) {
    warning("No occurrences of the requested feature were found")
    return(list())
  }

  # A beat without room for the full span is dropped, not truncated
  limits <- range(object$signal$sample)
  whole <- centres - before >= limits[1] & centres + after <= limits[2]
  if (any(!whole)) {
    message(
      sum(!whole),
      " of ",
      length(centres),
      " beats lie too near the ends of the record for a ",
      params$before + params$after,
      " ms window and were dropped"
    )
  }
  centres <- centres[whole]

  if (length(centres) == 0) {
    warning("No beat had room for the full window")
    return(list())
  }

  lapply(seq_along(centres), function(i) {
    cut_window(
      object,
      ann,
      onset = centres[i] - before,
      offset = centres[i] + after,
      name = paste0("beat", i),
      info = paste0(
        "beat window ", i, " centred on ", centres[i],
        " (-", params$before, "/+", params$after, " ms)"
      ),
      adjust_sample_indices = params$adjust_sample_indices
    )
  })
}

# Extraction entry point -------------------------------------------------------

#' Extract windows of signal from a record
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Searches a record for every span of signal that satisfies a windowing
#' strategy, and returns each span as its own `EGM`. This is how a whole study is
#' broken into the analysable segments - individual beats, mapping windows,
#' intervals of interest - that the rest of the window functions operate on.
#'
#' @details The strategy may be given as a bare name for its defaults, or built
#'   explicitly with a `by_*()` constructor when its arguments need setting:
#'
#'   ```r
#'   get_windows(ecg, by = "rhythm")
#'   get_windows(ecg, by = by_rhythm(channel = 2))
#'   ```
#'
#'   Both forms route through the same constructor, so both are validated the
#'   same way. Strategy arguments belong to the constructor rather than to this
#'   function, which is what allows a mistyped argument to be reported instead of
#'   silently ignored.
#'
#'   Each returned window is a complete `EGM`: its own header (named
#'   `<record>_<strategy><n>`, with an info string recording the source sample
#'   range), the signal for every channel, and the annotations falling inside it.
#'   Windows are generally *not* the same length, since they follow the signal
#'   rather than a fixed duration; see [pad_window()], [normalize_window()], and
#'   [warp_window()] for the ways to bring them onto a common basis.
#'
#' @param object An `EGM` object, including header, signal, and annotation
#'   information.
#' @param by A [window_strategy] object, or the name of one as a string.
#'   Defaults to `"rhythm"`.
#'
#' @return A [windows] object: a collection of `EGM` objects, one per window.
#'
#' @examples
#' \dontrun{
#' ecg <- read_wfdb("ecg", test_path(), "ecgpuwave")
#'
#' # Sinus beats, using the strategy defaults
#' beats <- get_windows(ecg, by = "rhythm")
#'
#' # Guided by a single lead, then collapsed to a median beat
#' get_windows(ecg, by = by_rhythm(channel = 2)) |>
#'   median_window(align_feature = "N", channel_criteria = 2)
#' }
#'
#' @seealso [by_rhythm()] for the available strategy, [pad_window()],
#'   [normalize_window()] and [warp_window()] to bring windows onto a common
#'   length, and [change_frequency()] to harmonise sampling rates either before
#'   windowing or on the returned collection.
#'
#' @export
get_windows <- function(object, by = "rhythm") {
  stopifnot(
    "Requires object of <EGM> class for evaluation" = inherits(object, 'EGM')
  )

  strategy <- as_window_strategy(by)
  engine <- window_registry()[[strategy@method]]$engine

  windows <- engine(object, strategy@params)

  # Extract source record name
  source_record <-
    if (!is.null(object$header$record_name)) {
      object$header$record_name
    } else if (!is.null(attributes(object$header)$record_line$record_name)) {
      attributes(object$header)$record_line$record_name
    } else {
      "unknown"
    }

  new_windows(
    lapply(windows, keep_ECG, windows = list(object)),
    method = strategy@method,
    source_record = source_record
  )
}

# Rhythm engine ----------------------------------------------------------------

#' Rhythm windowing engine
#'
#' @description Performs the search described by a [by_rhythm()] strategy. Called
#'   by [get_windows()]; returns a bare list, which the caller wraps.
#'
#' @param object An `EGM` object.
#' @param params The resolved parameter list from a rhythm [window_strategy].
#'
#' @return A list of `EGM` objects.
#'
#' @keywords internal
windows_by_rhythm <- function(object, params) {
  rhythm <- params$rhythm
  onset_criteria <- params$onset
  offset_criteria <- params$offset
  reference_criteria <- params$reference
  channel_criteria <- params$channel
  adjust_sample_indices <- params$adjust_sample_indices

  ann <- get_single_annotation(object)

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
        "Specify `channel` in the strategy to select a guiding lead."
      )
    }
  }

  # Boundary detection is a strict equality match on every named criterion.
  # This is deliberately narrower than `locate_features()`, which additionally
  # resolves channel fallbacks and range criteria: boundaries are matched against
  # an already channel-restricted table, so a fallback here would silently widen
  # the search back out to other leads.
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
    reference <- NA_integer_
    if (!is.null(reference_points)) {
      refs_between <- reference_samples[
        reference_samples > onset &
          reference_samples < offset
      ]
      if (length(refs_between) == 0) {
        # No reference point between onset and offset, skip this window
        next
      }
      reference <- refs_between[1]
    }

    # Apply rhythm-specific validation
    if (rhythm == "sinus") {
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

    window_count <- window_count + 1

    info <- paste0(
      rhythm, " window ", window_count,
      ", onset: ", onset, ", offset: ", offset
    )
    if (!is.na(reference)) {
      info <- paste0(info, ", reference: ", reference)
    }

    windows[[window_count]] <- cut_window(
      object,
      ann,
      onset = onset,
      offset = offset,
      name = paste0(rhythm, window_count),
      info = info,
      adjust_sample_indices = adjust_sample_indices
    )
  }

  if (length(windows) == 0) {
    warning(
      "No complete ",
      rhythm,
      " windows found with the specified criteria"
    )
    return(list())
  }

  # Return list of windows
  windows
}

# Shared cutting ---------------------------------------------------------------

#' Cut one window out of a record
#'
#' @description Takes the span between two sample positions and returns it as a
#'   self-contained `EGM`: the signal, a header renamed and re-sized for the
#'   span, and the annotations falling inside it. Every windowing engine ends
#'   here, which is what keeps their output identical in everything but where
#'   they cut.
#'
#' @param object The source `EGM`.
#' @param annotation The source annotation table.
#' @param onset,offset Inclusive sample bounds of the window.
#' @param name Suffix for the window's record name, e.g. `"sinus3"`.
#' @param info A string describing the cut, appended to the header info strings.
#' @param adjust_sample_indices Whether to rebase sample indices to zero. A
#'   window is a new WFDB record, so it normally starts its own count; callers
#'   retain the source record's absolute indices by disabling this.
#'
#' @return An `EGM` object.
#'
#' @keywords internal
cut_window <- function(
  object,
  annotation,
  onset,
  offset,
  name,
  info,
  adjust_sample_indices = TRUE
) {
  hea <- object$header
  record <- attributes(hea)$record_line

  signal <- object$signal[object$signal$sample %between% c(onset, offset), ]
  fiducials <- annotation[annotation$sample %between% c(onset, offset), ]
  start_time <- record$start_time

  if (adjust_sample_indices) {
    signal$sample <- signal$sample - onset
    fiducials$sample <- fiducials$sample - onset
    if (
      inherits(start_time, "POSIXt") &&
        length(start_time) == 1L &&
        !is.na(start_time)
    ) {
      start_time <- start_time + onset / record$frequency
    }
  }

  EGM(
    signal = signal,
    header = header_table(
      record_name = paste0(record$record_name, "_", name),
      number_of_channels = record$number_of_channels,
      frequency = record$frequency,
      samples = nrow(signal),
      start_time = start_time,
      storage_format = hea$storage_format,
      ADC_gain = hea$ADC_gain,
      ADC_baseline = hea$ADC_baseline,
      ADC_units = hea$ADC_units,
      ADC_zero = hea$ADC_zero,
      ADC_resolution = hea$ADC_resolution,
      label = hea$label,
      info_strings = c(attributes(hea)$info_strings, window_info = info)
    ),
    annotation = fiducials
  )
}

# Wave labelling ---------------------------------------------------------------

#' Label annotation waves positionally
#'
#' @description Adds a working `wave` column (one of `"P"`, `"QRS"`, `"T"`, or
#'   `NA`) to an annotation table by recovering wave identity from the peak
#'   symbol enclosed within each `(`/`)` waveform bracket. This is used by
#'   [by_rhythm()] to isolate P-onset -> T-offset beats even when the
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
