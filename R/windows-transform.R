# Window transforms ------------------------------------------------------------

# Windows come out of `get_windows()` ragged: they follow the signal, so a beat
# is however many samples it happened to take. Almost everything downstream -
# averaging, stacking into a matrix, plotting overlaid - needs them rectangular.
# There are three ways to get there, and they differ in what they destroy:
#
#   pad_window()       non-destructive. Real samples untouched, flat padding
#                      added at the edges. Anchors one fiducial.
#   normalize_window() warps the time axis so the two borders (or a single
#                      aligned feature) land on a common grid.
#   warp_window()      warps piecewise between many landmarks at once.
#
# `median_window()` is the reduction they all feed: many windows in, one
# representative beat out.

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
#' @param x A `windows` object (or list of `EGM` objects) to pad.
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
#'   `align_feature` in multi-lead annotation tables. Mirrors the `channel`
#'   argument of [by_rhythm()].
#' @param pad_value The value used for padding. Defaults to `0`.
#' @param preserve_class Logical. If TRUE (default), returns a `windows` object;
#'   if FALSE, returns a plain list of `EGM` objects.
#' @param ... Additional arguments (currently unused).
#'
#' @return A `windows` object (or list) of padded `EGM` objects, all sharing the
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
      return(rewrap_windows(list(), x, "padded"))
    }
    return(list())
  }

  # Resolve a millisecond target to samples using the first window's frequency
  if (is.null(target_samples) && !is.null(target_ms)) {
    freq <- stats::frequency(windows[[1]])
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
    return(rewrap_windows(padded, x, "padded"))
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
#' Reduces a `windows` collection to a single representative `EGM` by taking the
#' element-wise median across all windows, lead by lead. Given an ECG windowed
#' into its individual beats, this returns the "median beat" - a robust template
#' that suppresses beat-to-beat noise and ectopy while preserving the dominant
#' morphology.
#'
#' @details The median is computed per sample and per lead, so every window must
#'   share the same length and the same set of leads. Beats windowed from raw
#'   rhythm are generally *not* the same length (see [get_windows()]), so this
#'   function offers to align them first: when `align_feature` is supplied and
#'   the windows differ in length, they are padded and anchored on that feature
#'   via [pad_window()] before the median is taken. If the windows differ in
#'   length and no `align_feature` is given, an error is raised pointing to
#'   [pad_window()] or [normalize_window()] so the caller chooses the alignment
#'   explicitly.
#'
#'   The returned beat carries the fiducials that produced it, each placed at the
#'   median of its own positions across the aligned windows. Annotations are
#'   matched between windows by channel, type, and how many of that pair came
#'   before them in their own window, so the first QRS onset of one beat lines up
#'   with the first of every other. A fiducial that most windows do not carry is
#'   dropped, since it is not part of what the median describes. The header is
#'   named `<source>_median` and carries an info string recording how many
#'   windows it came from.
#'
#' @param x A `windows` object (or list of `EGM` objects) to reduce.
#' @param align_feature Optional feature used to pad-align windows of unequal
#'   length before averaging, given as a `character` type symbol (e.g. "N", the
#'   QRS peak) or a named list of annotation criteria. If `NULL` (default), the
#'   windows must already share a common length.
#' @param channel_criteria Optional guiding channel used when locating
#'   `align_feature`. Mirrors the `channel` argument of [by_rhythm()].
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

  # Header from the first window, renamed and re-sized
  median_header <- data.table::copy(windows[[1]]$header)
  rl <- attributes(median_header)$record_line
  rl$samples <- n
  src <- window_source_record(x)
  base_name <- if (length(src) > 0 && nzchar(src[1])) src[1] else "window"
  rl$record_name <- paste0(base_name, "_median")
  attr(median_header, "record_line") <- rl

  # `window_info` names a single source window, which this no longer is
  info <- attributes(median_header)$info_strings
  info$window_info <- NULL
  attr(median_header, "info_strings") <- c(
    info,
    list(median_info = paste0(
      "median beat of ", length(windows), " windows; ",
      "each annotation is the median position of that fiducial"
    ))
  )

  new_EGM(
    signal = median_signal,
    header = median_header,
    annotation = median_annotations(windows, rl$frequency)
  )
}

#' Fiducials of a median beat
#'
#' @description Collapses the annotations of a set of aligned windows the same
#'   way the signal itself is collapsed: each fiducial is placed at the median of
#'   its positions.
#'
#' @details Annotations are matched between windows by channel, type, and how
#'   many of that pair came before them within their own window, so the first QRS
#'   onset of one beat lines up with the first of every other rather than with
#'   whichever bracket happens to sort alongside it. A fiducial carried by no more
#'   than half the windows is dropped; it is not part of what the median beat
#'   describes.
#'
#' @param windows A list of aligned `EGM` objects sharing a common length.
#' @param frequency Sampling frequency in Hz, used to fill annotation times.
#'
#' @return An `annotation_table`, empty when the windows carry no annotations.
#'
#' @keywords internal
median_annotations <- function(windows, frequency) {
  fiducials <- data.table::rbindlist(lapply(windows, function(w) {
    ann <- as.data.frame(get_single_annotation(w))
    if (nrow(ann) == 0) {
      return(NULL)
    }
    ann <- ann[order(ann$sample), , drop = FALSE]

    # Key: the channel and type, plus the rank of this annotation among others
    # of that pair in the same window
    pair <- paste(ann$channel, ann$type)
    ann$key <- paste(pair, stats::ave(seq_along(pair), pair, FUN = seq_along))
    ann
  }))

  if (nrow(fiducials) == 0) {
    return(annotation_table())
  }

  carried <- table(fiducials$key)
  kept <- fiducials[
    fiducials$key %in% names(carried)[carried > length(windows) / 2],
  ]
  positions <- tapply(kept$sample, kept$key, stats::median)

  # One row per fiducial, taking the non-positional fields from its first window
  beat <- kept[!duplicated(kept$key), ]
  beat$sample <- as.integer(round(positions[beat$key]))
  beat <- beat[order(beat$sample), ]

  annotation_table(
    annotator = attr(get_single_annotation(windows[[1]]), "annotator"),
    sample = beat$sample,
    frequency = frequency,
    type = beat$type,
    subtype = beat$subtype,
    channel = beat$channel,
    number = beat$number,
    aux = beat$aux
  )
}

# Time normalization -----------------------------------------------------------

#' Time-normalize windows to a fixed length
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Resamples each window so that every window spans the same number of samples.
#' Without `align_feature` this stretches or compresses each window as a whole,
#' mapping its left and right borders (e.g. P-onset and T-offset for a sinus
#' beat) onto a common `[0, target_samples)` grid. With `align_feature` the
#' windows are instead resampled at native resolution around that fiducial, so
#' the feature lands at the centre of every output window and the true morphology
#' timing is preserved.
#'
#' This is a *destructive* standardization - in the border-to-border mode the
#' time axis is warped, so absolute intervals are no longer comparable - but it
#' is a fast, simple way to bring ragged beats onto a shared basis for averaging
#' or matrix operations.
#'
#' @details For alignment on a fiducial without warping the time axis at all, use
#'   [pad_window()] instead; to align several landmarks at once, use
#'   [warp_window()]; to change the physical sampling rate while preserving
#'   duration, use [change_frequency()].
#'
#'   When `align_feature` is given but not found in a window, that window falls
#'   back to border-to-border resampling with a warning.
#'
#' @param x A `windows` object (or list of `EGM` objects) to normalize.
#' @param target_samples Desired output length in samples. Default 500.
#' @param target_ms Alternative target length in milliseconds, converted using
#'   the first window's sampling frequency. Used only when `target_samples` is
#'   `NULL`.
#' @param interpolation_method Interpolation method for the stretch. One of
#'   "linear" (default), "spline", or "step".
#' @param align_feature Optional feature to centre each window on, given as a
#'   `character` type symbol (e.g. "N", the QRS peak) or a named list of
#'   annotation criteria. `NULL` (default) resamples border-to-border.
#' @param channel_criteria Optional guiding channel used when locating
#'   `align_feature`. Multi-lead annotation files (e.g. an `ecgpuwave`-style run
#'   per lead) carry one fiducial per lead at slightly different samples, so a
#'   bare `align_feature = "N"` would centre on whichever lead sorts first.
#'   Mirrors the `channel` argument of [by_rhythm()]. Ignored when
#'   `align_feature` is `NULL` or already specifies a `channel`.
#' @param preserve_amplitude Logical. If TRUE, rescales each lead back to its
#'   original amplitude range after interpolation. Defaults to FALSE, since a
#'   pure time stretch should leave amplitudes to the interpolation.
#' @param preserve_class Logical. If TRUE (default), returns a `windows` object;
#'   if FALSE, returns a plain list of `EGM` objects.
#' @param ... Additional arguments (currently unused).
#'
#' @return A `windows` object (or list) of time-normalized `EGM` objects, each
#'   exactly `target_samples` long. Each carries a resampled `signal_table`, the
#'   window's own `header_table` (with `samples` updated and the per-beat
#'   record/file name preserved), and its annotations remapped onto the resampled
#'   time base.
#'
#' @examples
#' \dontrun{
#' ecg <- read_wfdb("ecg", test_path(), "ecgpuwave")
#' beats <- get_windows(ecg, by = by_rhythm(channel = 2))
#'
#' # Stretch every beat border-to-border onto 500 samples
#' normalize_window(beats, target_samples = 500)
#'
#' # Or 500 milliseconds, using the record's sampling frequency
#' normalize_window(beats, target_samples = NULL, target_ms = 500)
#'
#' # Centre each beat on its QRS instead of stretching the borders
#' normalize_window(beats, align_feature = "N", channel_criteria = 2)
#' }
#'
#' @export
normalize_window <- function(
  x,
  target_samples = 500L,
  target_ms = NULL,
  interpolation_method = c("linear", "spline", "step"),
  align_feature = NULL,
  channel_criteria = NULL,
  preserve_amplitude = FALSE,
  preserve_class = TRUE,
  ...
) {
  windows <- as_window_list(x)
  interpolation_method <- match.arg(interpolation_method)

  normalized <- time_normalize_windows(
    windows,
    target_samples = target_samples,
    target_ms = target_ms,
    interpolation_method = interpolation_method,
    align_feature = align_feature,
    channel_criteria = channel_criteria,
    preserve_amplitude = preserve_amplitude
  )

  if (preserve_class) {
    return(rewrap_windows(normalized, x, "normalized"))
  }
  normalized
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
        original_indices <- signal_data$sample

        new_samples <- feature_idx + (output_samples - center_point)

        # Proceed with interpolation
        for (col in signal_cols) {
          col_name <- names(signal_data)[col]
          original_values <- signal_data[[col]]

          resampled_data[[col_name]] <- interpolate_signal(
            original_indices,
            original_values,
            new_samples,
            interpolation_method
          )
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
#' @param x A `windows` object (or list of `EGM` objects) to warp.
#' @param template A [template] created manually with [template()] or learned
#'   with [learn_template()]. Plain lists are not accepted.
#' @param interpolation_method Interpolation method for the warp. One of "linear"
#'   (default), "spline", or "step".
#' @param channel_criteria Optional fallback guiding channel used when locating a
#'   landmark whose own spec does not name a channel. A landmark's own channel
#'   always takes precedence.
#' @param preserve_amplitude Logical. If TRUE, rescales each lead back to its
#'   original amplitude range after warping. Defaults to FALSE.
#' @param preserve_class Logical. If TRUE (default), returns a `windows` object;
#'   if FALSE, returns a plain list of `EGM` objects.
#' @param missing What to do when a required landmark is absent: warp with the
#'   remaining landmarks, drop that window, or error.
#' @param ambiguous What to do when a landmark matches multiple annotations:
#'   error, use the first, or drop that window.
#' @param order_policy What to do when landmarks are crossed or duplicated:
#'   error or drop that window.
#' @param ... Additional arguments (currently unused).
#'
#' @return A `windows` object (or list) of landmark-warped `EGM` objects, each
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
      return(rewrap_windows(list(), x, "warped"))
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
    out <- rewrap_windows(warped, x, "warped")
    attr(out, "warp_mappings") <- mappings
    return(out)
  }
  warped
}
