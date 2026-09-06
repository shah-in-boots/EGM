# Sampling frequency -----------------------------------------------------------

# The sampling rate is a property of a record, fixed at acquisition and recorded
# in the header. Everything downstream of a read - windowing, padding, warping,
# template learning - is expressed in *samples*, and the rate is the only thing
# that ties those sample counts back to physical time. This file holds the two
# operations that treat the rate as a first-class property: reading it
# (`frequency()`) and changing it (`change_frequency()`).
#
# Changing the rate is deliberately a standalone step rather than an argument
# buried in the functions that happen to need a common rate. Records arrive at
# whatever rate their vendor chose, and harmonising them is a pipeline stage:
#
#   read_wfdb(...) |> change_frequency(from = 250, to = 500) |> get_windows()

#' Sampling frequency of signal data
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Reports the sampling rate, in Hz, that a record was acquired at. This is read
#' from the header rather than inferred from the signal, and is the value that
#' converts sample indices into physical time. Use [change_frequency()] to
#' change it.
#'
#' @details These are methods for the [stats::frequency()] generic, defined for
#'   every object that carries a rate: an `EGM` (and so an [ECG]), a bare
#'   `header_table`, and a `windows` collection. A `header_table` needs its own
#'   method because the default one answers `1` for any object without a `tsp`
#'   attribute, and a 500 Hz record reported as 1 Hz is wrong in a way that
#'   nothing downstream can catch.
#'
#'   For a single record the rate is a scalar. A `windows` collection may in
#'   principle mix rates - windows from different source records can be
#'   concatenated with [c.windows()] - so its method returns the *distinct* rates
#'   present. A result of length one therefore means the collection is already
#'   harmonised, and anything longer means the windows are not directly
#'   comparable.
#'
#'   A record whose header carries no usable rate is an error rather than an
#'   `NA`. There is no arithmetic that recovers from a missing sampling rate: it
#'   propagates into every interval, every heart rate, and every duration, while
#'   leaving the analyses that do not divide by it looking healthy. Repair the
#'   header, or state the rate with [change_frequency()].
#'
#' @param x An `EGM`, `header_table`, or `windows` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return A `numeric` sampling rate in Hz; for `windows` objects a vector of the
#'   distinct rates present (empty for an empty collection).
#'
#' @examples
#' \dontrun{
#' ecg <- read_wfdb("ecg", test_path(), "ecgpuwave")
#' frequency(ecg)
#'
#' # The header answers for itself
#' frequency(ecg$header)
#'
#' # A harmonised collection reports a single rate
#' frequency(get_windows(ecg))
#' }
#'
#' @seealso [change_frequency()]
#'
#' @name frequency
NULL

#' @rdname frequency
#' @export
frequency.EGM <- function(x, ...) {
  require_frequency(frequency_of(x), "record")
}

#' @rdname frequency
#' @export
frequency.header_table <- function(x, ...) {
  require_frequency(frequency_of(x), "header")
}

#' @rdname frequency
#' @export
frequency.windows <- function(x, ...) {
  if (length(x) == 0) {
    return(numeric())
  }
  unique(vapply(x, stats::frequency, numeric(1)))
}

#' Read the recorded sampling rate without demanding one
#'
#' @description The accessor the package uses internally. It reports `NA_real_`
#'   for a record that carries no usable rate, which the exported [frequency()]
#'   methods then refuse; the two are separated because a handful of operations
#'   - [change_frequency()], most of all - are defined precisely on records whose
#'   header is missing or wrong.
#'
#' @param x An `EGM`, `header_table`, or anything carrying a header.
#'
#' @return A single `numeric` rate in Hz, or `NA_real_`.
#'
#' @keywords internal
frequency_of <- function(x) {
  header <- if (is_header_table(x)) x else x$header
  freq <- attributes(header)$record_line$frequency
  if (is.null(freq) || length(freq) == 0) {
    return(NA_real_)
  }
  freq <- suppressWarnings(as.numeric(freq[1]))
  if (is.na(freq) || !is.finite(freq) || freq <= 0) {
    return(NA_real_)
  }
  freq
}

#' Refuse a missing sampling rate
#' @keywords internal
require_frequency <- function(freq, what = "record") {
  if (is.na(freq)) {
    stop(
      "This ",
      what,
      " carries no usable sampling frequency. Every interval, rate, and ",
      "duration is derived from it, so there is nothing sensible to return; ",
      "repair the header, or state the rate with change_frequency().",
      call. = FALSE
    )
  }
  freq
}

# Changing the sampling frequency ----------------------------------------------

#' Change the sampling frequency of signal data
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Converts signal data from one sampling frequency to another, up- or
#' down-sampling as needed while preserving the recording's duration. This
#' harmonises records captured at different rates (e.g. mixing 250 Hz and 500 Hz
#' ECGs) so that downstream operations such as [median_window()] or
#' [normalize_window()] compare like with like, and lets analyses that assume
#' a fixed rate be fed at that rate.
#'
#' @details Only the target rate (`to`) has to be stated. The source rate is
#'   taken from the object's own header, which already carries it; state `from`
#'   as well when you want it checked, and a disagreement with the recorded rate
#'   is an error. A bare `numeric` lead has no header, so there `from` is
#'   required - the sampling rate is the one property that cannot be recovered
#'   from the samples themselves. Read the recorded rate with [frequency()].
#'
#'   The change is duration-preserving: a 0.8 second beat stays 0.8 seconds long,
#'   but its sample count scales with the frequency ratio. For `EGM` input the
#'   signal values are recomputed on the new grid, the header frequency and
#'   sample count are updated, and every annotator's sample indices are rescaled
#'   onto the new grid. Annotation `time` strings are left alone, because
#'   absolute time is unchanged by definition.
#'
#'   **The annotations carried by the object are rescaled; a separate copy of
#'   them is not.** Annotations read separately from disk, or held from before
#'   the conversion, remain on the original grid, and mixing the two halves every
#'   interval measured from them (or doubles it). Take the annotations from the
#'   converted object with [get_annotation()] rather than re-reading them:
#'
#'   ```r
#'   slow <- change_frequency(ecg, to = 250)
#'   get_annotation(slow)      # rescaled, on the 250 Hz grid
#'   read_annotation(...)      # still on the 500 Hz grid
#'   ```
#'
#'   It is written as a pipe stage rather than an argument of the functions that
#'   need it, so the rate change is explicit and happens exactly once:
#'
#'   ```r
#'   read_wfdb("ecg", ".", "ecgpuwave") |>
#'     change_frequency(to = 250) |>
#'     get_windows(by = "rhythm")
#'   ```
#'
#'   Four `method` choices are offered, covering the standard approaches:
#'
#'   * `linear` (default) - piecewise-linear interpolation onto the new grid.
#'     Fast, monotone, and free of overshoot, which matters when the sharp edges
#'     of a QRS complex would otherwise ring.
#'   * `spline` - natural cubic spline interpolation. Smoother through curved
#'     segments such as T waves, at the cost of possible overshoot at steep
#'     transitions.
#'   * `step` - nearest-preceding sample (piecewise constant). Non-interpolating,
#'     so it never invents intermediate voltages; mainly useful for step-like or
#'     categorical channels such as pacing markers.
#'   * `polyphase` - anti-aliased polyphase FIR resampling via
#'     [signal::resample()], the conventional DSP approach. The rate ratio is
#'     approximated by a rational `p/q`, so it is most natural for related rates
#'     (500 to 250 Hz, 250 to 1000 Hz) and applies its own anti-alias filter.
#'
#'   Down-sampling folds any energy above the new Nyquist frequency back into the
#'   retained band. The interpolating methods do not guard against this on their
#'   own, so `anti_alias = TRUE` (the default) low-pass filters each lead with a
#'   zero-phase Butterworth filter before the grid change. Records too short to
#'   filter stably are left unfiltered, and up-sampling needs no filter at all.
#'   The `polyphase` method ignores this argument because its own FIR stage
#'   already performs the equivalent filtering.
#'
#' @param x The signal data to convert. Either an `EGM` object, a `windows`
#'   object, a plain list of `EGM` objects, or a bare `numeric` vector holding a
#'   single lead.
#'
#' @param to The target sampling rate in Hz.
#'
#' @param from The rate the data is currently sampled at, in Hz. Defaults to the
#'   rate the object's own header declares, and must agree with it when given.
#'   Required for a bare `numeric` lead, which carries no header.
#'
#' @param method The resampling method, one of `"linear"` (default), `"spline"`,
#'   `"step"`, or `"polyphase"`. See details.
#'
#' @param anti_alias Logical. If `TRUE` (default), applies a low-pass filter
#'   before down-sampling with an interpolating method. Ignored when
#'   up-sampling, and by the `polyphase` method.
#'
#' @param preserve_class Logical, for `windows` input only. If `TRUE` (default),
#'   returns a `windows` object; if `FALSE`, a plain list of `EGM` objects.
#'
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of the same kind as `x`, sampled at `to` Hz.
#'
#' @examples
#' \dontrun{
#' ecg <- read_wfdb("ecg", test_path(), "ecgpuwave")
#'
#' # Down-sample the whole record to 250 Hz; the source rate is on the header
#' slow <- change_frequency(ecg, 250)
#' frequency(slow)
#'
#' # State `from` as well to assert what you believe you were given
#' change_frequency(ecg, to = 250, from = 500)
#'
#' # Or harmonise a set of windowed beats after the fact
#' beats <- get_windows(ecg, by = "rhythm")
#' change_frequency(beats, to = 1000, method = "polyphase")
#'
#' # A bare lead works too, for signal-processing pipelines. It carries no
#' # header, so `from` is required
#' change_frequency(ecg$signal$II, to = 1000, from = 500)
#' }
#'
#' @seealso [frequency()], [normalize_window()] to change the number of samples
#'   without changing the physical rate.
#'
#' @export
change_frequency <- function(
  x,
  to,
  from = NULL,
  method = c("linear", "spline", "step", "polyphase"),
  anti_alias = TRUE,
  preserve_class = TRUE,
  ...
) {
  method <- match.arg(method)
  to <- validate_frequency(to, "to")

  # A bare lead has no header to consult, so `from` is the only source of truth
  if (is.numeric(x)) {
    if (!is.null(dim(x))) {
      stop("`x` must be a plain `numeric` vector, not an array or matrix")
    }
    if (is.null(from)) {
      stop(
        "`from` is required for a bare `numeric` lead, which carries no ",
        "header to read the current rate from"
      )
    }
    from <- validate_frequency(from, "from")
    return(resample_values(as.numeric(x), ratio_of(from, to), method, anti_alias))
  }

  # `EGM` inherits from list, so it has to be tested before the list branch
  if (is_EGM(x)) {
    from <- resolve_source_frequency(frequency_of(x), from)
    return(resample_egm(x, ratio_of(from, to), to, method, anti_alias))
  }

  # `data.frame` is a list too, but is a table of samples rather than a
  # collection of records, so it falls through to the error below
  if (is_window_set(x) || (is.list(x) && !is.data.frame(x))) {
    windows <- as_window_list(x)
    if (length(windows) == 0) {
      if (is_window_set(x) && preserve_class) {
        return(rewrap_windows(list(), x, "resampled"))
      }
      return(list())
    }
    from <- resolve_source_frequency(
      unique(vapply(windows, frequency_of, numeric(1))),
      from
    )
    ratio <- ratio_of(from, to)
    converted <- lapply(
      windows,
      resample_egm,
      ratio = ratio,
      to = to,
      method = method,
      anti_alias = anti_alias
    )
    if (is_window_set(x) && preserve_class) {
      return(rewrap_windows(converted, x, "resampled"))
    }
    return(converted)
  }

  stop(
    "`x` must be an `EGM` object, a `windows` object, a list of `EGM` ",
    "objects, or a `numeric` vector"
  )
}

# Engines ----------------------------------------------------------------------

#' Resample a single lead onto a new sampling grid
#'
#' @description The shared engine behind [change_frequency()]. It works purely in
#'   terms of the frequency ratio, so it is indifferent to whether the lead came
#'   from an `EGM` or was handed over as a bare vector.
#'
#' @details The endpoints stay fixed: `(n - 1)` source steps become
#'   `(n - 1) * ratio` output steps, and each output index is projected back to a
#'   real source position so the interpolation samples true morphology rather
#'   than a warped copy. The `polyphase` method instead delegates to
#'   [signal::resample()], whose own output length is then authoritative.
#'
#' @param values A `numeric` lead.
#' @param ratio The frequency ratio (target / source).
#' @param method One of "linear", "spline", "step", or "polyphase".
#' @param anti_alias Whether to low-pass filter before down-sampling. Ignored by
#'   the `polyphase` method, which filters internally.
#'
#' @return The resampled lead.
#'
#' @keywords internal
resample_values <- function(values, ratio, method, anti_alias) {
  n <- length(values)
  if (n < 2L) {
    return(values)
  }

  if (method == "polyphase") {
    pq <- rational_ratio(ratio)
    return(as.numeric(signal::resample(values, pq$p, pq$q)))
  }

  if (anti_alias) {
    values <- lowpass_for_decimation(values, ratio)
  }
  n_new <- resampled_length(n, ratio)
  interpolate_signal(
    seq_len(n) - 1L,
    values,
    (seq_len(n_new) - 1L) / ratio,
    method
  )
}

#' Output length of a resampled lead
#' @keywords internal
resampled_length <- function(n, ratio) {
  max(2L, as.integer(round((n - 1L) * ratio)) + 1L)
}

#' Rebuild an `EGM` at a new sampling frequency
#'
#' @description Applies [resample_values()] to every lead, then brings the rest
#'   of the object along: the header's rate and sample count, and each
#'   annotator's sample indices.
#'
#' @param x An `EGM` object.
#' @param ratio The frequency ratio (target / source).
#' @param to The target rate in Hz, written into the header.
#' @param method,anti_alias Passed to [resample_values()].
#'
#' @return The converted `EGM` object.
#'
#' @keywords internal
resample_EGM <- function(x, ratio, to, method, anti_alias) {
  # A phase-warped object no longer has a physical sampling interval, so its
  # `frequency` field is provenance rather than a rate (see warp_window()).
  if (isTRUE(attributes(x$header)$record_line$time_warped)) {
    warning(
      "Changing the frequency of a phase-warped object; its `frequency` is ",
      "not a physical sampling rate. Convert before warping, or use ",
      "normalize_window() to change the grid length."
    )
  }

  sig <- x$signal
  n_orig <- nrow(sig)
  if (n_orig < 2L) {
    warning("Fewer than two samples to resample; returning the record unchanged")
    return(x)
  }

  leads <- setdiff(names(sig), "sample")
  values <- lapply(leads, function(lead) {
    resample_values(as.numeric(sig[[lead]]), ratio, method, anti_alias)
  })
  n_new <- if (length(values) > 0) {
    length(values[[1]])
  } else {
    resampled_length(n_orig, ratio)
  }

  # Sample indices are positions in time, so they scale with the rate. Records
  # normally start at zero and this reduces to the usual 0-based grid, but a
  # window that kept its absolute source indices keeps them (rescaled) here too.
  new_origin <- as.integer(round(as.numeric(sig$sample)[1] * ratio))
  new_samples <- new_origin + seq_len(n_new) - 1L
  new_signal <- do.call(
    signal_table,
    c(
      list(sample = new_samples),
      stats::setNames(values, leads),
      list(units = signal_units(sig))
    )
  )

  # Header carries forward; only the rate and the sample count change
  new_header <- data.table::copy(x$header)
  rl <- attributes(new_header)$record_line
  rl$frequency <- to
  rl$samples <- n_new
  attr(new_header, "record_line") <- rl

  # Every annotator moves with the grid, not just the first one
  new_annotation <- lapply(
    x$annotation,
    rescale_annotation,
    ratio = ratio,
    lower = new_samples[1],
    upper = new_samples[n_new]
  )

  new_EGM(
    signal = new_signal,
    header = new_header,
    annotation = new_annotation
  )
}

# Helpers ----------------------------------------------------------------------

#' Validate a sampling frequency argument
#' @keywords internal
validate_frequency <- function(frequency, arg = "frequency") {
  if (
    length(frequency) != 1L || !is.numeric(frequency) ||
      !is.finite(frequency) || frequency <= 0
  ) {
    stop("`", arg, "` must be a single positive value in Hz")
  }
  as.numeric(frequency)
}

#' Settle on the rate the data is currently sampled at
#'
#' @description The source rate normally comes from the object's own header, and
#'   passing it explicitly is redundant. A caller may still state it, in which
#'   case it is an assertion: a disagreement with the recorded rate is an error
#'   rather than an instruction to rescale by the wrong ratio.
#'
#' @details A record with no usable recorded rate is accepted on the caller's
#'   word - there is nothing to contradict, and the declared rate repairs the
#'   header - but with nothing declared either there is no ratio to compute, so
#'   that combination fails. A `windows` collection that mixes rates cannot be
#'   described by a single `from`, so it fails here rather than silently
#'   mis-scaling part of the collection; convert each source record before
#'   combining them.
#'
#' @param actual Recorded rate(s), as [frequency_of()] reports them.
#' @param from The rate the caller declared, or `NULL`.
#'
#' @return The source rate to convert from.
#'
#' @keywords internal
resolve_source_frequency <- function(actual, from) {
  known <- actual[is.finite(actual) & actual > 0]

  if (is.null(from)) {
    if (length(known) == 0) {
      stop(
        "The data carries no usable sampling frequency, so there is nothing to ",
        "convert from; state it with `from`",
        call. = FALSE
      )
    }
    if (length(known) > 1) {
      stop(
        "The data mixes sampling rates (",
        paste0(format(known), " Hz", collapse = ", "),
        "), so a single conversion cannot describe it. Convert each source ",
        "record before combining them.",
        call. = FALSE
      )
    }
    return(known)
  }

  from <- validate_frequency(from, "from")
  if (length(known) == 0) {
    return(from)
  }
  agrees <- vapply(
    known,
    function(f) isTRUE(all.equal(f, from)),
    logical(1)
  )
  if (!all(agrees)) {
    stop(
      "`from` was given as ",
      from,
      " Hz, but the data is recorded at ",
      paste0(format(known), " Hz", collapse = ", "),
      ". Check the recorded rate with frequency(); a collection that mixes ",
      "rates must be converted one source record at a time.",
      call. = FALSE
    )
  }
  from
}

#' The conversion ratio, target over source
#' @keywords internal
ratio_of <- function(from, to) {
  to / validate_frequency(from, "from")
}

#' Rescale annotation sample indices onto a resampled grid
#'
#' @description Annotations mark positions in time, so a rate change moves them
#'   by the same ratio as the signal. The `time` column is intentionally left
#'   alone: absolute time is unchanged by a duration-preserving conversion.
#'
#' @param ann An `annotation_table`, possibly empty.
#' @param ratio The frequency ratio (target / source).
#' @param lower,upper Inclusive bounds of the new sample grid, used to clamp
#'   annotations that would round past an edge.
#'
#' @return The rescaled `annotation_table`.
#'
#' @keywords internal
rescale_annotation <- function(ann, ratio, lower, upper) {
  if (is.null(ann) || nrow(ann) == 0) {
    return(ann)
  }
  out <- data.table::copy(ann)
  mapped <- round(as.numeric(out$sample) * ratio)
  out$sample <- as.integer(pmin(pmax(mapped, lower), upper))
  out
}

#' Low-pass filter a lead ahead of down-sampling
#'
#' @description Removes energy above the new Nyquist frequency so that it is not
#'   folded back into the retained band by the grid change. The cutoff follows
#'   the usual decimation convention of 80% of the new Nyquist, expressed here
#'   normalised to the *original* Nyquist (hence `0.8 * ratio`).
#'
#' @details Filtering is zero-phase ([signal::filtfilt()]), so fiducial timing is
#'   not shifted. Short records are returned unfiltered rather than filtered
#'   unstably: `filtfilt` runs the filter in both directions and needs a
#'   comfortable margin of samples relative to the filter order. Leads carrying
#'   non-finite values are also passed through, since the filter would propagate
#'   an `NA` across the whole lead.
#'
#' @param values A `numeric` lead.
#' @param ratio The frequency ratio (target / source). Values at or above 1 are
#'   up-sampling and need no filter.
#' @param order Butterworth filter order.
#'
#' @return The filtered (or untouched) lead.
#'
#' @keywords internal
lowpass_for_decimation <- function(values, ratio, order = 4L) {
  if (ratio >= 1 || !all(is.finite(values))) {
    return(values)
  }
  if (length(values) < 3L * (3L * order + 1L)) {
    return(values)
  }
  cutoff <- 0.8 * ratio
  if (cutoff <= 0 || cutoff >= 1) {
    return(values)
  }
  bf <- signal::butter(order, cutoff)
  as.numeric(signal::filtfilt(bf, values))
}

#' Approximate a ratio as a rational fraction
#'
#' @description Polyphase resampling is defined in terms of integer up- and
#'   down-sampling factors, so an arbitrary frequency ratio has to be expressed
#'   as `p/q`. This uses a continued-fraction expansion, which yields the best
#'   rational approximation for a given denominator bound - exact for the related
#'   rates that come up in practice (500/250, 1000/250), and close for the rest.
#'
#' @param ratio A positive ratio to approximate.
#' @param max_denominator The largest denominator to allow. Bounding this keeps
#'   the FIR filter that `signal::resample()` builds to a sane length.
#'
#' @return A list with integer elements `p` (numerator) and `q` (denominator).
#'
#' @keywords internal
rational_ratio <- function(ratio, max_denominator = 1000L) {
  if (!is.finite(ratio) || ratio <= 0) {
    stop("Cannot approximate a non-positive frequency ratio")
  }

  # Successive convergents of the continued fraction for `ratio`, kept as the
  # pair (p1/q1) one step behind the first convergent that exceeds the bound.
  p0 <- 0
  q0 <- 1
  p1 <- 1
  q1 <- 0
  remainder <- ratio

  repeat {
    whole <- floor(remainder)
    p2 <- whole * p1 + p0
    q2 <- whole * q1 + q0
    if (q2 > max_denominator) {
      break
    }
    p0 <- p1
    q0 <- q1
    p1 <- p2
    q1 <- q2

    fraction <- remainder - whole
    if (fraction < 1e-9) {
      break
    }
    remainder <- 1 / fraction
  }

  # Ratios smaller than 1 / max_denominator collapse to zero; keep the fraction
  # usable rather than degenerate.
  list(p = max(1L, as.integer(p1)), q = max(1L, as.integer(q1)))
}
