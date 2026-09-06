# Class definition for `windows` objects ---------------------------------------

# A `windows` object is a list of `EGM` objects that were cut out of a common
# source record, plus enough provenance to say where they came from and what has
# been done to them since. The provenance is split deliberately:
#
#   method  - the extraction strategy that produced the collection. Set once by
#             `get_windows()` and never overwritten, so a padded, normalized,
#             resampled collection still knows it started as rhythm windows.
#   history - every step applied, in order, e.g. c("rhythm", "padded",
#             "normalized"). Each transform appends exactly one entry.
#   dropped - candidate beats the strategy found but did not return, by reason.
#             Recorded here rather than only messaged, so that a batch running
#             on background workers can still audit its own drop rate.
#
# The window count is deliberately *not* stored; it is `length(x)`, and keeping a
# copy meant every method that changed the length had to remember to update it.

#' Create a `windows` object containing a collection of EGM windows
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `windows` objects are lists of `EGM` objects that represent segments of one
#' or more source signals. This allows specialized methods to be applied to a
#' whole collection of segments at once. This function is the class constructor
#' and only applies class attributes; collections are normally produced by
#' [get_windows()] rather than built by hand.
#'
#' @param x A list of `EGM` objects.
#' @param method The extraction strategy that produced the collection, e.g.
#'   `"rhythm"`. Recorded once and carried through every transform.
#' @param source_record The name of the original record.
#' @param history A `character` vector of the steps applied so far, in order.
#'   Defaults to `method`, which is correct at the point of extraction.
#' @param dropped A named `integer` vector counting candidate beats the strategy
#'   found but did not return, by reason. Read it back with [window_dropped()].
#' @param ... Additional arguments passed to methods.
#'
#' @return An object of class `windows` which inherits from `list`.
#'
#' @seealso [get_windows()] to create one from an `EGM`, [window_dropped()] for
#'   the candidates it did not return.
#'
#' @export
new_windows <- function(
  x = list(),
  method = character(),
  source_record = character(),
  history = method,
  dropped = integer(),
  ...
) {
  if (!is.list(x)) {
    stop("x must be a list")
  }

  if (length(x) > 0) {
    is_EGM_list <- all(vapply(x, inherits, logical(1), "EGM"))
    if (!is_EGM_list) {
      stop("All elements of x must be of class 'EGM'")
    }
  }

  set_windows_attrs(
    unclass(x),
    method = method,
    source_record = source_record,
    history = history,
    dropped = dropped,
    creation_time = Sys.time()
  )
}

# Shared attribute stamping, so the constructor, `[` and `c` cannot drift apart.
set_windows_attrs <- function(
  x,
  method,
  source_record,
  history,
  dropped = integer(),
  creation_time = Sys.time()
) {
  if (is.null(dropped)) {
    dropped <- integer()
  }
  structure(
    x,
    class = c("windows", "list"),
    method = as.character(method),
    source_record = source_record,
    history = as.character(history),
    dropped = dropped,
    creation_time = creation_time
  )
}

#' Test if an object is a `windows` collection
#'
#' @description Named `is_window_set()` rather than the shorter `is_windows()`
#'   so it is not mistaken for the operating-system predicate of that name that
#'   many packages define (a test of `.Platform$OS.type`). This tests the EGM
#'   `windows` class, not the platform.
#'
#' @param x An object to test.
#'
#' @return `TRUE` if x is a `windows` object, `FALSE` otherwise.
#'
#' @export
is_window_set <- function(x) {
  inherits(x, "windows")
}

#' Format a `windows` object for printing
#'
#' @param x A `windows` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return Invisibly returns x.
#'
#' @export
format.windows <- function(x, ...) {
  cat("<windows: ", length(x), " EGM segments>\n", sep = "")
  cat("Method: ", paste(attr(x, "method"), collapse = ", "), "\n", sep = "")
  # The history is the pipeline as applied, so an arrow chain reads correctly
  cat("History: ", paste(attr(x, "history"), collapse = " -> "), "\n", sep = "")
  cat("Source: ", attr(x, "source_record"), "\n", sep = "")
  dropped <- window_dropped(x)
  dropped <- dropped[dropped > 0]
  if (length(dropped) > 0) {
    cat(
      "Dropped: ",
      paste0(dropped, " ", names(dropped), collapse = ", "),
      "\n",
      sep = ""
    )
  }
  cat("Created: ", format(attr(x, "creation_time")), "\n", sep = "")

  invisible(x)
}

#' Print a `windows` object
#'
#' @param x A `windows` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return Invisibly returns x.
#'
#' @export
print.windows <- function(x, ...) {
  format(x, ...)
  invisible(x)
}

#' Subset a `windows` object
#'
#' @param x A `windows` object.
#' @param i Index to subset.
#' @param ... Additional arguments passed to methods.
#'
#' @return A `windows` object with the specified subset of elements.
#'
#' @export
`[.windows` <- function(x, i, ...) {
  attrs <- attributes(x)
  result <- NextMethod()

  set_windows_attrs(
    result,
    method = attrs$method,
    source_record = attrs$source_record,
    history = attrs$history,
    dropped = attrs$dropped,
    creation_time = attrs$creation_time
  )
}

#' Concatenate `windows` objects
#'
#' @param ... `windows` objects to concatenate.
#'
#' @return A `windows` object containing all the elements of the inputs.
#'
#' @export
c.windows <- function(...) {
  args <- list(...)

  if (!all(vapply(args, is_window_set, logical(1)))) {
    stop("All arguments must be `windows` objects")
  }


  # Provenance is taken from the first non-empty input; an all-empty
  # concatenation falls back to the first so the attributes are still well formed
  first_non_empty <- which(vapply(args, length, integer(1)) > 0)[1]
  if (is.na(first_non_empty)) {
    first_non_empty <- 1
  }
  donor <- args[[first_non_empty]]

  result <- do.call(c, lapply(args, unclass))

  set_windows_attrs(
    result,
    method = attr(donor, "method"),
    source_record = attr(donor, "source_record"),
    history = attr(donor, "history"),
    # Drop counts are counts of candidates, so a concatenation accumulates them
    dropped = sum_dropped(lapply(args, window_dropped))
  )
}

# Add named count vectors that need not share names or order.
sum_dropped <- function(counts) {
  counts <- counts[lengths(counts) > 0]
  if (length(counts) == 0) {
    return(integer())
  }
  reasons <- unique(unlist(lapply(counts, names)))
  totals <- vapply(
    reasons,
    function(r) sum(vapply(counts, function(x) {
      if (!(r %in% names(x)) || is.na(x[[r]])) 0L else as.integer(x[[r]])
    }, integer(1))),
    integer(1)
  )
  totals
}

#' Apply a function across a collection of windows
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Applies `f` to each `EGM` in a collection. When every result is itself an
#' `EGM` the collection is rebuilt as a `windows` object with its provenance
#' carried forward; otherwise a plain list of results is returned, which is the
#' natural shape for extracting per-window measurements.
#'
#' @param x A `windows` object or list of `EGM` objects.
#' @param f A function applied to each window.
#' @param ... Additional arguments passed to `f`.
#'
#' @return A `windows` object when all results are `EGM` objects, otherwise a
#'   list.
#'
#' @examples
#' \dontrun{
#' beats <- get_windows(read_wfdb("ecg", test_path(), "ecgpuwave"))
#'
#' # Per-window measurement -> plain list
#' map_windows(beats, function(w) nrow(w$signal))
#'
#' # EGM-valued transform -> windows object, provenance preserved
#' map_windows(beats, function(w) w)
#' }
#'
#' @export
map_windows <- function(x, f, ...) {
  windows <- as_window_list(x)
  f <- match.fun(f)

  results <- lapply(unclass(windows), f, ...)

  if (
    length(results) > 0 &&
      all(vapply(results, inherits, logical(1), "EGM"))
  ) {
    return(new_windows(
      results,
      method = window_method(x),
      source_record = window_source_record(x),
      history = c(window_history(x), "mapped"),
      dropped = window_dropped(x)
    ))
  }

  results
}

# Shared window helpers --------------------------------------------------------

# The transform functions (padding, median, normalization, warping, and the
# resampling in `frequency.R`) all operate on the same raw material: a `windows`
# object or a bare list of `EGM` objects. They repeatedly need to (a) accept
# either container and (b) recover the provenance so derived collections stay
# traceable. Factoring these here keeps the public functions small and their
# behaviour consistent with one another.

#' Coerce window input to a plain list of `EGM` objects
#'
#' @description Accepts either a `windows` object or a bare list of `EGM`
#'   objects and returns it unchanged after validating that every element is an
#'   `EGM`. The window-transform functions iterate with base `lapply()`/`vapply()`
#'   (which treat a `windows` object as an ordinary list), so no unclassing is
#'   required here.
#'
#' @param x A `windows` object or list of `EGM` objects.
#' @param arg Name of the calling argument, used for a clearer error message.
#'
#' @return The input, validated as a list of `EGM` objects.
#'
#' @keywords internal
as_window_list <- function(x, arg = "x") {
  if (is_window_set(x)) {
    return(x)
  }
  if (is.list(x) && (length(x) == 0 || all(vapply(x, inherits, logical(1), "EGM")))) {
    return(x)
  }
  stop("`", arg, "` must be a `windows` object or a list of `EGM` objects")
}

#' Recover the source record label from a window collection
#'
#' @description Reads the `source_record` attribute set by [new_windows()]. Bare
#'   lists carry no such attribute, so an empty character vector is returned,
#'   which [new_windows()] accepts as its default.
#'
#' @param x A `windows` object or list of `EGM` objects.
#'
#' @return A `character` scalar (or empty character vector).
#'
#' @keywords internal
window_source_record <- function(x) {
  sr <- attr(x, "source_record")
  if (is.null(sr)) character() else sr
}

#' Recover the extraction strategy from a window collection
#'
#' @description Reads the `method` attribute, which records how the collection
#'   was originally extracted and is never overwritten by a transform. Bare lists
#'   return an empty character vector.
#'
#' @param x A `windows` object or list of `EGM` objects.
#'
#' @return A `character` scalar (or empty character vector).
#'
#' @keywords internal
window_method <- function(x) {
  m <- attr(x, "method")
  if (is.null(m)) character() else as.character(m)
}

#' Rebuild a window collection after a transform
#'
#' @description Wraps a transform's list of results back into a `windows`
#'   object, carrying the extraction method and source record through unchanged
#'   and appending one entry to the history. Every transform routes its return
#'   value through here, which is what keeps provenance from being erased a step
#'   at a time.
#'
#' @param result The list of transformed `EGM` objects.
#' @param source The collection the transform was applied to.
#' @param step A single string naming the step, e.g. `"padded"`.
#'
#' @return A `windows` object.
#'
#' @keywords internal
rewrap_windows <- function(result, source, step) {
  new_windows(
    lapply(result, keep_ECG, windows = source),
    method = window_method(source),
    source_record = window_source_record(source),
    history = c(window_history(source), step),
    dropped = window_dropped(source)
  )
}

#' Carry the ECG class onto a derived beat
#'
#' @description A window cut from an [ECG], and a beat derived from such windows,
#'   holds the same leads and so is still an `ECG`. Without this the class would
#'   be lost at the first transform, and analyses gated on it - notably
#'   [vcg()] - could not be handed a windowed beat.
#'
#' @details The class is set directly rather than through [ECG()], which would
#'   re-validate a lead set already known to be good and warn once per window.
#'
#' @param x The derived `EGM` object.
#' @param windows The windows it came from; the class is carried only when every
#'   one of them is an `ECG`.
#'
#' @return `x`, classed as an `ECG` where that is warranted.
#'
#' @keywords internal
keep_ECG <- function(x, windows) {
  inherited <- length(windows) > 0 &&
    all(vapply(windows, is_ECG, logical(1)))

  if (!inherited || is_ECG(x)) {
    return(x)
  }

  structure(x, class = union("ECG", class(x)))
}

#' Recover the transform history from a window collection
#'
#' @description Reads the `history` attribute, the ordered record of every step
#'   applied to the collection. Each transform appends one entry, so a bare list
#'   (which has no history) starts an empty chain.
#'
#' @param x A `windows` object or list of `EGM` objects.
#'
#' @return A `character` vector of steps, in order applied.
#'
#' @keywords internal
window_history <- function(x) {
  h <- attr(x, "history")
  if (is.null(h)) character() else as.character(h)
}
