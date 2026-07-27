#' @keywords internal
identify_channel_source <- function(x) {
  # Intakes character vector and identifies the source
  stopifnot("Not a known/supported channel yet." = x %in% .labels)

  # Find source of lead bipole
  for (i in names(.leads)) {
    if (x %in% .leads[[i]]) {
      y <- i
    }
  }

  # Return
  y
}

# Annotation channels ----------------------------------------------------------

#' Per-lead annotations and the guiding channel
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Why several functions in this package ask which lead to follow, and how they
#' decide when they have to.
#'
#' @section Guiding channel:
#'
#'   A delineating annotator may be run over a record once, or once per lead. Run
#'   per lead it writes twelve independent copies of every fiducial, separated
#'   only by the `channel` column, and such a table does not describe beats until
#'   it is resolved to a single lead: pooled, it reports twelve times as many
#'   beats as the record contains, and taking the first match at each point
#'   silently follows whichever lead happens to sort first.
#'
#'   So every function that reads fiducials - [get_windows()],
#'   [learn_template()], [median_window()] and the other window transforms,
#'   [extract_f_waves()], [vectorcardiogram()] - takes a `channel`, and resolves
#'   it the same way:
#'
#'   * annotations spanning more than one channel with no `channel` given are an
#'     error, not a guess;
#'   * a `channel` that the annotations do not carry is an error, which is what
#'     catches a numbering convention that does not match;
#'   * the global channel `0` is not a lead, so it never counts toward the span
#'     and is always kept alongside the chosen lead.
#'
#'   The channel may be a number or a channel name (`"II"`), resolved against the
#'   record's own header. A number is matched literally against the `channel`
#'   column, and the numbering there belongs to the annotator that wrote the
#'   file - the WFDB tools number channels from `0`, others from `1`. Read it off
#'   the record rather than assuming:
#'
#'   ```r
#'   table(get_annotation(x)$channel)
#'   ```
#'
#'   Only the boundaries are restricted to one lead. Windows still carry the
#'   signal for every channel, which is what makes a windowed beat usable as a
#'   vectorcardiogram.
#'
#' @seealso [get_windows()], [label_waves()], [get_annotation()]
#'
#' @name channels
NULL

# An annotator run per lead writes one copy of every fiducial for each lead,
# separated only by the `channel` column. Every function that consumes
# annotations has to resolve that, and they all resolve it here so that a record
# needing a guiding lead is told so in the same words wherever it is noticed.
#
# The policy has two halves, and both are errors rather than warnings. A warning
# that is correct but survivable is the worst of both: it disappears in a batch
# job and leaves behind an object that looks well formed.
#
#   * A table spanning more than one channel is ambiguous. Pooling the channels
#     multiplies every count by the number of leads; taking the first match
#     silently picks whichever lead sorted first.
#   * A channel the caller did name has to be present. Otherwise the search
#     falls back to the global channel and quietly returns the wrong fiducials,
#     which is how an off-by-one in the numbering convention would go unnoticed.
#
# Channel `0` is the WFDB global channel rather than a lead of its own, so it is
# excluded from the span and always retained by a restriction.

#' Channels an annotation table spans
#'
#' @description The distinct lead-specific channels present in an annotation
#'   table. The global channel `0` is excluded, so a table that carries only
#'   global annotations spans no channels and needs no guiding lead.
#'
#' @param ann An `annotation_table` (or compatible `data.table`).
#'
#' @return A sorted `integer` vector, empty when the table has no `channel`
#'   column or holds only global annotations.
#'
#' @keywords internal
annotation_channels <- function(ann) {
  if (is.null(ann) || nrow(ann) == 0L || !("channel" %in% colnames(ann))) {
    return(integer())
  }
  channels <- unique(suppressWarnings(as.integer(ann$channel)))
  sort(channels[!is.na(channels) & channels != 0L])
}

#' Validate a channel argument
#'
#' @description Accepts the forms a caller reasonably writes for a single
#'   guiding lead: a channel number, a stable channel name, or `NULL` for no
#'   restriction. A `list(channel = ...)` wrapper is also accepted and unwrapped,
#'   since the neighbouring `onset`/`offset` arguments do take criteria lists and
#'   that is the shape one reaches for first.
#'
#' @param x The channel argument as written by the caller.
#' @param arg The argument name, used in the error message.
#'
#' @return `NULL`, a single `integer`, or a single `character` channel name.
#'
#' @keywords internal
valid_channel <- function(x, arg = "channel") {
  if (is.null(x)) {
    return(NULL)
  }

  if (is.list(x)) {
    if (!identical(names(x), "channel")) {
      stop(
        "`", arg, "` names one guiding lead, not a criteria list. Write ",
        "`", arg, " = 2` or `", arg, " = \"II\"`."
      )
    }
    x <- x[["channel"]]
    if (is.null(x)) {
      return(NULL)
    }
  }

  if (length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be a single channel number or name, or NULL")
  }
  if (is.character(x)) {
    if (!nzchar(x)) {
      stop("`", arg, "` must be a single channel number or name, or NULL")
    }
    return(x)
  }
  if (!is.numeric(x) || !is.finite(x) || x < 0 || x != as.integer(x)) {
    stop("`", arg, "` must be NULL, a non-negative whole number, or a channel name")
  }
  as.integer(x)
}

#' Resolve the guiding channel for an annotation table
#'
#' @description The single point where the package decides whether it may
#'   proceed without being told which lead's annotations to follow. Given the
#'   table and whatever the caller asked for, it returns the channel to restrict
#'   to, or errors with a message that names the argument that fixes it.
#'
#' @details A channel name is returned untouched; only the caller knows the
#'   record it should be resolved against (`resolve_channel_spec()`), and the
#'   presence check applies once it is a number.
#'
#' @param ann An `annotation_table` (or compatible `data.table`).
#' @param channel The validated channel argument, as returned by
#'   [valid_channel()].
#' @param what A `character` naming the caller, used to open the error message.
#' @param arg The argument name to point the caller at.
#'
#' @return `NULL` when no restriction is needed, otherwise the channel.
#'
#' @keywords internal
resolve_annotation_channel <- function(
  ann,
  channel,
  what = "This analysis",
  arg = "channel"
) {
  present <- annotation_channels(ann)

  if (is.null(channel)) {
    if (length(present) > 1L) {
      stop(
        what,
        " needs a guiding `",
        arg,
        "`: these annotations span ",
        length(present),
        " channels (",
        paste(present, collapse = ", "),
        "), which is what an annotator run per lead writes - one copy of every ",
        "fiducial for each lead. Set `",
        arg,
        "` to the channel whose annotations should guide it.",
        call. = FALSE
      )
    }
    return(NULL)
  }

  if (is.character(channel) || length(present) == 0L) {
    return(channel)
  }

  if (!(channel %in% present)) {
    stop(
      "`",
      arg,
      "` was given as ",
      channel,
      ", which these annotations do not carry; they hold channel(s) ",
      paste(present, collapse = ", "),
      ". The numbering is the annotator's own - WFDB's tools number channels ",
      "from 0, others from 1 - so check it with ",
      "`table(get_annotation(x)$channel)`.",
      call. = FALSE
    )
  }

  channel
}

#' Resolve the guiding channel for a collection of windows
#'
#' @description The window transforms locate a fiducial in every window of a
#'   collection, so they apply the channel policy once, against the first window
#'   that carries annotations. The windows of a collection come from one record
#'   and share its annotator, so one is representative of all.
#'
#' @details A channel *name* is also resolved to a number here, against the first
#'   window's own header, since the functions downstream match the `channel`
#'   column numerically.
#'
#' @param windows A list of `EGM` objects.
#' @param channel The validated channel argument, as returned by
#'   [valid_channel()].
#' @param what A `character` naming the caller, used to open the error message.
#'
#' @return `NULL` when no restriction is needed, otherwise the channel number.
#'
#' @keywords internal
require_window_channel <- function(windows, channel, what) {
  if (length(windows) == 0) {
    return(channel)
  }
  channel <- resolve_channel_spec(windows[[1]], channel)
  for (window in windows) {
    ann <- get_single_annotation(window)
    if (!is.null(ann) && nrow(ann) > 0) {
      return(resolve_annotation_channel(ann, channel, what = what))
    }
  }
  channel
}

#' Accept the superseded `channel_criteria` argument
#'
#' @description `channel_criteria` was the older name for what every windowing
#'   function calls `channel`, and its name invited a criteria list although it
#'   only ever accepted a scalar. Both are accepted for now; this resolves them
#'   to one value and warns, once per session per function, when the old name is
#'   used.
#'
#' @param channel,channel_criteria The two arguments as written by the caller.
#' @param fn The calling function's name, used to warn only once for each.
#' @param arg The name of the current argument, used in error messages.
#'
#' @return The validated channel, as [valid_channel()] returns it.
#'
#' @keywords internal
resolve_channel_argument <- function(
  channel,
  channel_criteria = NULL,
  fn = "",
  arg = "channel"
) {
  if (!is.null(channel_criteria)) {
    if (!is.null(channel)) {
      stop(
        "Give either `",
        arg,
        "` or the superseded `channel_criteria`, not both",
        call. = FALSE
      )
    }
    if (!isTRUE(deprecation_state[[fn]])) {
      deprecation_state[[fn]] <- TRUE
      warning(
        "`channel_criteria` is superseded by `",
        arg,
        "` in ",
        fn,
        "(); it names one lead rather than a criteria list. It still works, ",
        "and this is reported once per session.",
        call. = FALSE
      )
    }
    channel <- channel_criteria
  }
  valid_channel(channel, arg)
}

# Warning once rather than once per record: a rename reported 14,000 times in a
# batch job is noise that hides everything around it.
deprecation_state <- new.env(parent = emptyenv())
