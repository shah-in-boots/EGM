# Annotation feature criteria and lookup ---------------------------------------

# A "feature" is a rule for picking annotation rows out of an annotation table:
# a bare symbol matched against `type`, or a named list matched column by column.
# The same rule powers window boundary detection, padding and median alignment,
# and landmark template learning and warping, so it lives here rather than in any
# one of those modules.

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

# Criteria lists are the user-facing half of a feature: a named list mapping
# annotation column -> accepted value(s). Strategy and landmark constructors
# validate them at construction so that a misspelled column errors where the
# user typed it, rather than silently matching nothing later.
valid_feature_criteria <- function(x) {
  is.list(x) && length(x) > 0L && !is.null(names(x)) &&
    all(nzchar(names(x))) && !anyDuplicated(names(x))
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
  as.integer(match_features(ann, feature, channel_criteria)$sample)
}

#' Rows of an annotation table matching a feature specification
#'
#' @description The engine behind [locate_features()], returning the matched rows
#'   rather than their sample indices. Callers that have to explain an ambiguous
#'   match need the rows: whether twelve matches are twelve fiducials or one
#'   fiducial annotated on twelve leads is only visible in their channels.
#'
#' @inheritParams locate_feature
#' @return The matching rows, in annotation order.
#' @keywords internal
match_features <- function(ann, feature, channel_criteria = NULL) {
  if (is.null(ann) || nrow(ann) == 0) {
    return(empty_match())
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
  # lead. Where the table numbers its channels by signal there is no global
  # channel to fall back to, and 0 is a lead like any other.
  if (
    !is.null(channel_criteria) && "channel" %in% colnames(work) &&
      !("channel" %in% names(criteria))
  ) {
    requested <- filter_criteria(
      work[work$channel == as.integer(channel_criteria), ]
    )
    if (nrow(requested) > 0 || identical(channel_zero(ann), "signal")) {
      return(requested)
    }
    return(filter_criteria(work[work$channel == 0L, ]))
  }

  filter_criteria(work)
}

# The zero-row result, shaped so that `$sample` and `$channel` are still
# addressable by callers that summarise a match without testing for emptiness.
empty_match <- function() {
  data.frame(sample = integer(), channel = integer())
}

#' Explain an ambiguous feature match
#'
#' @description Turns "matched 12 annotations" into a sentence that names the
#'   cause and the argument that resolves it. A count that rises with the number
#'   of leads is the clue that an annotator was run per lead, and it only reads
#'   as a clue to someone who already knows.
#'
#' @param rows The matched rows, as [match_features()] returns them.
#' @param where An optional clause naming where the match happened, e.g.
#'   `" in example 3"`, placed before the explanation.
#' @param arg The argument name that would disambiguate them.
#'
#' @return A `character` scalar describing the match.
#'
#' @keywords internal
describe_matches <- function(rows, where = "", arg = "channel") {
  channels <- annotation_channels(rows)
  counted <- paste0("matched ", nrow(rows), " annotations")
  if (length(channels) < 2L) {
    return(paste0(counted, where))
  }
  paste0(
    counted, " across ", length(channels),
    " channels (", paste(channels, collapse = ", "), ")", where,
    ", which is one fiducial per lead rather than several fiducials; set `",
    arg, "` to choose a guiding lead"
  )
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
#'   on demand via [label_waves()], mirroring how [by_rhythm()] resolves
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
