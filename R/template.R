# Landmark and template classes ----------------------------------------------

# Used by both S7 validators. Keeping this small shared predicate avoids
# repeating subtly different scalar-string checks across the two classes.
valid_scalar_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

# A channel is unset when a landmark should be located without a channel
# restriction. This predicate is shared by construction, learning, printing,
# and warping.
channel_is_unset <- function(x) {
  is.null(x) || (length(x) == 1L && is.numeric(x) && is.na(x))
}

#' A single template landmark
#'
#' `r lifecycle::badge("experimental")`
#'
#' A landmark identifies one annotation-derived fiducial point on one channel.
#' Its `position` is `NA` until learned, or a sample coordinate on a template's
#' target grid when supplied manually or returned by [learn_template()].
#'
#' @param name A single non-empty landmark name.
#' @param criteria A non-empty named list of annotation matching criteria, such
#'   as `list(type = "N")`.
#' @param channel One non-negative channel number, a stable channel name, or
#'   `NA` to search without a channel restriction.
#' @param position Target sample position, or `NA` for an unfitted landmark.
#' @param required Whether this landmark must be found for a complete match.
#'
#' @return A `landmark` S7 object.
#' @export
landmark <- S7::new_class(
  "landmark",
  properties = list(
    name = S7::class_character,
    criteria = S7::class_list,
    channel = S7::class_any,
    position = S7::class_numeric,
    required = S7::class_logical
  ),
  constructor = function(
    name,
    criteria,
    channel = NA_integer_,
    position = NA_real_,
    required = TRUE
  ) {
    if (is.numeric(channel) && length(channel) == 1L && !is.na(channel)) {
      channel <- as.integer(channel)
    }
    S7::new_object(
      S7::S7_object(),
      name = name,
      criteria = criteria,
      channel = channel,
      position = as.numeric(position),
      required = as.logical(required)
    )
  },
  validator = function(self) {
    if (!valid_scalar_string(self@name)) {
      return("`name` must be a single string")
    }
    if (
      !is.list(self@criteria) || length(self@criteria) == 0L ||
        is.null(names(self@criteria)) || any(!nzchar(names(self@criteria))) ||
        anyDuplicated(names(self@criteria))
    ) {
      return("`criteria` must be a non-empty named list")
    }
    channel_ok <- length(self@channel) == 1L && (
      (is.numeric(self@channel) &&
        (is.na(self@channel) ||
          (is.finite(self@channel) && self@channel >= 0 &&
            self@channel == as.integer(self@channel)))) ||
        (is.character(self@channel) && !is.na(self@channel) &&
          nzchar(self@channel))
    )
    if (!channel_ok) {
      return("`channel` must be one non-negative number, channel name, or NA")
    }
    if (
      length(self@position) != 1L ||
        (!is.na(self@position) && !is.finite(self@position))
    ) {
      return("`position` must be one finite number or NA")
    }
    if (length(self@required) != 1L || is.na(self@required)) {
      return("`required` must be TRUE or FALSE")
    }
    NULL
  }
)

#' A template of ordered landmarks
#'
#' `r lifecycle::badge("experimental")`
#'
#' A template is an ordered group of positioned [landmark] objects on a common
#' target sample grid. Create one manually with `template()` or learn landmark
#' positions from multiple EGM examples with [learn_template()]. It is passed
#' directly to [warp_window()].
#'
#' @param landmarks A position-ordered list of fitted [landmark] objects.
#' @param target_samples Length of the target sample grid.
#' @param method Either `"manual"` or `"learned"`.
#' @param frequency Source sampling frequency retained as provenance, or `NA`.
#' @param statistics Optional landmark learning statistics.
#'
#' @return A `template` S7 object.
#' @examples
#' manual_template <- template(
#'   landmarks = list(
#'     landmark("onset", list(type = "("), channel = 2, position = 0),
#'     landmark("QRS", list(type = "N"), channel = 2, position = 250),
#'     landmark("offset", list(type = ")"), channel = 2, position = 499)
#'   ),
#'   target_samples = 500
#' )
#' is_template(manual_template)
#' @export
template <- S7::new_class(
  "template",
  properties = list(
    landmarks = S7::class_list,
    target_samples = S7::class_integer,
    method = S7::class_character,
    frequency = S7::class_numeric,
    statistics = S7::class_any
  ),
  constructor = function(
    landmarks,
    target_samples = 500L,
    method = "manual",
    frequency = NA_real_,
    statistics = NULL
  ) {
    S7::new_object(
      S7::S7_object(),
      landmarks = landmarks,
      target_samples = as.integer(target_samples),
      method = method,
      frequency = as.numeric(frequency),
      statistics = statistics
    )
  },
  validator = function(self) {
    if (length(self@landmarks) == 0L ||
      !all(vapply(
        self@landmarks,
        function(x) S7::S7_inherits(x, landmark),
        logical(1)
      ))) {
      return("`landmarks` must be a non-empty list of landmark objects")
    }
    landmark_names <- vapply(
      self@landmarks,
      function(x) x@name,
      character(1)
    )
    if (anyDuplicated(landmark_names)) {
      return("Landmark names must be unique")
    }
    if (length(self@target_samples) != 1L || is.na(self@target_samples) ||
      self@target_samples < 2L) {
      return("`target_samples` must be a single integer of at least 2")
    }
    positions <- vapply(
      self@landmarks,
      function(x) as.numeric(x@position),
      numeric(1)
    )
    if (anyNA(positions) || any(!is.finite(positions)) ||
      any(positions < 0 | positions > self@target_samples - 1L)) {
      return("Landmark positions must be finite and lie within the target grid")
    }
    if (anyDuplicated(positions) || is.unsorted(positions, strictly = TRUE)) {
      return("Landmark positions must be unique and increasing")
    }
    if (!valid_scalar_string(self@method) ||
      !self@method %in% c("manual", "learned")) {
      return("`method` must be 'manual' or 'learned'")
    }
    if (length(self@frequency) != 1L ||
      (!is.na(self@frequency) &&
        (!is.finite(self@frequency) || self@frequency <= 0))) {
      return("`frequency` must be one positive number or NA")
    }
    if (!is.null(self@statistics) && !is.data.frame(self@statistics)) {
      return("`statistics` must be NULL or a data frame")
    }
    NULL
  }
)

#' Test whether an object is a template
#'
#' @param x An object to test.
#' @return A single logical value.
#' @export
is_template <- function(x) S7::S7_inherits(x, template)

# Wrapped in `local()` for the reason given at the same construct in
# `windows-extract.R`: a top-level `method(print, cls) <- f` leaves a copy of
# `print` in the namespace, against which every `S3method(print, ...)` directive
# then registers instead of `base::print`.
local({
  S7::method(print, landmark) <- function(x, ...) {
    channel <- if (channel_is_unset(x@channel)) {
      "any"
    } else {
      as.character(x@channel)
    }
    position <- if (is.na(x@position)) "unfitted" else format(x@position)
    cat(
      "<landmark: ", x@name, ">\n",
      "  channel: ", channel, "\n",
      "  position: ", position, "\n",
      sep = ""
    )
    invisible(x)
  }

  S7::method(print, template) <- function(x, ...) {
    cat(
      "<template: ", x@method, ">\n",
      "  landmarks: ", length(x@landmarks), "\n",
      "  target samples: ", x@target_samples, "\n",
      sep = ""
    )
    invisible(x)
  }
})

# Resolve a stable signal name to the integer annotation channel used by one
# EGM. Numeric channel specifications pass through unchanged. This is shared by
# template learning and window warping.
resolve_channel_spec <- function(egm, channel) {
  if (channel_is_unset(channel)) {
    return(NULL)
  }
  if (is.numeric(channel)) {
    return(as.integer(channel))
  }

  signal_names <- setdiff(names(egm$signal), "sample")
  idx <- match(channel, signal_names)
  if (!is.na(idx)) {
    return(as.integer(idx))
  }

  header <- egm$header
  for (field in c("label", "lead")) {
    if (field %in% names(header)) {
      idx <- match(channel, as.character(header[[field]]))
      if (!is.na(idx)) {
        if ("number" %in% names(header)) {
          return(as.integer(header$number[[idx]]))
        }
        return(as.integer(idx))
      }
    }
  }

  stop(
    "Channel '", channel, "' is not present in the EGM; available signals: ",
    paste(signal_names, collapse = ", ")
  )
}

#' Learn a landmark template from EGM examples
#'
#' Locates each requested landmark in multiple annotated EGM examples, estimates
#' its median or mean fractional position, and returns a fitted [template]. For
#' manual templates, construct positioned [landmark] objects and pass them to
#' [template()] directly.
#'
#' @param x An annotated `EGM`, list of `EGM` objects, or `windows` object.
#' @param landmarks A named list of concise landmark specifications or a list of
#'   unfitted [landmark] objects. A concise specification may be a type string or
#'   a named criteria list with optional `channel` and `required` fields.
#' @param target_samples Number of samples in the template target grid.
#' @param channel Default channel for landmarks without their own one, given as a
#'   channel number or name. Required when the examples' annotations span more
#'   than one channel; see the channels section.
#' @param frequency Optional source frequency stored as provenance. By default it
#'   is read from the first example.
#' @param position_estimator Use the median or mean landmark phase.
#' @param missing Retain only complete examples, use all available observations,
#'   or error on incomplete examples.
#' @param ambiguous Error when a landmark has multiple matches, or use the first.
#' @param order_policy Drop examples with crossed landmarks, or error.
#' @param channel_criteria Superseded name for `channel`, still accepted.
#' @param ... Additional arguments, currently unused.
#'
#' @inheritSection channels Guiding channel
#'
#' @return A learned [template] S7 object.
#' @export
learn_template <- function(
  x,
  landmarks = list(
    P_onset = list(type = "(", wave = "P"),
    QRS = list(type = "N"),
    T_offset = list(type = ")", wave = "T")
  ),
  target_samples = 500L,
  channel = NULL,
  frequency = NULL,
  position_estimator = c("median", "mean"),
  missing = c("complete", "available", "error"),
  ambiguous = c("error", "first"),
  order_policy = c("drop", "error"),
  channel_criteria = NULL,
  ...
) {
  position_estimator <- match.arg(position_estimator)
  missing <- match.arg(missing)
  ambiguous <- match.arg(ambiguous)
  order_policy <- match.arg(order_policy)

  if (
    length(target_samples) != 1L || is.na(target_samples) ||
      !is.finite(target_samples) || target_samples < 2L ||
      target_samples != as.integer(target_samples)
  ) {
    stop("`target_samples` must be a single integer of at least 2")
  }
  target_samples <- as.integer(target_samples)

  channel_criteria <- resolve_channel_argument(
    channel,
    channel_criteria,
    fn = "learn_template"
  )

  if (
    is.list(landmarks) && length(landmarks) > 0L &&
      all(vapply(
        landmarks,
        function(x) S7::S7_inherits(x, landmark),
        logical(1)
      ))
  ) {
    specs <- lapply(landmarks, function(x) {
      landmark(
        name = x@name,
        criteria = x@criteria,
        channel = if (channel_is_unset(x@channel) &&
          !is.null(channel_criteria)) channel_criteria else x@channel,
        required = x@required
      )
    })
  } else {
    if (!is.list(landmarks) || length(landmarks) == 0L ||
      is.null(names(landmarks)) || any(!nzchar(names(landmarks)))) {
      stop("`landmarks` must be a non-empty named list")
    }
    specs <- lapply(names(landmarks), function(name) {
      spec <- landmarks[[name]]
      if (is.character(spec)) {
        if (length(spec) == 0L || anyNA(spec)) {
          stop("Landmark '", name, "' must contain a non-missing type")
        }
        criteria <- list(type = spec)
        channel <- channel_criteria
        required <- TRUE
      } else if (is.list(spec)) {
        normalized <- "criteria" %in% names(spec)
        if (normalized) {
          unknown <- setdiff(names(spec), c("criteria", "channel", "required"))
          if (length(unknown) > 0L) {
            stop(
              "Unknown field(s) in landmark '", name, "': ",
              paste(unknown, collapse = ", ")
            )
          }
          criteria <- spec$criteria
        } else {
          criteria <- spec[setdiff(names(spec), c("channel", "required"))]
        }
        channel <- if ("channel" %in% names(spec)) {
          spec$channel
        } else if (!is.null(channel_criteria)) {
          channel_criteria
        } else {
          NA_integer_
        }
        required <- if ("required" %in% names(spec)) spec$required else TRUE
      } else {
        stop("Landmark '", name, "' must be a type string or a named list")
      }
      landmark(
        name = name,
        criteria = criteria,
        channel = channel,
        required = required
      )
    })
  }

  landmark_names <- vapply(specs, function(x) x@name, character(1))
  if (anyDuplicated(landmark_names)) {
    stop("Landmark names must be unique")
  }

  if (is_EGM(x)) {
    examples <- list(x)
  } else if (is.list(x) && length(x) > 0L &&
    all(vapply(x, is_EGM, logical(1)))) {
    examples <- x
  } else {
    stop("`x` must be an EGM or a non-empty list of EGM examples")
  }
  if (is.null(frequency)) {
    frequency <- as.numeric(attributes(examples[[1]]$header)$record_line$frequency)
  }

  fractions <- matrix(
    NA_real_,
    nrow = length(examples),
    ncol = length(specs),
    dimnames = list(NULL, landmark_names)
  )
  for (i in seq_along(examples)) {
    egm <- examples[[i]]
    samples <- as.numeric(egm$signal$sample)
    if (length(samples) < 2L || any(!is.finite(samples)) ||
      any(diff(samples) <= 0)) {
      next
    }
    annotations <- get_single_annotation(egm)
    for (j in seq_along(specs)) {
      spec_channel <- resolve_channel_spec(egm, specs[[j]]@channel)
      rows <- match_features(annotations, specs[[j]]@criteria, spec_channel)
      matches <- as.integer(rows$sample)
      if (length(matches) > 1L && ambiguous == "error") {
        # The count rising with the number of leads is the clue that the
        # annotator was run per lead, so say so rather than report the symptom
        stop(
          "Landmark '", landmark_names[j], "' ",
          describe_matches(rows, where = paste0(" in example ", i)),
          call. = FALSE
        )
      }
      if (length(matches) > 0L) {
        fractions[i, j] <-
          (matches[1] - samples[1]) / (samples[length(samples)] - samples[1])
      }
    }
  }

  fractions[fractions < 0 | fractions > 1] <- NA_real_
  crossed <- apply(fractions, 1L, function(row) {
    observed <- row[!is.na(row)]
    length(observed) > 1L && any(diff(observed) <= 0)
  })
  if (any(crossed)) {
    if (order_policy == "error") {
      stop(
        "Landmarks are crossed or duplicated in example(s): ",
        paste(which(crossed), collapse = ", ")
      )
    }
    fractions[crossed, ] <- NA_real_
  }

  required <- vapply(specs, function(x) x@required, logical(1))
  incomplete <- if (any(required)) {
    apply(fractions[, required, drop = FALSE], 1L, anyNA)
  } else {
    rep(FALSE, nrow(fractions))
  }
  if (missing == "error" && any(incomplete)) {
    stop(
      "Required landmarks are missing in example(s): ",
      paste(which(incomplete), collapse = ", ")
    )
  }
  if (missing == "complete") {
    fractions[incomplete, ] <- NA_real_
  }

  counts <- colSums(!is.na(fractions))
  if (any(required & counts == 0L)) {
    stop(
      "Landmark(s) never located: ",
      paste(landmark_names[required & counts == 0L], collapse = ", ")
    )
  }
  fitted <- counts > 0L
  if (!any(fitted)) {
    stop("No examples remained after landmark quality control")
  }
  if (any(!fitted)) {
    warning(
      "Optional landmark(s) omitted because they were never found: ",
      paste(landmark_names[!fitted], collapse = ", ")
    )
    specs <- specs[fitted]
    landmark_names <- landmark_names[fitted]
    fractions <- fractions[, fitted, drop = FALSE]
    counts <- counts[fitted]
  }

  estimate <- if (position_estimator == "median") stats::median else base::mean
  phase <- vapply(
    seq_along(specs),
    function(j) estimate(fractions[, j], na.rm = TRUE),
    numeric(1)
  )
  phase_mad <- vapply(seq_along(specs), function(j) {
    if (counts[j] < 2L) NA_real_ else stats::mad(fractions[, j], na.rm = TRUE)
  }, numeric(1))
  positions <- phase * (target_samples - 1L)
  order <- order(positions)
  fitted_landmarks <- lapply(order, function(i) {
    landmark(
      name = specs[[i]]@name,
      criteria = specs[[i]]@criteria,
      channel = specs[[i]]@channel,
      position = positions[[i]],
      required = specs[[i]]@required
    )
  })
  statistics <- data.frame(
    name = landmark_names,
    n = as.integer(counts),
    phase = phase,
    phase_mad = phase_mad,
    position = as.numeric(positions),
    stringsAsFactors = FALSE
  )
  statistics <- statistics[order, , drop = FALSE]
  rownames(statistics) <- NULL

  template(
    landmarks = fitted_landmarks,
    target_samples = target_samples,
    method = "learned",
    frequency = frequency,
    statistics = statistics
  )
}
