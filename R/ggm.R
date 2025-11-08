# `ggplot` Class ---------------------------------------------------------------

#' Visualization of EGMs using `ggplot`
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' The `ggm()` function is used to plot objects of the `egm` class. This
#' function however is more than just a plotting function - it serves as a
#' visualization tool and confirmation of patterns, annotations, and underlying
#' waveforms in the data. The power of this, instead of being a `geom_*()`
#' object, is that annotations, intervals, and measurements can be added
#' incrementally.
#'
#' @param data Data of the `egm` class, which includes header (meta) and signal
#'   information together.
#'
#' @param channels A `character` vector of which channels to use. Can give
#'   either the channel label (e.g "CS 1-2") or the recording device/catheter
#'   type (e.g "His" or "ECG"). If no channels are selected, the default is all
#'   channels.
#'
#' @param time_frame A time range that should be displaced given in the format
#'   of a vector with a length of 2. The left value is the start, and right
#'   value is the end time. This is given in seconds (decimals may be used).
#'
#' @param ... Additional arguments to be passed to the function
#'
#' @returns An `{ggplot2}` compatible object with the `ggm` class, which
#'   contains additional elements about the header and annotations of the
#'   original data.
#'
#' @import ggplot2 data.table
#' @export
ggm <- function(
  data,
  channels = character(),
  time_frame = NULL,
  palette = NULL,
  ...
) {
  # Global variables (used in data.table)
  . <- color <- mV <- label <- NULL

  stopifnot(inherits(data, "egm"))

  # Clean channels
  channels <- gsub("_", "\ ", x = channels)

  # Process header and signal
  hea <- data$header
  # Get first annotation from list (or empty if none)
  ann_list <- data$annotation
  if (length(ann_list) > 0) {
    ann <- ann_list[[1]]
    if (length(ann_list) > 1) {
      message(
        "Multiple annotators found: ",
        paste(names(ann_list), collapse = ", "),
        ". Using '",
        names(ann_list)[1],
        "' for plotting. ",
        "Use get_annotation() to access other annotators."
      )
    }
  } else {
    ann <- annotation_table()
  }
  sig <- data.table::as.data.table(data$signal)
  hea$label <-
    as.character(hea$label) |>
    gsub("_", "\ ", x = _)
  names(sig) <- c('sample', hea$label)

  # Should be all of the same frequency of data
  hz <- attributes(hea)$record_line$frequency
  sig$time <- sig$sample / hz

  # check if time frame exists within series, allowing for
  # indexed rounding based on frequency
  if (is.null(time_frame)) {
    time_frame <- c(min(sig$time, na.rm = TRUE), max(sig$time, na.rm = TRUE))
  }
  stopifnot(
    "`time_frame` must be within available data" = all(
      min(time_frame) + 1 / hz >= min(sig$time) &
        max(time_frame) - 1 / hz <= max(sig$time)
    )
  )

  # Filter time appropriately based on samples
  sampleStart <- sig$sample[sig$time == time_frame[1]]
  sampleEnd <- sig$sample[sig$time == time_frame[2]]

  # Trim the signal and annotation files to match the time frame
  sig <- sig[sample >= sampleStart & sample <= sampleEnd, ]
  ann <- ann[sample >= sampleStart & sample <= sampleEnd, ]

  # Make sure appropriate channels are selected
  availableChannels <- hea$label
  exactChannels <- channels[channels %in% .labels]
  fuzzyChannels <- channels[!(channels %in% .labels)]
  channelGrep <-
    paste0(
      c(paste0("^", exactChannels, "$", collapse = "|"), fuzzyChannels),
      collapse = "|"
    )
  selectedChannels <- grep(channelGrep, availableChannels, value = TRUE)
  if (length(channels) == 0) {
    selectedChannels <- availableChannels
  }
  stopifnot(
    "The requested channels do not exist within the signal data" = length(
      selectedChannels
    ) >
      0
  )

  # Get channel data from individual signals
  # Need to make sure all that information is present from header
  channelData <-
    hea[, c("label", "source", "lead", "color")] |>
    as.data.table()

  dt <-
    data.table::melt(
      sig[, c('sample', 'time', selectedChannels), with = FALSE],
      id.vars = c("sample", "time"),
      variable.name = "label",
      value.name = "mV"
    ) |>
    {
      \(.x) {
        channelData[.x, on = .(label)][, mV := as.numeric(mV)]
      }
    }()

  # Relevel because order is lost in the labels during transformation
  # But only do this if the labels are... "official" and not custom labels
  if (all(selectedChannels %in% .labels)) {
    dt$label <-
      factor(
        dt$label,
        levels = intersect(.labels, selectedChannels),
        ordered = TRUE
      )
  } else {
    dt$label <- factor(dt$label)
  }

  # Create final plot
  g <-
    ggplot(dt, aes(x = sample, y = mV, colour = color)) +
    geom_line() +
    facet_wrap(~label, ncol = 1, scales = "free_y", strip.position = "left") +
    scale_colour_identity() +
    scale_x_continuous(
      breaks = seq(sampleStart, sampleEnd, by = hz),
      labels = NULL
    )

  # Respect default header colours while ensuring a contrasting background
  selected_colors <- unique(stats::na.omit(dt$color))
  to_rgb <- function(colour) {
    tryCatch(
      as.numeric(grDevices::col2rgb(colour)),
      error = function(...) rep(NA_real_, 3)
    )
  }
  is_white <- function(colour_vector) {
    !any(is.na(colour_vector)) && all(colour_vector >= 250)
  }
  is_black <- function(colour_vector) {
    !any(is.na(colour_vector)) && all(colour_vector <= 5)
  }
  colour_vectors <- lapply(selected_colors, to_rgb)
  has_white <- any(vapply(colour_vectors, is_white, logical(1)))
  has_black <- any(vapply(colour_vectors, is_black, logical(1)))

  background_mode <- "default"
  if (has_white && !has_black) {
    background_mode <- "dark"
  } else if (has_black) {
    background_mode <- "light"
  }

  # Update class
  g <- new_ggm(g, header = hea, annotation = ann)

  # Apply theme with appropriate background mode
  g <- g + theme_egm(background = background_mode)

  # Return object if available
  g
}

new_ggm <- function(
  object = ggplot(),
  header = list(),
  annotation = annotation_table()
) {
  stopifnot(is_ggplot(object))

  structure(
    object,
    header = header,
    annotation = annotation,
    class = c("ggm", class(object))
  )
}

# Annotations ------------------------------------------------------------------

#' Add annotations to a `ggm` object
#'
#' @description The `add_annotations()` adds annotations to a `ggm` object. It is
#'   specific to this class as it requires the output of [ggm()] to included
#'   data stored in [annotation_table()].
#'
#' @inheritDotParams ggm
#' @export
add_annotations <- function(...) {}

# Colors -----------------------------------------------------------------------

#' Theming and color options for `ggm` objects
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' The general purpose is to improve visualization of electrical signals. There
#' is a pattern of colors that are generally given from different recording
#' software, and they can be replicated to help improve visibility.
#'
#' @returns A `ggm` object, with inheritance similar to
#'   [ggplot2::theme_minimal()]
#'
#' @name colors
NULL

#' @rdname colors
#' @param background A background style to apply. Use `"default"` for the
#'   standard appearance, `"dark"` for a black canvas suited to light traces,
#'   or `"light"` for a white canvas suited to dark traces.
#' @export
theme_egm <- function(background = c("default", "dark", "light")) {
  background <- match.arg(background)
  font <- "Arial"
  base_theme <-
    theme_minimal() %+replace%
    theme(
      # Panels
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),

      # Axes
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_line(),

      # Facets
      panel.spacing = unit(0, units = "npc"),
      panel.background = element_blank(),
      strip.text.y.left = element_text(angle = 0, hjust = 1),

      # Legend
      legend.position = "none"
    )

  if (background == "dark") {
    base_theme <-
      base_theme +
      theme(
        panel.background = element_rect(fill = "black", colour = NA),
        plot.background = element_rect(fill = "black", colour = NA),
        strip.text.y.left = element_text(color = "white"),
        axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_line(color = "white")
      )
  } else if (background == "light") {
    base_theme <-
      base_theme +
      theme(
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA),
        strip.text.y.left = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.ticks.x = element_line(color = "black")
      )
  }

  base_theme
}


#' @rdname colors
#' @export
theme_egm_light <- function() {
  font <- "Arial"
  list(
    theme_egm(background = "light"),
    # If needed to force the colors to be black, can add something like this...
    scale_color_manual(
      values = rep("black", length(.labels)),
      na.value = "black"
    )
  )
}

#' @rdname colors
#' @export
theme_egm_dark <- function() {
  font <- "Arial"

  list(
    theme_egm(background = "dark"),
    # If needed to force the colors to be white, can add something like this...
    scale_color_manual(
      values = rep("white", length(.labels)),
      na.value = "white"
    )
  )
}
