# `ggplot` Class ---------------------------------------------------------------

#' Visualization of EGMs using `ggplot`
#'
#' @param data Data of the `egm` class, which inclues header (meta) and signal
#'   information together.
#'
#' @param channels Character vector of which channels to use. Can give either
#'   the channel label (e.g "CS 1-2") or the recording device/catheter type (e.g
#'   "His" or "ECG"). If no channels are selected, the default is all channels.
#'
#' @param time_frame A time range that should be displaced given in the format
#'   of a vector with a length of 2. The left value is the start, and right
#'   value is the end time. This is given in seconds (decimals may be used).
#'
#' @param mode The base color scheme to be used. Defaults to the a "white on
#'   black" scheme, similar to that of _LabSystem Pro_ format (and most other
#'   high-contrast visualizations), for minimizing eye strain.
#'
#' @import ggplot2 data.table
#' @export
ggm <- function(data,
								channels = character(),
								time_frame = NULL,
								mode = "dark",
								...) {

	# Global variables (used in data.table)
	. <- color <- mV <- label <- NULL

	stopifnot(inherits(data, "egm"))

	# Clean channels
	channels <- gsub("\ ", "_", x = channels)

	# Process header and signal
	hea <- data$header
	ann <- data$annotation
	sig <- data.table::as.data.table(data$signal)
	hea$label <- as.character(hea$label)
	names(sig) <- c('sample', hea$label)

	# Should be all of the same frequency of data
	hz <- attributes(hea)$record_line$frequency
	sig$time <- sig$sample / hz

	# check if time frame exists within series, allowing for
	# indexed rounding based on frequency
	if (is.null(time_frame)) {
		time_frame <- c(min(sig$time, na.rm = TRUE), max(sig$time, na.rm = TRUE))
	}
	stopifnot("`time_frame` must be within available data" = all(
		min(time_frame) + 1 / hz >= min(sig$time) &
			max(time_frame) - 1 / hz <= max(sig$time)
	))

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
		paste0(c(paste0("^", exactChannels, "$", collapse = "|"), fuzzyChannels),
					 collapse = "|")
	selectedChannels <- grep(channelGrep, availableChannels, value = TRUE)
	if (length(channels) == 0) {
		selectedChannels <- availableChannels
	}
	stopifnot("The requested channels do not exist within the signal data" =
							length(selectedChannels) > 0)

	# Get channel data from individual signals
	# Need to make sure all that information is present from header
	channelData <-
		hea[, c("label", "source", "lead", "color")] |>
		as.data.table()
	if (is.null(channelData$color)) {
		channelData$color <- '#000000'
	}


	dt <-
		data.table::melt(
			sig[, c('sample', 'time', selectedChannels), with = FALSE],
			id.vars = c("sample", "time"),
			variable.name = "label",
			value.name = "mV"
		) |>
		{
			\(.x)
			channelData[.x, on = .(label)
									][, mV := as.numeric(mV)]
		}()

	# Relevel because order is lost in the labels during transformation
	# But only do this if the labels are... "official" and not custom labels
	if (all(selectedChannels %in% .labels)) {
		dt$label <-
			factor(dt$label,
						 levels = intersect(.labels, selectedChannels),
						 ordered = TRUE)
	} else {
		dt$label <- factor(dt$label)
	}

	# TODO need to tweak plotting parameter for hertz
	g <-
		ggplot(dt, aes(x = sample, y = mV, color = color)) +
		geom_line() +
		facet_wrap( ~ label,
								ncol = 1,
								scales = "free_y",
								strip.position = "left") +
		scale_color_identity() +
		theme_egm() +
		scale_x_continuous(breaks = seq(sampleStart, sampleEnd, by = hz / 10), labels = NULL)

	# Return with updated class
	new_ggm(g,
					header = hea,
					annotation = ann)

}

new_ggm <- function(object = ggplot(),
										header = list(),
										annotation = annotation_table()) {

	stopifnot(is.ggplot(object))

	structure(
		object,
		header = header,
		annotation = annotation,
		class = c("ggm", class(object))
	)
}

# Annotations ------------------------------------------------------------------


# Themes/colors ----------------------------------------------------------------

#' Add color scheme to a `ggm` object
#'
#' Using `add_colors()` is part of the theming process for a `<ggm>` object,
#' which in turn is a visual representation of an `<egm>` object. Often, the
#' `<egm>` dataset will contain default colors based on where the signal data
#' was brought in from. `add_colors()` can allow customization of those features
#' to some degree based on *opinionated* color palettes.
#'
#' @details
#' Currently, the color choices are individual decided based on the channel
#' source (e.g. lead) and are inspired by some modern palettes. The eventual
#' goal for this function is to accept a multitude of palette options using
#' heuristics similar to what is found in `{ggplot2}` or other graphing
#' packages.
#'
#' @inheritParams color_channels
#'
#' @return Returns an updated `ggm` object
#' @export
add_colors <- function(object, palette, mode = "light") {

	stopifnot("Requires `ggm` class" = inherits(object, "ggm"))

	# Extract data from ggplot
	dt <- object$data
	dt$color <- color_channels(dt$label, palette = palette, mode = mode)
	object$data <- dt

	# Depends on mode to add or update theme
	if (mode == "light") {
		object + theme_egm_light()
	} else if (mode == "dark") {
		object + theme_egm_dark()
	} else {
		message("Return unmodified `ggm` plot object")
		object
	}


}

#' Theming and color options for `ggm` objects
#'
#' @description
#'
#' The general purpose is to improve visualization of electrical signals. There
#' is a pattern of colors that are generally given from different recording
#' software, and they can be replicated to help improve visibility.
#'
#' @name colors
NULL

#' @rdname colors
#' @export
theme_egm <- function() {
	font <- "Arial"
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
}


#' @rdname colors
#' @export
theme_egm_light <- function() {
	font <- "Arial"
	list(
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
			),
		scale_color_manual(values = 'black')
	)
}

#' @rdname colors
#' @export
theme_egm_dark <- function() {
	font <- "Arial"

	list(
		theme_minimal() %+replace%
			theme(

				# Panels and background
				panel.grid.major.y = element_blank(),
				panel.grid.minor.y = element_blank(),
				panel.grid.major.x = element_blank(),
				panel.grid.minor.x = element_blank(),
				panel.background = element_rect(fill = "black"),
				plot.background = element_rect(fill = "black"),

				# Axes
				axis.ticks.y = element_blank(),
				axis.title.y = element_blank(),
				axis.text.y = element_blank(),
				axis.title.x = element_blank(),
				axis.text.x = element_text(color = "white"),
				axis.ticks.x = element_line(color = "white"),

				# Facets
				panel.spacing = unit(0, units = "npc"),
				strip.text.y.left = element_text(angle = 0, hjust = 1, color = "white"),

				# Legend
				legend.position = "none"
			),
		scale_color_manual(values = 'white')
	)
}
