#' Visualization of EGMs using `ggplot`
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param data Data of the `egm` class, which inclues header (meta) and signal
#'   information together.
#'
#' @param channels Character vector of which channels to use. Can give either
#'   the channel label (e.g "CS 1-2") or the recording device/catheter type (e.g
#'   "His" or "ECG").
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
								channels,
								time_frame = NULL,
								mode = "dark",
								...) {

	stopifnot(inherits(data, "egm"))

	hea <- attr(data, "hea")
	hea$LABEL <- as.character(hea$LABEL)
	ann <- attr(data, "annotation")
	signal <- data.table::as.data.table(data)
	names(signal) <- hea$LABEL

	# Should be all of the same frequency of data
	hz <- hea$FREQUENCY
	signal$index <- 1:nrow(signal)
	signal$time <- signal$index / hz

	# check if time frame exists within series, allowing for
	# indexed rounding based on frequency
	if (is.null(time_frame)) {
		time_frame <- c(min(signal$time, na.rm = TRUE), max(signal$time, na.rm = TRUE))
	}
	stopifnot("`time_frame` must be within available data" = all(
		min(time_frame) + 1 / hz >= min(signal$time) &
			max(time_frame) - 1 / hz <= max(signal$time)
	))

	# Filter time appropriately
	startTime <- time_frame[1]
	endTime <- time_frame[2]

	# Make sure appropriate channels are selected
	availableChannels <- names(signal)[1:(ncol(signal) - 2)] # Removes index/time cols
	exactChannels <- channels[channels %in% .labels]
	fuzzyChannels <- channels[!(channels %in% .labels)]
	channelGrep <-
		paste0(c(paste0("^", exactChannels, "$", collapse = "|"), fuzzyChannels),
					 collapse = "|")
	selectedChannels <- grep(channelGrep, availableChannels, value = TRUE)
	stopifnot("No requested channels existed within the signal data" =
							length(selectedChannels) > 0)

	# Get channel data from individual signals
	# Need to make sure all that information is present from header
	channelData <-
		hea[c("NUMBER", "LABEL", "SOURCE", "LEAD", "COLOR")] |>
		as.data.table()
	names(channelData) <- tolower(names(channelData))
	if (is.null(channelData$color)) {
		channelData$color <- '#FFFFFF'
	}

	dt <-
		data.table::melt(
			signal[, c(..selectedChannels, "index", "time")],
			id.vars = c("index", "time"),
			variable.name = "label",
			value.name = "mV"
		) |>
		{
			\(.x)
			channelData[.x, on = .(label)
									][time >= startTime & time <= endTime
									][, mV := as.numeric(mV)]
		}()

	# relevel because order is lost in the labels during transformation
	dt$label <-
		factor(dt$label,
					 levels = intersect(.labels, selectedChannels),
					 ordered = TRUE)

	g <-
		ggplot(dt, aes(x = time, y = mV, color = color)) +
		geom_line() +
		facet_wrap( ~ label,
								ncol = 1,
								scales = "free_y",
								strip.position = "left") +
		scale_color_identity() +
		theme_egm_dark() +
		scale_x_continuous(breaks = seq(time_frame[1], time_frame[2], by = 0.2),
											 label = NULL)

	# Return with updated class
	new_ggm(g,
					header = hea,
					annotation = ann)

}

#' Construct `ggm` class
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

#' Add intervals
#'
#' @param intervals The choice of whether interval data will be included. An
#'   annotation channel must be present, otherwise nothing will be plotted. This
#'   argument allows several choices.
#'
#'   * __TRUE__: all intervals will be annotated (default option)
#'
#'   * __integer__: an integer vector that represents the indexed intervals that
#'   should be annotated. If NULL, no intervals will be annotated. For example,
#'   if their are 5 beats, than there are 4 intervals between them that can be
#'   labeled. They can be referenced by their index, e.g. `intervals = c(2)` to
#'   reference the 2nd interval in a 5 beat range.
#'
#' @return Returns an updated `ggm` object
#' @export
add_intervals <- function(object,
													intervals = TRUE,
													channel,
													minimum_interval = 100) {

	# Initial validation
	stopifnot("`add_intervals()` requires a `ggm` object" =
							inherits(object, "ggm"))

	# Get channels and check
	dt <- object$data
	chs <- attributes(object)$header$label
	stopifnot("The channel must be in the plot to annotate." = channel %in% chs)
	hz <- attributes(object)$header$frequency

	# Subset data for an annotation channel
	ann <- dt[label == channel]
	t <- ann$index
	peaks <-
		gsignal::findpeaks(
			ann$mV,
			MinPeakHeight = quantile(ann$mV, probs = 0.99),
			MinPeakDistance = minimum_interval,
			DoubleSided = TRUE
		)

	# Find peaks and intervals, and trim data for mapping purposes
	pk_locs <- ann$time[peaks$loc]
	ints <- diff(peaks$loc)
	ints_locs <- pk_locs[1:length(ints)] + ints/(2 * hz)
	ht <- mean(abs(peaks$pks), na.rm = TRUE)

	dt_locs <- round(ints_locs*hz, digits = 0)
	dt_ann <- ann[dt_locs, ]

	# Interval validation
	stopifnot(inherits(as.integer(intervals), "integer"))
	stopifnot("Intervals not available based on number of beats." =
							all(intervals %in% seq_along(ints)))

	# Color choice for text annotation
	bg <- object$theme$plot.background$fill
	txtColor <- ifelse(bg == "black", "white", "black")

	# For all intervals
	if (isTRUE(intervals)) {

		n <- seq_along(ints)

		gtxt <- lapply(n, function(.x) {
			geom_text(
				data = dt_ann,
				aes(
					x = ints_locs[.x],
					y = ht / 2,
					label = ints[.x]
				),
				color = txtColor,
				inherit.aes = FALSE
			)
		})

	# Selected intervals
	} else if (inherits(as.integer(intervals), "integer")) {

		gtxt <- lapply(intervals, function(.x) {
			geom_text(
				data = dt_ann,
				aes(
					x = ints_locs[.x],
					y = ht / 2,
					label = ints[.x]
				),
				color = txtColor,
				inherit.aes = FALSE
			)
		})

	}

	# Return updated plot
	object + gtxt
}


#' Add color scheme to a `ggm` object
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

#' Custom theme for EGM data
#' @keywords internal
#' @noRd
theme_egm_light <- function() {
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

#' Custom theme for EGM data
#' @keywords internal
#' @noRd
theme_egm_dark <- function() {
	font <- "Arial"
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
		)
}
