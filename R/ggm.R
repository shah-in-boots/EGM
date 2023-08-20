# `ggplot` Class ---------------------------------------------------------------

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
	stopifnot("No requested channels existed within the signal data" =
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
			sig[, c('sample', 'time', ..selectedChannels)],
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
		scale_x_continuous(breaks = seq(sampleStart, sampleEnd, by = hz / 10), label = NULL)

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

# Annotations ------------------------------------------------------------------

#' Add annotations
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Annotations are labels for specific points or samples within a signal. They
#' can be semantic, in that they may represent the boundary of a region of the
#' signal, or just an individual peak. They are stored as a WFDB-compatible
#' annotation file built into a `ggm` object.
#' @name annotations
NULL

#' @rdname annotations
#' @export
draw_boundary_mask <- function(object) {

	# Initial validation
	stopifnot("`add_boundary_mask()` requires a `ggm` object" =
							inherits(object, "ggm"))

	# Annotations from object will already be subset to relevant sample space
	data <- copy(object$data)
	ann <- copy(attributes(object)$annotation)
	type <- attributes(ann)$annotator

	# TODO Widen annotations based on wave type
	# Once wide, will be RANGE + Wave (e.g. 1-50 = P wave)
	# If wide can label segments by changing line color
	if (type == 'ecgpuwave') {
	 	# Rename onset and offset
	 	# Collect waveform locations and name them
	 	# Add groups of beats
		bounds <-
			ann[, type := ifelse(type == '(', 'onset', type)
			][, type := ifelse(type == ')', 'offset', type)
			][, wave := number # Create new column for wave type
			][, wave := ifelse(number == 0, 'p', wave)
			][, wave := ifelse(number == 1, 'qrs', wave)
			][, wave := ifelse(number == 2, 't', wave)
			][type %in% c('onset', 'offset'), .(sample, type, wave)
			][, beat := seq_len(.N), by = .(wave, type)
			][, beat := paste0(wave, beat)] |> # Beats by group
			dcast(beat + wave ~ type, value.var = 'sample', drop = TRUE) |>
			setkey(beat)

		# Merge in region data
		data[bounds,
				 on = .(sample >= onset, sample < offset),
				 c('wave', 'beat') := list(wave, beat)
		][is.na(wave), wave := NA_character_]

		# Change colors to fit
		data[wave == 'background', bounds := NA_character_,
		][wave == 'p', bounds := 'darkgoldenrod1'
		][wave == 'qrs', bounds := 'skyblue4'
		][wave == 't', bounds := 'indianred3']

	}

	if (type %in% tolower(.leads$ECG)) {

		# Label..
		leadLab <- toupper(type)

	 	# Get data bounds
		bounds <-
			ann[, type := ifelse(type == '(', 'onset', type)
			][, type := ifelse(type == ')', 'offset', type)
			][, start := shift(type, type = 'lead')
			][, end := shift(type, type = 'lag')
			][type == 'onset', wave := start
			][type == 'offset', wave := end
			][, wave := ifelse(wave == 'p', 'p', wave)
			][, wave := ifelse(wave == 'N', 'qrs', wave)
			][, wave := ifelse(wave == 't', 't', wave)
			][type %in% c('onset', 'offset'), .(sample, type, wave)
			][, label := leadLab # Add labels for future merge
			][, beat := seq_len(.N), by = .(wave, type)
			][, beat := paste0(wave, beat)] |> # Beats by group
			dcast(label + beat + wave ~ type, value.var = 'sample', drop = TRUE) |>
			setkey(beat)

		# Merge in region data
		data[bounds,
			on = .(label, sample >= onset, sample < offset),
			c('wave', 'beat') := list(wave, beat)
		][is.na(wave), wave := NA_character_]

		# Change colors to fit
		data[wave == 'background', bounds := NA_character_,
		][wave == 'p', bounds := 'darkgoldenrod1'
		][wave == 'qrs', bounds := 'skyblue4'
		][wave == 't', bounds := 'indianred3']

	 }

	# Update object data
	g <- object
	g$data <- data

	gg <-
		g +
		geom_line(
			aes(
				x = sample,
				y = mV,
				color = bounds,
				group = beat
			),
			linewidth = 2,
			alpha = 0.5
		)

	new_ggm(
		object = gg,
		header = attributes(object)$header,
		annotation = attributes(object)$annotation
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
	channel <- gsub("\ ", "_", x = channel)
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

# Themes/colors ----------------------------------------------------------------

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
