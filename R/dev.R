# nocov start

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

	lifecycle::signal_stage("experimental", "draw_boundary_mask()")

	# Null variables
	. <- beat <- onset <- offset <- start <- end <- label <- type <- mV <- wave <- number <- NULL

	# Initial validation
	stopifnot("`draw_boundary_mask()` requires a `ggm` object" =
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

	# Works for when individual leads are considered as the annotations
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
#' @return Returns an updated `ggm` object
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
#' @export
add_intervals <- function(object,
													intervals = TRUE,
													channel,
													minimum_interval = 100) {

	label <- type <- NULL

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
			MinPeakHeight = stats::quantile(ann$mV, probs = 0.99),
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
#' Using `add_colors()` is part of the theme process for a `ggm` object,
#' which in turn is a visual representation of an `egm` object. Often, the
#' `egm` dataset will contain default colors based on where the signal data
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
#' @param object A `ggm` object
#'
#' @return Returns an updated `ggm` object
#'
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

# nocov end
