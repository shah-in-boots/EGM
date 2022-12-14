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

	header <- .pull_header(data)
	signal <- .pull_signal(data)
	channels <- .pull_channels(data)

	# Should be all of the same frequency of data
	hz <- header$freq
	signal$index <- 1:nrow(signal)
	signal$time <- signal$index / hz

	# Check if time frame exists within series
	if (is.null(time_frame)) {
		time_frame <- c(min(signal$time, na.rm = TRUE), max(signal$time, na.rm = TRUE))
	}
	stopifnot("`time_frame` must be within available data" =
							all(time_frame %in% signal$time))

	# Filter appropriately
	start_time <- time_frame[1]
	end_time <- time_frame[2]
	ch <- channels$label
	surface_leads <- c("I", "II", "III", paste0("V", 1:6), "AVF", "AVR", "AVR")
	ch_exact <- ch[which(ch %in% surface_leads | grepl("\ ", ch))]
	ch_fuzzy <- ch[which(!(ch %in% ch_exact))]
	ch_grep <-
		paste0(c(paste0("^", ch_exact, "$", collapse = "|"), ch_fuzzy),
					 collapse = "|")

	# Lengthen for plotting
	dt <-
		melt(
			signal,
			id.vars = c("index", "time"),
			variable.name = "label",
			value.name = "mV"
		) |>
		{\(.x)
			channels[.x, on = .(label)]
		}() |>
		{\(.y)
			.y[grepl(ch_grep, label),
					][time >= start_time & time <= end_time,
					]
		}()


	# Relevel
	dt$label <- factor(dt$label, levels = levels(channels$label))

	# Final channels
	chs <- unique(dt$label)

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
	new_ggm(g)

}

#' Construct `ggm` class
new_ggm <- function(object = ggplot()) {

	stopifnot(is.ggplot(object))

	structure(
		object,
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
	stopifnot("`add_intervals()` requires a `ggm` object" = inherits(object, "ggm"))

	# Get channels and check
	dt <- object$data
	chs <- unique(dt$label)
	stopifnot("The channel must be in the plot to annotate." = channel %in% chs)
	hz <- unique(dt$freq)

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
				color = "black",
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
				color = "black",
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


#' Modify channel colors
#'
#' @param palette Color palette options for leads as below:
#'
#'   * __material__ - material color theme
#'
#'   * __bw__ - black or white color theme based on light/dark mode
#'
#' @param mode Adjust color settings for either light or dark mode
#' @return Character vector of hex code colors based on the selected palette and
#'   light/dark mode
#' @export
color_channels <- function(x, palette, mode = "light") {

	# Requires a character vector that has appropriate ECG lead types
	# Need unique levels
	stopifnot("Requires a `character` or `factor` input" = inherits(x, c("character", "factor")))
	y <- as.character(unique(x))

	# Lead/source labels as below
	surface <-
		c("I", "II", "III", "AVF", "AVL", "AVR", paste0("V", 1:6))
	lead_pairs <-
		paste0(seq(from = 1, to = 19, by = 2), "-", seq(from = 2, to = 20, by = 2))
	lead_loc <- c("D", "DIST", "DISTAL",
								"M", "MID", "MIDDLE",
								"P", "PROX", "PROXIMAL")
	hra <- paste("RA", lead_pairs[1:2])
	his <- paste("HIS", c(lead_pairs[1:3], lead_loc))
	cs <- paste("CS", lead_pairs[1:5])
	dd <- paste("DD", lead_pairs[1:10])
	rv <- paste("RV", lead_pairs[1:2])
	abl <- paste("ABL", c(lead_pairs[1:2], lead_loc[c(1:3, 7:9)]))
	lead_order <- c(surface, rev(c(lead_pairs, lead_loc)))
	label_order <- c(surface, hra, his, cs, dd, rv, abl)
	source_order <- c("ECG", "HRA", "RA", "HIS", "CS", "DD", "RV", "ABL")
	leads <- list(
		ECG = c("ECG", surface),
		HRA = c("HRA", hra),
		HIS = c("HIS", his),
		CS = c("CS", cs),
		DD = c("DD", dd),
		RV = c("RV", rv),
		ABL = c("ABL", abl)
	)

	# Colors in light to dark sequence
	# Includes surface, His, chambers (RA/RV and ablation), CS (and DD)
	# Each has an option for 10 colors (max)
	switch(
		palette,
		material = {

			colors <- list(
				# Yellow
				HIS =
					c(
						"#FFFDE6",
						"#FFF8C4",
						"#FFF49D",
						"#FFF176",
						"#FFED58",
						"#FFEB3A",
						"#FDD834",
						"#FABF2C",
						"#F8A725",
						"#F47F17"
					),

				# RA and RV leads are pink
				RA =
					c(
						"#FCE4EB",
						"#F8BAD0",
						"#F38EB1",
						"#F06192",
						"#EB3F79",
						"#E91E63",
						"#D81A5F",
						"#C1185A",
						"#AC1357",
						"#870D4E"
					),

				# RV
				RV =
					c(
						"#FCE4EB",
						"#F8BAD0",
						"#F38EB1",
						"#F06192",
						"#EB3F79",
						"#E91E63",
						"#D81A5F",
						"#C1185A",
						"#AC1357",
						"#870D4E"
					),

				# Ablation
				ABL =
					c(
						"#FCE4EB",
						"#F8BAD0",
						"#F38EB1",
						"#F06192",
						"#EB3F79",
						"#E91E63",
						"#D81A5F",
						"#C1185A",
						"#AC1357",
						"#870D4E"
					),

				# Surface leads are blue
				ECG = rep(
					c(
						"#E3F2FD",
						"#BADEFA",
						"#90CAF8",
						"#64B4F6",
						"#41A5F4",
						"#2096F2",
						"#1E87E5",
						"#1976D2",
						"#1465BF",
						"#0C46A0"
					),
					each = 2
				),

				### Green = Extended length multipolar, such as DD or CS

				# Decapolar / coronary sinus
				CS = c(
					"#DFF2F1",
					"#B2DFDA",
					"#7FCBC4",
					"#4CB6AC",
					"#26A599",
					"#009687",
					"#00887A",
					"#00796B",
					"#00685B",
					"#004C3F"
				),

				# Duodecapolar catheter
				DD = rep(
					c(
						"#DFF2F1",
						"#B2DFDA",
						"#7FCBC4",
						"#4CB6AC",
						"#26A599",
						"#009687",
						"#00887A",
						"#00796B",
						"#00685B",
						"#004C3F"
					),
					each = 2
				)
			)

		}
	)

	# Create a table of the potential lead types
	z <- y
	for (i in names(leads)) {
		for (j in seq_along(y)) {
			if (y[j] %in% leads[[i]]) {
				z[j] <- i
			}
		}
	}

	# Table them to know how many colors to select
	chs <- as.list(y)
	names(chs) <- z
	clrs <- as.list(table(z))

	# Apply colors based on mode to a template of the leads
	if (mode == "light") {

		for (i in names(clrs)) {

			clrs[[i]] <- rev(colors[[i]])[seq(clrs[[i]])]
		}

	} else if (mode == "dark") {

		for (i in names(clrs)) {
			clrs[[i]] <- colors[[i]][seq(clrs[[i]])]
		}

	}

	# Apply back to original template
	for (i in names(clrs)) {
		chs[names(chs) == i] <- clrs[[i]]
	}

	# Rename the channels with values being the colors
	names(chs) <- y
	new_colors <- as.character(x)

	for (i in names(chs)) {
		new_colors[new_colors == i] <- chs[[i]]
	}

	# Return
	new_colors

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
