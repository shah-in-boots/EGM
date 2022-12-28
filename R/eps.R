### Class Definitions ----------------------------------------------------------

#' Electrophysiology eps class definition
#'
#' @name eps
#' @export
eps <- function(x = double(),
								frequency = 1000L,
								voltage = "mV",
								label = character(),
								color = "#FFFFFF",
								...) {

	# If empty
	if (vec_size(x) == 0) {
		return(new_eps())
	}


	# Cast appropriately (factors will not work as they are not base=numeric)
	x <- vec_cast(x, double())
	frequency <- vec_cast(frequency, integer())
	voltage <- vec_cast(voltage, character())
	color <- vec_cast(color, character())
	label <- vec_cast(label, .labels)

	# Obbtain label source
	source <- identify_channel_source(label)
	source <- vec_cast(source, .source)

	# Class initialization
	new_eps(
		x,
		frequency = frequency,
		voltage = voltage,
		source = source,
		label = label,
		color = color
	)
}


#' @export
new_eps <- function(x = double(),
										frequency = integer(),
										voltage = character(),
										source = factor(),
										label = factor(),
										color = character()) {

	# Validate
	vec_assert(x, double())
	vec_assert(frequency, integer())
	vec_assert(voltage, character())
	# vec_assert(source, .source)
	# vec_assert(label, .labels)
	vec_assert(color, character())

	# Vector with attributes included
	new_vctr(x,
					 frequency = frequency,
					 voltage = voltage,
					 source = source,
					 label = label,
					 color = color,
					 class = "eps")
}

#' @rdname eps
#' @export
is_eps <- function(x) {
	inherits(x, "eps")
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("eps", "vctrs_vctr"))

#' @export
vec_ptype_abbr.eps <- function(x, ...) "eps"

#' @export
vec_ptype_full.eps <- function(x, ...) "electrical_signal"

#' @export
vec_ptype2.eps.eps <- function(x, y, ...) new_eps()

#' @export
vec_ptype2.eps.double <- function(x, y, ...) double()

#' @export
vec_ptype2.double.eps <- function(x, y, ...) double()

#' @export
vec_cast.eps.eps <- function(x, to, ...) x

#' @export
vec_cast.eps.double <- function(x, to, ...) eps(x, ...)

#' @export
vec_cast.double.eps <- function(x, to, ...) vec_data(x)


#' @export
format.eps <- function(x, ...) {
	if (vec_size(x) == 0) {
		new_eps()
	} else {
		formatC(vec_data(x), digits = 2, format = "f", flag = "0+- ")
	}
}

#' @export
obj_print_data.eps <- function(x, ...) {
	if (vec_size(x) == 0) {
		new_eps()
	} else if (vec_size(x) > 0) {
		print(format(x), quote = FALSE)
	}
}

### Helpers -------------------------------------------------------------------

#' @export
identify_channel_source <- function(x) {

	# Intakes character vector and identifies the source
	stopifnot("Not a known/supported channel yet." = x %in% .labels)

	# Find source of lead bipole
	for (i in names(.leads)) {
		if (x  %in% .leads[[i]]) {
			y <- i
		}
	}

	# Return
	y
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

	# Requires a character/factor that has appropriate ECG lead types
	stopifnot("Requires a `character` or `factor` input" =
							inherits(x, c("character", "factor")))
	stopifnot("All labels must be appropriate ECG or EGM leads" =
							all(x %in% .labels))

	# Needs unique levels for re-coloring
	uniqueLeads <- as.character(unique(x))

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
	leadSource <- uniqueLeads
	for (i in names(.leads)) {
		for (j in seq_along(uniqueLeads)) {
			if (uniqueLeads[j] %in% .leads[[i]]) {
				leadSource[j] <- i
			}
		}
	}

	# Table them to know how many colors to select
	leadList <- as.list(uniqueLeads)
	names(leadList) <- leadSource
	sourceTally <- as.list(table(leadSource))

	# Apply colors based on mode to a template of the leads
	if (mode == "light") {
		for (i in names(sourceTally)) {
			sourceTally[[i]] <- rev(colors[[i]])[seq(sourceTally[[i]])]
		}
	} else if (mode == "dark") {
		for (i in names(sourceTally)) {
			sourceTally[[i]] <- colors[[i]][seq(sourceTally[[i]])]
		}
	}

	# Apply back to original template
	for (i in names(sourceTally)) {
		leadList[names(leadList) == i] <- sourceTally[[i]]
	}

	# Rename the channels with values being the colors
	names(leadList) <- uniqueLeads
	newColors <- as.character(x)

	for (i in names(leadList)) {
		newColors[newColors == i] <- leadList[[i]]
	}

	# Return
	newColors

}

#' Get components of EPS data
#' @param x signal data from `eps` class
#' @name epsGetters
#' @return Request value based on attribute that is being called
#' @export
get_frequency <- function(x) {
	stopifnot("Object must be an `eps` class" = inherits(x, "eps"))
	attr(x, "frequency")
}

#' @rdname epsGetters
#' @export
get_voltage <- function(x) {
	stopifnot("Object must be an `eps` class" = inherits(x, "eps"))
	attr(x, "voltage")
}

#' @rdname epsGetters
#' @export
get_source <- function(x) {
	stopifnot("Object must be an `eps` class" = inherits(x, "eps"))
	attr(x, "source")
}


#' @rdname epsGetters
#' @export
get_label <- function(x) {
	stopifnot("Object must be an `eps` class" = inherits(x, "eps"))
	attr(x, "label")
}

#' @rdname epsGetters
#' @export
get_color <- function(x) {
	stopifnot("Object must be an `eps` class" = inherits(x, "eps"))
	attr(x, "color")
}
