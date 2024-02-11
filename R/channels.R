#' @keywords internal
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
#' @param x <character> Vector of names of requsted ECG or EGM leads
#'
#' @param palette <character> Color palette options for leads as below:
#'
#'   * __material__ - material color theme
#'
#'   * __bw__ - black or white color theme based on light/dark mode
#'
#' @param mode <character> Adjust color settings for either light or dark mode
#'
#' @return Character vector of hex code colors based on the selected palette and
#'   light/dark mode
#'
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
