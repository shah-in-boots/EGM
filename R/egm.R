### Class Definitions ----

#' Electrogram and electrocardiogram data class definition
#'
#' @description
#' This serves as a combinatorial class to describe electrical signal data in R.
#' It is based off of the formats available in WFDB, but has been digitized for
#' ease of use within the R ecosystem. The `egm` object contains three
#' components in a list:
#'
#' * signal data in multiple channels
#' * header information
#' * annotation labels at specified time points
#'
#' These components help to navigate, and visualize data.
#'
#' @details
#' The individual components of the class are further defined in their
#' respective functions [signal_table()], [header_table()],
#' [annotation_table()]. They are very simple classes that build upon the
#' `data.table` class.
#'
#' @name egm
#' @export
egm <- function(signal = signal_table(),
								header = header_table(),
								annotation = annotation_table(),
								...) {

	# Signal data will be in multi-channel format for EPS data, e.g. data.table
	# Header will be  a list

	new_egm(signal,
					header = header,
					annotation = annotation)
}

#' @export
new_egm <- function(signal = signal_table(),
										header = header_table(),
										annotation = annotation_table(),
										...) {

	# Signal will become a data frame (coerced into a data table)
	structure(
		list(
			signal = signal,
			header = header,
			annotation = annotation
		),
		class = c('egm', 'list')
	)

}

#' @export
format.egm <- function(x, ...) {
	hea <- x$header
	rec <- attributes(hea)$record_line
	ann <- x$annotation # May be empty table

	cat('<Electrical Signal>\n')
	cat('-------------------\n')
	cat('Recording Duration: ', rec$samples / rec$frequency, 'seconds\n' )
	cat('Recording frequency ', rec$frequency, ' hz\n')
	cat('Number of channels: ', rec$number_of_channels, '\n')
	cat('Channel Names: ', paste(hea$label), '\n')
	cat('Annotation: ', attributes(ann)$annotator)

}

#' @export
print.egm <- function(x, ...) {
	format(x)
}

#' @export
#' @rdname egm
is_egm <- function(x) {
	inherits(x, "egm")
}


