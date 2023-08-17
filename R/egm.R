### Class Definitions ----

#' Electrogram and electrocardiogram data class definition
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

	cat("<Electrical Signal>\n")
	cat("-------------------\n")
	cat( "Recording Duration: ", rec$samples / rec$frequency, "seconds\n" )
	cat("Recording frequency ", rec$frequency, " hz\n")
	cat("Number of channels: ", rec$number_of_channels, "\n")
	cat("Channel Names: ", paste(hea$label))
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


