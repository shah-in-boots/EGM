### Class Definitions ----

#' Electrogram and electrocardiogram data class definition
#'
#' @name egm
#' @export
egm <- function(signal = signal_table(),
								header = list(),
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
										header = list(),
										annotation = annotation_table(),
										...) {

	# Signal will become a data frame (coerced into a data table)
	structure(
		signal,
		header = header,
		annotation = annotation,
		class = c("egm", class(signal))
	)

}

#' @export
format.egm <- function(x, ...) {
	hea <- attr(x, "header")

	cat("<Electrical Signal>\n")
	cat("-------------------\n")
	cat("Recording Duration: ", hea$SAMPLES/hea$FREQUENCY, "seconds\n")
	cat("Recording frequency ", hea$FREQUENCY, " Hz\n")
	cat("Number of channels: ", hea$NUMBER_OF_CHANNELS, "\n")
	cat("Channel Names: ", paste(hea$LABEL))
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


