### Class Definitions ----

#' Electrogram and electrocardiogram data class definition
#'
#' @name egm
#' @export
egm <- function(header = list(),
								signal = multi_channel(),
								...) {

	# Header will be  a list
	# Signal data will be in multi-channel format for EPS data
	# Each column in the dataframe is an informative `eps` signal data

	new_egm(
		header = header,
		signal = signal
	)
}


#' @export
new_egm <- function(header = list(),
										signal = data.frame(),
										...) {

	# Signal will become a data frame (coerced into a data table)
	structure(
		signal,
		header = header,
		class = c("egm", "data.table", "data.frame")
	)


}

### {vctr} Definitions ----

#' @keywords internal
#' @noRd
methods::setOldClass(c("egm", "vctrs_list_of"))

#' @export
format.egm <- function(x, ...) {
	hea <- field(x, "header")
	chs <- field(x, "channels")

	cat("Signal Data\n")
	cat("-----------\n\n")
	cat("Recording Duration: ", hea$samples/hea$freq, "seconds\n")
	cat("Recording Frequency ", hea$freq, " Hz\n")
	cat("Number of channels: ", hea$number_of_channels, "\n")
	cat("Channel Names: ", paste(chs$label))

}

obj_print_data.egm <- function(x, ...) {
	if (length(x) != 0) {
		print(format(x), quote = FALSE)
	}
}


#' @export
#' @rdname egm
is_egm <- function(x) {
	inherits(x, "egm")
}

#' @export
vec_ptype_abbr.egm <- function(x, ...) "egm"

#' @export
vec_ptype_full.egm <- function(x, ...) "electrogram"

#' @export
vec_ptype2.egm.egm <- function(x, y, ...) new_egm()

#' @export
vec_cast.egm.egm <- function(x, to, ...) x

### External Helpers ----

#' @export
get_header <- function(x) {
	stopifnot("Requires `egm` class" = inherits(x, "egm"))
	attributes(x)$header
}

#' @export
get_signal <- function(x) {
	stopifnot("Requires `egm` class" = inherits(x, "egm"))
	vec_data(x)
}

### Internal Helpers ----

