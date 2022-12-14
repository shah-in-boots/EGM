### Class Definitions ----

#' Electrogram and electrocardiogram data class definition
#'
#' @name egm
#' @export
egm <- function(header = data.table(),
								channels = data.table(),
								signal = data.table(),
								...) {

	new_egm(
		header = header,
		channels = channels,
		signal = signal
	)
}


#' @export
new_egm <- function(header = data.table(),
										channels = data.table(),
										signal = data.table(),
										...) {

	stopifnot(is.data.table(header))
	stopifnot(is.data.table(channels))
	stopifnot(is.data.table(signal))

	x <- list(header = header, channels = channels, signal = signal)

	# List of header and signal information
	new_list_of(x, ptype = data.table(), class = "egm")

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
number_of_channels <- function(x) {
	stopifnot("Requires `egm` class" = inherits(x, "egm"))
	hea <- .pull_header(x)
	hea$number_of_channels
}

#' @export
sampling_frequency <- function(x) {
	stopifnot("Requires `egm` class" = inherits(x, "egm"))
	hea <- .pull_header(x)
	hea$freq
}

#' @export
recording_length <- function(x) {
	stopifnot("Requires `egm` class" = inherits(x, "egm"))
	hea <- .pull_header(x)
	hea$start_time
}

### Internal Helpers ----

#' Method to pull header data that allows for flexibility in internal functions
#' @keywords internal
#' @noRd
.pull_header <- function(x) {
	stopifnot("Requires `egm` class" = inherits(x, "egm"))

	x$header

}

#' Method to pull channel data that allows for flexibility in internal functions
#' @keywords internal
#' @noRd
.pull_channels <- function(x) {
	stopifnot("Requires `egm` class" = inherits(x, "egm"))

	x$channels

}

#' Method to pull signal data that allows for flexibility in internal functions
#' @keywords internal
#' @noRd
.pull_signal <- function(x) {
	stopifnot("Requires `egm` class" = inherits(x, "egm"))

	x$signal

}
