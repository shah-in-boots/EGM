#' Electrophysiology signal recording class definition
#'
#' @name egm
#' @export
egm <- function(header = data.frame(), signal = data.frame()) {

	new_egm(
		header = header,
		signal = signal
	)
}

#' @export
new_egm <- function(header = data.frame(), signal = data.frame()) {

	stopifnot(is.data.frame(header))
	stopifnot(is.data.frame(signal))

	x <- list(header = header, signal = signal)

	# List of header and signal information
	new_list_of(x, ptype = data.frame(), class = "egm")
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("egm", "vctrs_rcrd"))

#' @export
format.egm <- function(x, ...) {
	hea <- field(x, "header")
	sig <- field(x, "signal")

	print(head(hea))
	print(head(sig))
}


#' @export
vec_ptype_abbr.egm <- function(x, ...) "egm"

#' @export
vec_ptype_full.egm <- function(x, ...) "electrogram"
