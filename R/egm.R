#' Electrophysiology signal recording class definition
#'
#' @name egm
#' @export
egm <- function(header = data.table(), signal = data.table()) {

	new_egm(
		header = header,
		signal = signal
	)
}

#' @export
new_egm <- function(header = data.table(), signal = data.table()) {

	stopifnot(is.data.table(header))
	stopifnot(is.data.table(signal))

	x <- list(header = header, signal = signal)

	# List of header and signal information
	new_list_of(x, ptype = data.table(), class = "egm")

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("egm", "vctrs_list_of"))

#' @export
format.egm <- function(x, ...) {
	hea <- field(x, "header")
	sig <- field(x, "signal")

	print(head(hea))
	print(head(sig))
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
