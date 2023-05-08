### Class Definitions ----

#' Electrogram and electrocardiogram data class definition
#'
#' @name egm
#' @export
egm <- function(signal,
								header,
								annotation = NULL,
								...) {

	# Signal data will be in multi-channel format for EPS data, e.g. data.table
	# Header will be  a list
	# Annotation is a simple data.table
	if (is.null(annotation)) {
		annotation <-
			data.table(
				time = numeric(),
				sample = integer(),
				type = character(),
				subtype = character(),
				channel = integer(),
				number = integer()
			)
	}

	new_egm(signal = signal,
					header = header,
					annotation = annotation)
}

#' @export
new_egm <- function(signal = data.table(),
										header = list(),
										annotation = data.frame(),
										...) {

	# Signal will become a data frame (coerced into a data table)
	structure(
		signal,
		header = header,
		annotation = annotation,
		class = c("egm", class(signal))
	)

}


#' @keywords internal
#' @noRd
methods::setOldClass(c("egm", "vctrs_list_of"))

#' @export
format.egm <- function(x, ...) {
	hea <- attr(x, "header")

	cat("Signal Data\n")
	cat("-----------\n")
	cat("Recording Duration: ", hea$samples/hea$frequency, "seconds\n")
	cat("Recording frequency ", hea$frequency, " Hz\n")
	cat("Number of channels: ", hea$number_of_channels, "\n")
	cat("Channel Names: ", paste(hea$label))
}

#' @export
print.egm <- function(x, ...) {
	format(x)
}

#' @export
vec_ptype_abbr.egm <- function(x, ...) "egm"

#' @export
vec_ptype_full.egm <- function(x, ...) "electrogram"


#' @export
#' @rdname egm
is_egm <- function(x) {
	inherits(x, "egm")
}

# Casting and coercion -------------------------------------------------------

## HELPERS

#' @export
egm_ptype2 <- function(x, y, ...) {
	as.data.table(df_ptype2(x, y, ...))
}

#' @export
egm_cast <- function(x, to, ...) {
	as.data.table(df_cast(x, to, ...))
}

## SELF

#' @export
vec_ptype2.egm.egm <- function(x, y, ...) new_egm()

#' @export
vec_cast.egm.egm <- function(x, to, ...) x

## DATA.TABLE

#' @export
vec_ptype2.egm.data.table <- function(x, y, ...) {
	egm_ptype2(x, y, ...)
}

#' @export
vec_cast.egm.data.table <- function(x, to, ...) {
	egm_cast(x, to, ...)
}
