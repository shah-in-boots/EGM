# Class ------------------------------------------------------------------------

#' Signal Table
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' `signal_table()` modifies the `data.table` class to work with electrical
#' signal data. The input should be a data set of equal number of rows.
#' @export
signal_table <- function(...) {

	# Invariant rules:
	# 	Can add and remove rows (each row is a time point)
	# 	Rows can NOT be re-ordered
	# 	Columns CAN be re-ordered
	# 	Signal columns must be numeric (integer or double)
	#
	# Invariant columns:
	# 	sample Position: represents a time point and order of data (<integer>)

	x <- df_list(..., .name_repair = 'minimal')

	if (length(x) == 0) {
		return(new_signal_table())
	}

	new_signal_table(x = x)
}

#' @export
new_signal_table <- function(x = list()) {
	checkmate::assert_list(x, types = 'numeric')
	new_data_frame(x, class = c('signal_table', 'data.table'))

}

#' @export
print.signal_table <- function(x, ...) {
	cat(sprintf('<%s: %s x %s>\n', class(x)[[1]], dim(x)[1], dim(x)[2]))
	if (length(x) > 0) {
		NextMethod()
	}
}

#' @export
vec_ptype_abbr.signal_table <- function(x, ...) "sig_tbl"

#' @export
vec_ptype_full.signal_table <- function(x, ...) "signal_table"

#' @export
#' @rdname signal_table
is_signal_table <- function(x) {
	inherits(x, "signal_table")
}

#' @importFrom vctrs vec_ptype2 vec_cast
NULL

# Constructors -----------------------------------------------------------------


# Casting and Coercion ---------------------------------------------------------


#' @export
signal_table_ptype2 <- function(x, y, ...) {
	as.data.table(df_ptype2(x, y, ...))
}

#' @export
signal_table_cast <- function(x, to, ...) {
	as.data.table(df_cast(x, to, ...))
}

## signal_table ----

#' @export
vec_ptype2.signal_table.signal_table <- function(x, y, ...) {
	new_signal_table()
}

#' @export
vec_cast.signal_table.signal_table <- function(x, to, ...) {
	x
}

## data.table ----

#' @export
vec_ptype2.signal_table.data.table <- function(x, y, ...) {
	signal_table_ptype2(x, y, ...)
}

#' @export
vec_cast.signal_table.data.table <- function(x, to, ...) {
	signal_table_cast(x, to, ...)
}

## data.frame ----

#' @export
vec_ptype2.signal_table.data.frame <- function(x, y, ...) {
	signal_table_ptype2(x, y, ...)
}

#' @export
vec_cast.signal_table.data.frame <- function(x, to, ...) {
	signal_table_cast(x, to, ...)
}
