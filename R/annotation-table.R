# Class ------------------------------------------------------------------------

#' annotation Table
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' `annotation_table()` modifies the `data.table` class to work with annotation
#' data.
#'
#' @export
annotation_table <- function(time = numeric(),
														 sample = integer(),
														 type = character(),
														 subtype = character(),
														 channel = integer(),
														 number = integer()) {


	# Invariant rules:
	# 	Can add and remove rows (each row is an annotation)
	# 	Rows CAN be re-ordered
	# 	Columns CANNOT be re-ordered
	# 	Each column type is specific and invariant
	#
	# Invariant columns:
	#		time: <double>
	# 	sample: <integer>
	#		type: <character>
	# 	subtype: <character>
	#		channel: <integer>
	# 	number: <integer>

	x <- df_list(TIME = time,
							 SAMPLE = sample,
							 TYPE = type,
							 SUBTYPE = subtype,
							 CHANNEL = channel,
							 NUMBER = number)

	new_annotation_table(x = x)
}

#' @export
new_annotation_table <- function(x = list()) {

	checkmate::assert_list(
		x,
		types = c('numeric', 'integer', 'character')
	)

	checkmate::assert_names(
		names(x),
		identical.to = c('TIME', 'SAMPLE', 'TYPE', 'SUBTYPE', 'CHANNEL', 'NUMBER')
	)

	new_data_frame(x, class = c('annotation_table', 'data.table'))

}

#' @export
print.annotation_table <- function(x, ...) {
	cat(sprintf('<%s: %s annotations>\n', class(x)[[1]], dim(x)[1]))
	if (lengths(x)[1] > 0) {
		NextMethod()
	}
}

#' @export
vec_ptype_abbr.annotation_table <- function(x, ...) "ann_tbl"

#' @export
vec_ptype_full.annotation_table <- function(x, ...) "annotation_table"

#' @export
#' @rdname annotation_table
is_annotation_table <- function(x) {
	inherits(x, "annotation_table")
}

#' @importFrom vctrs vec_ptype2 vec_cast
NULL

# Constructors -----------------------------------------------------------------


# Casting and Coercion ---------------------------------------------------------


#' @export
annotation_table_ptype2 <- function(x, y, ...) {
	as.data.table(df_ptype2(x, y, ...))
}

#' @export
annotation_table_cast <- function(x, to, ...) {
	as.data.table(df_cast(x, to, ...))
}

## annotation_table ----

#' @export
vec_ptype2.annotation_table.annotation_table <- function(x, y, ...) {
	new_annotation_table()
}

#' @export
vec_cast.annotation_table.annotation_table <- function(x, to, ...) {
	x
}

## data.table ----

#' @export
vec_ptype2.annotation_table.data.table <- function(x, y, ...) {
	annotation_table_ptype2(x, y, ...)
}

#' @export
vec_cast.annotation_table.data.table <- function(x, to, ...) {
	annotation_table_cast(x, to, ...)
}

## data.frame ----

#' @export
vec_ptype2.annotation_table.data.frame <- function(x, y, ...) {
	annotation_table_ptype2(x, y, ...)
}

#' @export
vec_cast.annotation_table.data.frame <- function(x, to, ...) {
	annotation_table_cast(x, to, ...)
}
