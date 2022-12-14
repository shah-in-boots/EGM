### Class Definitions ----

#' Electrophysiology signal class definition
#'
#' @name signal
#' @export
signal <- function(x = double(),
											 label = factor(),
											 source = factor(),
											 color = character(),
											 frequency = integer(),
											 voltage = character()) {

	# Cast appropriately (factors will not work as they are not base=numeric)
	x <- vec_cast(x, double())
	color <- vec_cast(color, character())
	frequency <- vec_cast(frequency, integer())
	voltage <- vec_cast(voltage, character())

	# Class initialization
	new_signal(
		x,
		label = label,
		source = source,
		color = color,
		frequency = frequency,
		voltage = voltage
	)
}


#' @export
new_signal <- function(x = double(),
											 label = factor(),
											 source = factor(),
											 color = character(),
											 frequency = integer(),
											 voltage = character()) {

	# Validate
	vec_assert(x, double())
	#vec_assert(label, factor())
	#vec_assert(source, factor())
	vec_assert(color, character())
	vec_assert(frequency, integer())
	vec_assert(voltage, character())


	# Vector with attributes included
	new_vctr(x,
					 label = label,
					 source = source,
					 color = color,
					 frequency = frequency,
					 voltage = voltage,
					 class = "signal")
}

#' @export
is_signal <- function(x) {
	inherits(x, "signal")
}

### {vctr} Definitions ----

#' @keywords internal
#' @noRd
methods::setOldClass(c("signal", "vctrs_vctr"))

#' @export
vec_ptype_abbr.signal <- function(x, ...) "sig"

#' @export
vec_ptype_full.signal <- function(x, ...) "signal"

#' @export
vec_ptype2.signal.signal <- function(x, y, ...) new_signal()

#' @export
vec_ptype2.signal.double <- function(x, y, ...) double()

#' @export
vec_ptype2.double.signal <- function(x, y, ...) double()

#' @export
vec_cast.signal.signal <- function(x, to, ...) x

#' @export
vec_cast.signal.double <- function(x, to, ...) signal(x, ...)

#' @export
vec_cast.double.signal <- function(x, to, ...) vec_data(x)

### Output

#' @export
format.signal <- function(x, ...) {
	formatC(vec_data(x), digits = 2, format = "f", flag = "0+- ")
}

#' @export
obj_print_data.signal <- function(x, ...) {
	if (length(x) != 0) {
		print(format(x), quote = FALSE)
	}
}
