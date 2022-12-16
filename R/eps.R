### Class Definitions ----

#' Electrophysiology eps class definition
#'
#' @name eps
#' @export
eps <- function(x = double(),
								frequency = 1000L,
								voltage = "mV",
								label = character(),
								color = "#FFFFFF",
								...) {

	# If empty
	if (vec_size(x) == 0) {
		return(new_eps())
	}


	# Cast appropriately (factors will not work as they are not base=numeric)
	x <- vec_cast(x, double())
	frequency <- vec_cast(frequency, integer())
	voltage <- vec_cast(voltage, character())
	color <- vec_cast(color, character())
	label <- vec_cast(label, .labels)

	# Obbtain label source
	source <- identify_channel_source(label)
	source <- vec_cast(source, .source)

	# Class initialization
	new_eps(
		x,
		frequency = frequency,
		voltage = voltage,
		source = source,
		label = label,
		color = color
	)
}


#' @export
new_eps <- function(x = double(),
										frequency = integer(),
										voltage = character(),
										source = factor(),
										label = factor(),
										color = character()) {

	# Validate
	vec_assert(x, double())
	vec_assert(frequency, integer())
	vec_assert(voltage, character())
	# vec_assert(source, .source)
	# vec_assert(label, .labels)
	vec_assert(color, character())

	# Vector with attributes included
	new_vctr(x,
					 frequency = frequency,
					 voltage = voltage,
					 source = source,
					 label = label,
					 color = color,
					 class = "eps")
}

#' @export
is_eps <- function(x) {
	inherits(x, "eps")
}

### {vctr} Definitions ----

#' @keywords internal
#' @noRd
methods::setOldClass(c("eps", "vctrs_vctr"))

#' @export
vec_ptype_abbr.eps <- function(x, ...) "eps"

#' @export
vec_ptype_full.eps <- function(x, ...) "electrical_signal"

#' @export
vec_ptype2.eps.eps <- function(x, y, ...) new_eps()

#' @export
vec_ptype2.eps.double <- function(x, y, ...) double()

#' @export
vec_ptype2.double.eps <- function(x, y, ...) double()

#' @export
vec_cast.eps.eps <- function(x, to, ...) x

#' @export
vec_cast.eps.double <- function(x, to, ...) eps(x, ...)

#' @export
vec_cast.double.eps <- function(x, to, ...) vec_data(x)


#' @export
format.eps <- function(x, ...) {
	if (vec_size(x) == 0) {
		new_eps()
	} else {
		formatC(vec_data(x), digits = 2, format = "f", flag = "0+- ")
	}
}

#' @export
obj_print_data.eps <- function(x, ...) {
	if (vec_size(x) == 0) {
		new_eps()
	} else if (vec_size(x) > 0) {
		print(format(x), quote = FALSE)
	}
}

### External Helpers ----

#' @export
identify_channel_source <- function(x) {

	# Intakes character vector and identifies the source
	stopifnot("Not a known/supported channel yet." = x %in% .labels)

	# Find source of lead bipole
	for (i in names(.leads)) {
		if (x  %in% .leads[[i]]) {
			y <- i
		}
	}

	# Return
	y
}

