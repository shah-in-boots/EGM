#' Circular Regression
#'
#' To be used similar to the `stats::lm()` function. This function assumes that
#' the dependent variable has a circular pattern (e.g. time, angles, directions).
#' The independent variable may either all be circular, or they may all be linear
#' (but not both).
#'
#' Other standards for these types of models have been preset to simplify
#' modeling. This function requires installation of the `{circular}` package for
#' the back-end of this function.
#'
#' @param formula Standard formula with single dependent variable and multiple
#' independent variables
#'
#' @param type  describes the right-hand side of the formula as being either "linear"
#' or "circular" distributions
#'
#' @param data Data set that contains the named terms from the formula
#'
#' @param units The type of units that the circular data should be calculated in
#' (e.g. radians, degrees)
#'
#' @param tidy If the results should be in a text block or should be in a tidy
#' table. If FALSE, then the confidence arguments are not needed/used.
#'
#' @inheritParams broom::tidy
#'
#' @examples
#' formula = el_svg ~ lab_hba1c + bmi
#' type = "linear"
#' data = card::geh
#' units = "degrees"
#' raw <- cm(formula = formula, type = type, data = data, units = units)
#' tidy <- cm(formula = formula, type = type, data = data, units = units, tidy = TRUE)
#'
#' @export
cm <- function(formula,
							 type,
							 data,
							 units,
							 tidy = FALSE,
							 conf.int = TRUE,
							 conf.level = 0.95,
							 ...) {

	# Formula terms
	lhs <- all.vars(formula)[1]
	rhs <- all.vars(formula)[-1]

	# Ensure proper circular inputs
	y <- circular::circular(data[[lhs]], units = units)

	if (type == "circular") {
		x <-
			lapply(data[rhs], function(.x) {
				circular::circular(.x, units = units)
			}) |>
			as.matrix()
	} else {
		x <-
			data[rhs] |>
			as.matrix()
	}

	# Model fitting
	if (type == "linear") {
		m <-
			circular:::lm.circular.cl(
				y = y,
				x = x,
				init = rep(0, ncol(x)),
				tol = 1e-3,
				verbose = FALSE
			)
	} else if (type == "circular") {

		lvl <- 1 - conf.level
		m <-
			circular:::lm.circular.cc(
				y = y,
				x = x,
				order = 1,
				level = lvl
			)
	}

  # Tidy if needed
	if (tidy) {

	  names(m$coefficients) <- colnames(m$x)
		coefs <- m$coefficients
		se <- m$se.coef
		l <- list(coefs = coefs, se = se)
		mat <- do.call(cbind, lapply(l, function(m) {m[match(names(l[[1]]), names(m))]}))

		# Tibble it
		result <-
		  mat |>
		  dplyr::as_tibble(rownames = "term") |>
		  dplyr::rename(estimate = coefs, std.error = se) |>
			tibble::add_column(
				statistic = m$t.values,
				p.value = m$p.values
			)

		# Add confidence intervals if needed
		if (conf.int) {
		  tdist <- stats::qt(1 - (1 - conf.level)/2, df = nrow(m$x) - 2)
		  result <-
		  	result |>
		  	tibble::add_column(
		  		conf.low = coefs - tdist * se,
		  		conf.high = coefs + tdist * se
		  	)
		}

		# Return tidy table
		return(result)

	} else {

		# Return model
		return(m)

	}

}
