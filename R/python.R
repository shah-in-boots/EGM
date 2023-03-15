# nocov start

#' Simple method to load `wfdb` and its dependencies
install_wfdb <- function(method = "auto", conda = "auto") {
	reticulate::py_install("wfdb", method = method, conda = conda)
}


.onLoad <- function(libname, package) {

	# Created from exported WFDB <- NULL function
	wfdb <<- reticulate::import("wfdb", delay_load = TRUE, convert = FALSE)

}

# nocov end
