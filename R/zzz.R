.onLoad <- function(libname, pkgname) {

	# Handle WFDB options
	op <- options()

	# Find WFDB
	wp <- find_wfdb_software()

	# Add path to options as default
	op.shiva <- list(
		wfdb_path = wp
	)

	toset <- !(names(op.shiva) %in% names(op))
	if (any(toset)) options(op.shiva[toset])

}
