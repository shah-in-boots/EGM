# nocov start
.onAttach <- function(libname, pkgname) {

	# Handle WFDB options
	op <- options()

	# Find WFDB
	# If it can detect the software, then set the path for the user
	# If not, the user will have to set the path themselves under
	# 	- options()$wfdb_path
	# 	- options()[["wfdb_path"]]
	if (grepl("wfdbdesc", Sys.which("wfdbdesc"))) {
		wp <-
			Sys.which("wfdbdesc") |>
			fs::path() |>
			fs::path_dir()

		# Send message confirming
		packageStartupMessage("WFDB software detected and set as `options(wfdb_path = '", wp, "')`")

		# Add path to options as default
		op.shiva <- list(
			wfdb_path = wp
		)

		toset <- !(names(op.shiva) %in% names(op))
		if (any(toset)) options(op.shiva[toset])

	}

}

# nocov end
