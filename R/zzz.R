# nocov start
.onAttach <- function(libname, pkgname) {
	# Handle WFDB options
	op <- options()

	# Ensure a default backend is available
	backend_defaults <- list(
		wfdb_backend = "native"
	)

	toset <- !(names(backend_defaults) %in% names(op))
	if (any(toset)) {
		options(backend_defaults[toset])
	}

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
		packageStartupMessage(
			"WFDB software detected and set as `options(wfdb_path = '",
			wp,
			"')`, which will only be used if the `wfdb_backend` is changed to `system` from the default of `native`."
		)

		# Add path to options as default
		op.EGM <- list(
			wfdb_path = wp
		)

		toset <- !(names(op.EGM) %in% names(op))
		if (any(toset)) options(op.EGM[toset])
	}
}

# nocov end
