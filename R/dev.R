# nocov start

#' Describes signals based on WFDB-formatted files
#'
#' @param record String that will be used to name the WFDB record. Cannot
#'   include extensions, and is not a filepath. alphanumeric characters are
#'   acceptable, as well as hyphens (-) and underscores (_)
#'
#' @param location The directory that the target record is located within. As
#'   this is related to the [PhysioNet](https://physionet.org), using the
#'   location name `mitdb` will access the online directory for the MIT
#'   Database.
#'
describe_wfdb2 <- function(record,
													 location = ".") {
	if (location == "mitdb") {
		# nocov start
		out <-
			reticulate::py_capture_output(wfdb$wfdbdesc(record, location))
		# nocov end
	} else {
		fp <- file.path(location, record)
		ft <- paste0(fp, c(".ann", ".atr", ".dat", ".hea", ".sig"))
		stopifnot("The record name does not exist within the directory"
							= any(file.exists(ft)))
		# nocov start
		out <-
			reticulate::py_capture_output(wfdb$wfdbdesc(reticulate::r_to_py(fp)))
		# nocov end
	}

}

# nocov end
