#' @keywords internal
#' @noRd
find_wfdb_software <- function(.path, .app) {

	# Systematic checks to make sure the paths/files are correct and available
	#		Check that the directory path is a absolute path
	#		Check if directory exists
	#		Check if the application file exists
	if (fs::is_absolute_path(.path) &
			fs::dir_exists(.path) &
			fs::file_exists(fs::path(.path, .app))) {

		wfdbPath <- fs::path(.path, .app)
	} else {
		stop(paste("Incorrect path or `", .app, "` could not be found"))
	}

	wfdbPath

}
