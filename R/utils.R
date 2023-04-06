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


#' @keywords internal
#' @noRd
annotation_table_to_lines <- function(data) {

	# Each annotation file has a string length of 42 characters
	# Each annotation `rdann -e` has 4 characters of spaces up front
	# When using hte `-e` option for rdann, gives an elapsed time
	# That assumption leads to spaces before the time starts

	# Columns are... n = 6
	#		Time
	#		Sample
	#		Annotation
	#		Type
	#		Subtype
	#		Channel
	#		Number
	#		Auxillary (7th, ignored)

	# The spacing is as such...
	# 	[TIME] = 12
	# 	[SAMPLE] = 9
	# 	[TYPE] = 6
	# 	[SUBTYPE] = 5
	# 	[CHANNEL] = 5
	# 	[NUMBER] = 5

	# Each column can get appropriately padded back into lines
	v1 <- stringr::str_pad(data[[1]], width = 12, side = "left")
	v2 <- stringr::str_pad(data[[2]], width = 9, side = "left")
	v3 <- stringr::str_pad(data[[3]], width = 6, side = "left")
	v4 <- stringr::str_pad(data[[4]], width = 5, side = "left")
	v5 <- stringr::str_pad(data[[5]], width = 5, side = "left")
	v6 <- stringr::str_pad(data[[6]], width = 5, side = "left")

	# Output will be put back into `wrann` compatible lines
	lines <- paste0(v1, v2, v3, v4, v5, v6)

	# Return
	lines

}
