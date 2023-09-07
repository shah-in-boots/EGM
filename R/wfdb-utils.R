#' @keywords internal
#' @noRd
find_wfdb_software <- function() {

	# Confirm operating system structure for pulling
	if (grepl("windows|Windows", sessionInfo()$running)) {
		os <- "win"
	} else if (grepl("mac", sessionInfo()$running)) {
		os <- "mac"
	} else if (grepl("n*x", sessionInfo()$running)) {
		os <- "nix"
	} else {
		os <- NA
		packageStartupMessage("Operating system could not be identified. The WFDB will not be attempted to be found, and the path will default to $HOME.")
		wfdbPath <- fs::path_home()
	}

	if (os %in% c("mac", "nix")) {
		# Find possible binary folders
		possibleBins <- c(
			fs::path("/", "usr", "local", "bin"),
			fs::path_expand(fs::path("~", "bin")),
			fs::path("/", "usr", "bin")
		)

		# Find where WFDB could potentially exist using 'wfdbdesc' as standard
		possibleWFDB <-
			possibleBins[fs::dir_exists(possibleBins)] |>
			fs::path("wfdbdesc")

		# Obtain WFDB path
		wfdbDesc <- possibleWFDB[min(which(fs::file_exists(possibleWFDB)))]
		wfdbPath <- fs::path_dir(wfdbDesc)
	}

	# If the system is a Windows, the path may be on WSL, can add that as default
	if (os == "win") {
		packageStartupMessage("Operating system is Windows. Default installation location for WFDB will be on WSL at '/usr/local/bin'. This can be modified by changing the WFDB path options.")
		wfdbPath <- paste("wsl", "/usr/local/bin")
	}

	# Return
	wfdbPath


}

#' @keywords internal
#' @noRd
find_wfdb_command <- function(.app,
															.path = getOption('wfdb_path')) {

	cmd <- fs::path(.path, .app)
	if (fs::file_exists(cmd)) {
		return(cmd)
	} else {
		warning("Cannot find '", .app, "' in '", .path, "'")
	}

}

#' @keywords internal
#' @noRd
annotation_table_to_lines <- function(data) {

	# Each annotation file has a string length of 42 characters
	# Each annotation `rdann -e` has 4 characters of spaces up front
	# When using the `-e` option for rdann, gives an elapsed time
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
	# 	base::sprintf() is 2-3 faster than paste
	# 	lines <- paste0(v1, v2, v3, v4, v5, v6)
	lines <- sprintf(paste0(rep("%s", 6), collapse = ""), v1, v2, v3, v4, v5, v6)

	# Return
	lines

}

#' Evaluates a character string and extracts first date and time objects
#' Internally contains different matches for different WFDB formats
#' Requires that string can be broken into components via a space
#' @keywords internal
#' @noRd
parse_date_and_time <- function(x) {

	stopifnot('Requires `x` to be a character string' = is.character(x))

	# Time
	# 	Assumes HH:MM:SS
	tm <- stringr::str_extract(x, '\\d\\d:\\d\\d:\\d\\d')

	# Dates are more varied
	# 	DD/MM/YYYY
	dt <- stringr::str_extract(x, '\\d+/\\d+/\\d+')

	# Create date time
	as.POSIXct(strptime(paste(tm[1], dt[1]), "%H:%M:%S %d/%m/%Y"))

}
