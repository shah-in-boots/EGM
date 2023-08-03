#' ECG waveform detection
#'
#' @return Creates a WFDB-compatible annotation file
#'
#' @inheritParams rewrite_wfdb
#'
#' @param detector Signal detector that can create WFDB-compatible annotation
#'   files
#'
#' @examples
#' record <- "300"
#' detector <- "ecgpuwave"
#' @export
detect_surface_beats <- function(record,
																 record_dir = ".",
																 detector,
																 wfdb_path = getOption("wfdb_path"),
																 ...) {

	# Validate
	# 	WFDB software - must be an ECG detector software
	#		WFDB must be on path
	# 	Reading/writing directory must be on path
	if (detector %in% c("gqrs",
											"sqrs",
											"sqrs125",
											"wqrs")) {
		ext <- "qrs"
	} else if (detector %in% c("ecgpuwave")) {
		ext <- "ecgpuwave"
	}

	detector <- find_wfdb_command(detector)

	if (fs::dir_exists(record_dir)) {
		wd <- fs::path(record_dir)
	} else {
		wd <- getwd()
	}

	rec <- paste("-r", record)
	ann <- paste("-a", ext)

	# Change working directory for writing purposes
	# This should change back at end of writing process
	withr::with_dir(new = wd,
									code = {
										# System call to beat detector/annotator
										system2(command = detector,
														args = c(rec, ann),
														stdout = FALSE,
														stderr = FALSE)

										if (fs::file_exists('fort.20')) {
											fs::file_delete('fort.20')
										}
										if (fs::file_exists('fort.21')) {
											fs::file_delete('fort.21')
										}

									})

}
