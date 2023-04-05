# Signal -----------------------------------------------------------------------

#' Waveform Database (WFDB) Software Package
#'
#' @author
#' Original software: George Moody, Tom Pollard, Benjamin Moody.
#' R implementation: Anish S. Shah
#' Last updated: 01/22/23
#'
#' @description
#'
#' This implementation of WFDB is a back-end for the WFDB using a combination of
#' _python_, _C++_, and _C_ language. The related functions are documented
#' separately.
#'
#' @details
#'
#' # WFDB
#'
#' The WFDB (Waveform Database) Software Package has been developed over the
#' past thirty years, providing a large collection of software for processing
#' and analyzing physiological waveforms. The package is written in highly
#' portable C and can be used on all popular platforms, including GNU/Linux,
#' MacOS X, MS-Windows, and all versions of Unix.
#'
#' The foundation of the WFDB Software Package is the WFDB library,
#' consisting of a set of functions for reading and writing digitized signals
#' and annotations. These functions can be used by programs written in C, C++,
#' or Fortran, running under any operating system for which an ANSI/ISO C
#' compiler is available, including all versions of Unix, MS-DOS, MS-Windows,
#' the Macintosh OS, and VMS.
#'
#' # Data format
#'
#' The records that the WFDB uses have three components...
#'
#' 1. Signals: integer values that are at equal intervals at a certain sampling
#' frequency
#'
#' 1. Attributes: recording information such as sample number, gain,
#' sampling frequency
#'
#' 1. Annotations: information about the record such as abeat
#' labels or alarm triggers
#' @name wfdb
NULL

#' Create WFDB-compatible signal and header files from EP recording systems
#'
#' This function allows for WFDB files to be read from specific EP recording
#' systems, as indicated by the __type__ argument below.
#'
#' @param file Name of file. If it is a full path, then `read_location` is
#'   ignored
#'
#' @param type Type of signal data, as specified by the recording system.
#'   Currently supports:
#'
#' * _lspro_ = Boston Scientific LabSystem Pro (Bard)
#'
#' @param record String that will be used to name the WFDB record. Cannot
#'   include extensions, and is not a filepath. alphanumeric characters are
#'   acceptable, as well as hyphens (-) and underscores (_)
#'
#' @param record_dir File path of directory that should be used read and write
#'   files. Defaults to current directory.
#'
#' @export
write_wfdb <- function(file,
											 type,
											 record = file,
											 record_dir = ".",
											 wfdb_path,
											 ...) {

	# Validate WFDB software
	cmd <- find_wfdb_software(wfdb_path, "wrsamp")

	# Validate working directories and paths
	# 	Current or parent working directory
	# 	Reading directory and relevant file
	# 	Writing directory
	if (fs::file_exists(file)) {
		fp <- fs::path(file)
	} else if (fs::file_exists(fs::path(record_dir, file))) {
		fp <- fs::path(record_dir, file)
	} else {
		stop("Cannot find the file in the the reading directory")
	}

	if (fs::dir_exists(record_dir)) {
		wd <- fs::path(record_dir)
	} else {
		wd <- getwd()
	}

	# Validate record name
	record <-
		record |>
		fs::path_sanitize() |>
		fs::path_ext_remove()

	if (type == "lspro") {

		# Get LSPro data
		hea <- read_lspro_header(fp)
		sig <- read_lspro_signal(fp, n = Inf)

		# Write out temporary CSV file for WFDB to use
		# Add a timestamp for wfdb::wrsamp() to pull this in
		tmpFile <- fs::file_temp("lspro", ext = "csv")
		withr::defer(fs::file_delete(tmpFile))
		data.table::fwrite(sig, file = tmpFile, col.names = FALSE)

		# Options for `wrsamp`
		# 	-F <numeric>		sampling frequency, default is 250
		# 	-G <numeric>		gain in analog/digital units per milivolt, default is 200
		#		-i <string>			input file (default standard input)
		#		-o <string>			Write the signal file in current directory as 'record'
		#												record.dat
		#												record.hea
		#		-x <numeric>		Scaling factor if needed

		# Frequency
		hz <- paste("-F", hea$freq)

		# ADC
		adc <- paste("-G", paste0(
			'"',
			paste(hea$ADC_saturation / hea$channels$gain, collapse = " "),
			'"'
		))

		# Input (full file path)
		ip <- paste("-i", tmpFile)

		# Output
		op <- paste("-o", record)

		# Write with `wrsamp`
		# 	Change into correct folder/directory (the writing directory)
		# 	Then reset to base directory
		# 	Cleanup and remove temporary CSV file immediately
		withr::local_dir(new = wd)
		system2(command = cmd,
						args = c(hz, adc, ip, op))

		# Modify header file with more data
			# Record line (first one) needs a date and time appended
			# Then handle the signal specification files
		headLine <-
			readLines(con = paste0(record, ".hea"), n = 1) |>
			paste(format(hea$start_time, "%H:%M:%S %d/%m/%Y"))

		# 10 columns:
		# 	>= V9 and V10 are descriptive fields
		# 		Should be a tab-delim field
		#			Can contain spaces internal to it
		# 	V3 is ADC units
		#			Can be appended with baseline value "(0)"
		# 		Can be appended with "/mV" to specify units
		header <-
			read.table(file = paste0(record, ".hea"),
								 skip = 1)
		header[[3]] <- paste0(header[[3]], "(0)", "/mV", sep = "")
		header <- header[1:9]
		header[9] <- hea$channels$label

		# Write header back in place
		writeLines(text = headLine,
							 con = paste0(record, ".hea"))
		write.table(
			header,
			file = paste0(record, ".hea"),
			sep = "\t",
			quote = FALSE,
			col.names = FALSE,
			row.names = FALSE,
			append = TRUE
		)
	}

}

#' Reading in WFDB signal
#'
#' @param begin,end,interval Timepoint in seconds, which is converted to an
#'   index position based on sampling frequency. The default is to start at the
#'   beginning of the record. If `end` or `interval` are given, the earlier of
#'   the two will be returned. The `end` argument gives a time index to read
#'   until. The `interval` argument is the length of time past the start point.
#'
#' @param units A string representing either `digital` (DEFAULT) or
#'   `physical` units that should be returned, if available.
#'
#'		- digital = Index in sample number, signal in integers (A/D units)
#'
#'		- physical = Index in elapsed time, signal in decimal voltage (e.g. mV).
#'		This will include 1 additional row over the header/column names that
#'		describes units
#'
#' @param channels Either the signal/channel in a vector as a name or number.
#'   Allows for duplication of signal or to re-order signal if needed.
#'
#' @return Data table
#' @export
read_wfdb <- function(record,
											record_dir = ".",
											wfdb_path,
											begin = 0,
											end = NA_integer_,
											interval = NA_integer_,
											units = "digital",
											channels = character(),
											...) {

	# Validate:
	#		WFDB software command
	# 	Current or parent working directory
	# 	Directory of the record/WFDB files
	# 	Variable definitions
	rdsamp <- find_wfdb_software(wfdb_path, "rdsamp")

	if (fs::dir_exists(record_dir)) {
		wd <- fs::path(record_dir)
	} else {
		wd <- getwd()
	}

	checkmate::assert_number(begin)
	checkmate::assert_number(end, na.ok = TRUE)
	checkmate::assert_number(interval, na.ok = TRUE)

	# Create all the necessary parameters for rdsamp
	#		-f			Start time
	#		-l, -t	Interval length OR end time ... one or other, not both
	#		-H			High resolution mode for high-sampling frequencies
	#		-p			Uses physical units instead of digital
	#							Miliseconds/physical (mV) units
	#							default is sample interval (index) and A/D units
	# 	-P			Capital P gives back higher number of decimal places
	#		-s			Select which signals to print out (can duplicate + re-order)
	#							Name or Number, separated by spaces
	#							Default prints all signals
	#		-v			Column headings
	#		-X, -c	Output format: either XML or CSV, default = tab (not needed)

	cmd <-
		paste(rdsamp, '-r', record) |>
		{
			\(.) {
				if (begin != 0) {
					paste(., "-f", begin)
				} else {
					.
				}
			}
		}() |>
		{
			\(.) {
				if (!is.na(interval)) {
					paste(., "-l", interval)
				} else {
					.
				}
			}
		}() |>
		{
			\(.) {
				if (!is.na(end)) {
					paste(., "-t", end)
				} else {
					.
				}
			}
		}() |>
		{
			\(.) {
				if (units == "physical") {
					paste(., "-p")
				} else {
					.
				}
			}
		}() |>
		{
			\(.) {
				if (length(channels) > 0) {
					paste(., "-s", paste(channels, collapse = " "))
				} else {
					.
				}
			}
		}() |>
		paste("-v")

	# Temporary local/working directory, to reset at end of function
	withr::with_dir(new = wd, code = {
		dat <- data.table::fread(cmd = cmd)
	})

	# Return data
	dat
}

# Annotation -------------------------------------------------------------------

#' ECG waveform detection
#'
#' @return Creates a WFDB-compatible annotation file
#'
#' @inheritParams write_wfdb
#'
#' @param detector Signal detector that can create WFDB-compatible annotation
#'   files
#'
#' @examples
#' record <- "sample"
#' detector <- "ecgpuwave"
#' @export
detect_beats <- function(record,
												 record_dir = ".",
												 detector,
												 wfdb_path,
												 ...) {

	# Validate
	# 	WFDB software - must be an ECG detector software
	#		WFDB must be on path
	# 	Reading/writing directory must be on path
	if (checkmate::check_subset(detector, c("gqrs",
																					"sqrs",
																					"sqrs125",
																					"wqrs",
																					"ecgpuwave"))) {
		ext <- "qrs"
	}

	cmd <- find_wfdb_software(wfdb_path, detector)

	if (fs::dir_exists(record_dir)) {
		wd <- fs::path(record_dir)
	} else {
		wd <- getwd()
	}

	rec <- paste("-r", record)
	ann <- paste("-a", ext)

	# Change working directory for writing purposes
	# 	This should change back at end of writing process
	withr::with_dir(new = wd,
									code = {
										# System call to beat detector/annotator
										system2(command = cmd,
														args = c(rec, ann),
														stdout = FALSE)

										# Clean up extraneous files that were created
										# 	`ecgpuwave` -> fortran files
										fs::file_delete(c("fort.21", "fort.20"))
									})


}

#' Read WFDB-compatible annotation file
#'
#' @inheritParams write_wfdb
#' @inheritParams read_wfdb
#'
#' @export
read_annotation <- function(record,
														annotator,
														record_dir = ".",
														begin = 0,
														end = NA_integer_,
														...) {

	# Validate:
	#		WFDB software command
	# 	Current or parent working directory
	# 	Directory of the record/WFDB files
	# 	Variable definitions
	rdann <- find_wfdb_software(wfdb_path, "rdann")

	if (fs::dir_exists(record_dir)) {
		wd <- fs::path(record_dir)
	} else {
		wd <- getwd()
	}

	checkmate::assert_number(begin)
	checkmate::assert_number(end, na.ok = TRUE)
	checkmate::assert_number(interval, na.ok = TRUE)

	# Create all the necessary parameters for rdann
	#		-f			Start time
	#		-t			End time
	#		-v			Column headings
	#		-e			Elapsed time as (versus absolute time)
	# TODO filtering flags not yet included
	cmd <-
		paste(rdann, '-r', record, '-a', annotator) |>
		{
			\(.) {
				if (begin != 0) {
					paste(., "-f", begin)
				} else {
					.
				}
			}
		}() |>
		{
			\(.) {
				if (!is.na(end)) {
					paste(., "-t", end)
				} else {
					.
				}
			}
		}() |>
		paste('-v')

	# Temporary local/working directory, to reset at end of function
	withr::with_dir(new = wd, code = {
		dat <-
			data.table::fread(
				cmd = cmd,
				header = TRUE,
				fill = TRUE,
				sep = "\t"
			)
	})

	# Return data
	dat
}
