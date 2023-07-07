# Writing WFDB format data -----------------------------------------------------

#' Waveform Database (WFDB) Software Package
#'
#' @author
#' Original software: George Moody, Tom Pollard, Benjamin Moody \cr
#' R implementation: Anish S. Shah \cr
#' Last updated: 04/28/23 \cr
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

#' I/O of WFDB-compatible signal & header files from EP recording systems
#'
#' This function allows for WFDB files to be read from specific EP recording
#' systems, as indicated by the __type__ argument below.
#'
#' @param data A `data.frame` (or similar) that has a column that represents a
#'   time point or index, and columns that represent signal values.
#'
#' @param type Type of signal data, as specified by the recording system.
#'   Currently supports:
#'
#' * _lspro_ = Boston Scientific LabSystem Pro (Bard)
#'
#' * _muse_ = GE MUSE
#'
#' @param header A header file is a named list of parameters that will be used
#'   to organize and describe the signal input from the `data` argument. If the
#'   `type` is given, specific additional elements will be searched for, such as
#'   the low or high pass filters, colors, or other signal attributes. At
#'   minimum, the following elements are required (as cannot be calculated):
#'
#'   * frequency = sample frequency in Hertz <integer>
#'
#'   * label = vector of names for each channel <character>
#'
#'   * start_time = date/time object <date>
#'
#' @param record String that will be used to name the WFDB record. Cannot
#'   include extensions, and is not a filepath. alphanumeric characters are
#'   acceptable, as well as hyphens (-) and underscores (_)
#'
#' @param record_dir File path of directory that should be used read and write
#'   files. Defaults to current directory.
#'
#' @name wfdb_io
#' @export
write_wfdb <- function(data,
											 type,
											 header,
											 record,
											 record_dir = ".",
											 wfdb_path = getOption("wfdb_path"),
											 ...) {

	# TODO How do you write a R-object data file into a WFDB-format?
		# Options for `wrsamp`
		# 	-F <numeric>		sampling frequency, default is 250
		# 	-G <numeric>		gain in analog/digital units per milivolt, default is 200
		#		-i <string>			input file (default standard input)
		#		-o <string>			Write the signal file in current directory as 'record'
		#												record.dat
		#												record.hea
		#		-x <numeric>		Scaling factor if needed

	# Validation of paths
	wrsamp <- find_wfdb_command('wrsamp')
	if (fs::dir_exists(record_dir)) {
		wd <- fs::path(record_dir)
	} else {
		wd <- getwd()
	}

	# Validate arguments
	checkmate::assert_character(record)
	checkmate::assert_data_frame(data)

	# Based on type, can change how header information is stored
	# 	LSPRO (EPS)
	# 	MUSE (ECG)
	if (type == "lspro") {

		# If header is available, check to see if key variables are present
		if (!is.null(header)) {
			checkmate::assert_names(names(header),
															must.include = c("frequency", "adc_gain", "label", "start_time"))

			checkmate::assert_integerish(header$frequency)
			checkmate::assert_character(as.character(header$label), len = ncol(data))
			checkmate::assert_numeric(header$adc_gain, lower = 0, len = ncol(data))
		}

		# Write out temporary CSV file for WFDB to use
		tmpFile <- fs::file_temp("lspro", ext = "csv")
		withr::defer(fs::file_delete(tmpFile))
		data.table::fwrite(data, file = tmpFile, col.names = FALSE)

		# Options for `wrsamp`
		# 	-F <numeric>		sampling frequency, default is 250
		# 	-G <numeric>		gain in analog/digital units per milivolt, default is 200
		#		-i <string>			input file (default standard input)
		#		-o <string>			Write the signal file in current directory as 'record'
		#												record.dat
		#												record.hea
		#		-x <numeric>		Scaling factor if needed

		# Frequency
		hz <- paste("-F", header$frequency)

		# ADC = -G "adc adc adc adc" format
		adc <- paste('-G', paste0('"', paste(header$adc_gain, collapse = " "), '"'))

		# Input (full file path)
		ip <- paste("-i", tmpFile)

		# Output
		op <- paste("-o", record)

		# Write with `wrsamp`
		# 	Change into correct folder/directory (the writing directory)
		# 	Then reset to base directory
		# 	Cleanup and remove temporary CSV file immediately
		withr::local_dir(new = wd)
		system2(command = wrsamp,
						args = c(hz, adc, ip, op))

		# Modify header file with more data
			# Record line (first one) needs a date and time appended
			# Then handle the signal specification files
		headLine <-
			readLines(con = paste0(record, ".hea"), n = 1) |>
			paste(format(header$start_time, "%H:%M:%S %d/%m/%Y"))

		# 10 columns:
		# 	>= V9 and V10 are descriptive fields
		# 		Should be a tab-delim field
		#			Can contain spaces internal to it
		# 	V3 is ADC units
		#			Can be appended with baseline value "(0)"
		# 		Can be appended with "/mV" to specify units
		headerFile <-
			read.table(file = paste0(record, ".hea"),
								 skip = 1)
		headerFile[[3]] <- paste0(headerFile[[3]], "(0)", "/mV", sep = "")
		headerFile <- headerFile[1:9]
		headerFile[9] <- header$label

		# Write header back in place
		writeLines(text = headLine,
							 con = paste0(record, ".hea"))

		write.table(
			headerFile,
			file = paste0(record, ".hea"),
			sep = "\t",
			quote = FALSE,
			col.names = FALSE,
			row.names = FALSE,
			append = TRUE
		)

		# Add additional information at end of header
		info <- c(paste("# low_pass", paste(header$low_pass, collapse = " ")),
							paste("# high_pass", paste(header$high_pass, collapse = " ")),
							paste("# color", paste(header$color, collapse = " ")),
							paste("# source", paste(header$source, collapse = " ")))

		write(info, file = paste0(record, ".hea"), append = TRUE, sep = "\n")
	}

	# For MUSE ECGs
	if (type == "muse") {

		# Write out temporary CSV file for WFDB to use
		tmpFile <- fs::file_temp("muse", ext = "csv")
		withr::defer(fs::file_delete(tmpFile))
		data.table::fwrite(data, file = tmpFile, col.names = FALSE)

		# Options for `wrsamp`
		# 	-F <numeric>		sampling frequency, default is 250
		# 	-G <numeric>		gain in analog/digital units per milivolt, default is 200
		#		-i <string>			input file (default standard input)
		#		-o <string>			Write the signal file in current directory as 'record'
		#												record.dat
		#												record.hea
		#		-x <numeric>		Scaling factor if needed

		# Frequency
		hz <- paste("-F", header$frequency)

		# ADC = -G "adc adc adc adc" format
		# adc <- paste('-G', paste0('"', paste(header$adc_gain, collapse = " "), '"'))

		# Input (full file path)
		ip <- paste("-i", tmpFile)

		# Output
		op <- paste("-o", record)

		# Write with `wrsamp`
		# 	Change into correct folder/directory (the writing directory)
		# 	Then reset to base directory
		# 	Cleanup and remove temporary CSV file immediately
		withr::local_dir(new = wd)
		system2(command = wrsamp,
						args = c(hz, ip, op))

		# Modify header file with more data
			# Record line (first one) needs a date and time appended
			# Then handle the signal specification files
		headLine <-
			readLines(con = paste0(record, ".hea"), n = 1) |>
			paste(format(header$start_time, "%H:%M:%S %d/%m/%Y"))

		# 10 columns:
		# 	>= V9 and V10 are descriptive fields
		# 		Should be a tab-delim field
		#			Can contain spaces internal to it
		# 	V3 is ADC units
		#			Can be appended with baseline value "(0)"
		# 		Can be appended with "/mV" to specify units
		headerFile <-
			read.table(file = paste0(record, ".hea"),
								 skip = 1)
		headerFile[[3]] <- paste0(headerFile[[3]], "(0)", "/mV", sep = "")
		headerFile <- headerFile[1:9]
		headerFile[9] <- header$label

		# Write header back in place
		writeLines(text = headLine,
							 con = paste0(record, ".hea"))

		write.table(
			headerFile,
			file = paste0(record, ".hea"),
			sep = "\t",
			quote = FALSE,
			col.names = FALSE,
			row.names = FALSE,
			append = TRUE
		)

		# Add additional information at end of header for MUSE ecg data
		info <- c(paste("# mrn", paste(header$mrn, collapse = " ")),
							paste("# age", paste(header$age, collapse = " ")),
							paste("# sex", paste(header$sex, collapse = " ")),
							paste("# race", paste(header$race, collapse = " ")),
							paste("# diagnosis", paste(header$diagnosis, collapse = " ")))

		write(info, file = paste0(record, ".hea"), append = TRUE, sep = "\n")

	}

}

#' @rdname wfdb_io
#'
#' @param file Name of file. If it is a full path, then `read_location` is
#'   ignored
#'
#' @inheritParams wfdb_io
#'
#' @export
rewrite_wfdb <- function(file,
												 type,
												 record = file,
												 record_dir = ".",
												 wfdb_path = getOption("wfdb_path"),
												 ...) {

	# Validate WFDB software
	wrsamp <- find_wfdb_command('wrsamp')

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
		hz <- paste("-F", hea$frequency)

		# ADC
		adc <- paste("-G", paste0(
			'"',
			paste(hea$ADC_saturation / hea$gain, collapse = " "),
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
		system2(command = wrsamp,
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
		header <-
			data.table::fread(file = paste0(record, ".hea"),
												skip = 1)
		header[[3]] <- paste0(header[[3]], "(0)", "/mV", sep = "")
		header <- header[, 1:9]
		header[, 9] <- hea$channels$label

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

		# Add additional information at end of header specific to LSPro data
		info <- c(paste("# low_pass", paste(header$low_pass, collapse = " ")),
							paste("# high_pass", paste(header$high_pass, collapse = " ")),
							paste("# color", paste(header$color, collapse = " ")),
							paste("# source", paste(header$source, collapse = " ")))

		write(info, file = paste0(record, ".hea"), append = TRUE, sep = "\n")
	}

}

# Reading WFDB format data -----------------------------------------------------

#' @rdname wfdb_io
#' @inheritParams wfdb_io
#' @param begin,end,interval Timepoint in seconds, which is converted to an
#'   index position based on sampling frequency. The default is to start at the
#'   beginning of the record. If `end` or `interval` are given, the earlier of
#'   the two will be returned. The `end` argument gives a time index to read
#'   until. The `interval` argument is the length of time past the start point.
#'
#' @param units A string representing either `digital` (DEFAULT) or `physical`
#'   units that should be used, if available.
#'
#'   * digital = Index in sample number, signal in integers (A/D units)
#'
#'   * physical = Index in elapsed time, signal in decimal voltage (e.g. mV).
#'   This will __include 1 additional row over the header/column names__ that
#'   describes units
#'
#' @param channels Either the signal/channel in a vector as a name or number.
#'   Allows for duplication of signal or to re-order signal if needed.
#'
#' @export
read_wfdb <- function(record,
											record_dir = ".",
											wfdb_path = getOption("wfdb_path"),
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
	rdsamp <- find_wfdb_command("rdsamp")

	if (fs::dir_exists(record_dir)) {
		wd <- fs::path(record_dir)
	} else {
		wd <- getwd()
	}

	checkmate::assert_number(begin)
	checkmate::assert_number(end, na.ok = TRUE)
	checkmate::assert_number(interval, na.ok = TRUE)
	checkmate::assert_choice(units, choices = c("digital", "physical"))

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

	# Return data after cleaning names
	names(dat)[1] <- "sample"
	dat
}

#' @rdname wfdb_io
#'
#' @inheritParams wfdb_io
#'
#' @export
read_header <- function(record,
												record_dir = ".",
												wfdb_path = getOption("wfdb_path"),
												...) {


	# TODO read header files directly from text files *.hea
	# TODO include the 'getinfo' at bottom, encased in '# name x x x' format
}
