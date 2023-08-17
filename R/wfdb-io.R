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
#' 1. Header attributes: recording information such as sample number, gain,
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
#' @param type Type of signal data, as specified by the recording system.
#'   Currently supports:
#'
#' * _lspro_ = Boston Scientific LabSystem Pro (Bard)
#'
#' * _muse_ = GE MUSE
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
											 header = list(frequency = 250,
											 							gain = 200L,
											 							label = character()),
											 info_strings = list(),
											 record,
											 record_dir,
											 wfdb_path = getOption('wfdb_path'),
											 ...) {


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

	# Set up data, checking to see whether its using a `egm` set or not
	if (inherits(data, 'egm')) {

		signal <- data$signal
		header <- data$header

	} else if (inherits(data, 'data.frame')) {

		signal <- signal_table(data)
		if (length(header$label) == 0) {
			header$label <- colnames(data)
		}

		header <- header_table(
			record_name = record,
			number_of_channels = length(signal),
			frequency = header$frequency,
			ADC_gain = header$gain,
			label = header$label,
			info_strings = info_strings
		)
	}

	# Write out temporary CSV file for WFDB to use
	tmpFile <- fs::file_temp("wfdb_", ext = "csv")
	withr::defer(fs::file_delete(tmpFile))
	data.table::fwrite(signal, file = tmpFile, col.names = FALSE)

	# Options for `wrsamp`
	# 	-F <numeric>		sampling frequency, default is 250
	# 	-G <numeric>		gain in analog/digital units per milivolt, default is 200
	#		-i <string>			input file (default standard input)
	#		-o <string>			Write the signal file in current directory as 'record'
	#												record.dat
	#												record.hea
	#		-x <numeric>		Scaling factor if needed

	# Frequency
	hz <- paste("-F", attributes(header)$record_line$frequency)

	# ADC = -G "adc adc adc adc" format
	adc <- paste('-G', paste0('"', paste(header$ADC_gain, collapse = " "), '"'))

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
		paste(format(attributes(header)$record_line$start_time, "%H:%M:%S %d/%m/%Y"))

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

	# Info strings are additional elements that may be available
	# Are placed after a `#` at end of header file
	# If there are additional lines in the header, can be placed in info section
	# 	e.g. color, bandpass
	# 	Otherwise uses named specific parameters like MRN or AGE

	additional_info <-
		header[, (ncol(header) - 5):ncol(header)] |>
		{
			\(.x) Filter(f = function(.y) !all(is.na(.y)), x = .x)
		}() |>
		as.list()

	info <- append(info_strings, additional_info)
	text <- lapply(info, function(.x) paste(.x, collapse = ' '))
	lines <- paste('#', names(info), text)
	write(lines, file = paste0(record, ".hea"), append = TRUE, sep = "\n")

}

#' @export
write_wfdb_old <- function(data,
											 header,
											 type,
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
		hz <- paste("-F", attributes(header)$record_line$frequency)

		# ADC = -G "adc adc adc adc" format
		adc <- paste('-G', paste0('"', paste(header$ADC_gain, collapse = " "), '"'))

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
			paste(format(attributes(header)$record_line$start_time, "%H:%M:%S %d/%m/%Y"))

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
		hz <- paste("-F", attributes(header)$record_line$frequency)

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
			paste(format(attributes(header)$record_line$start_time, "%H:%M:%S %d/%m/%Y"))

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
		headerFile[9] <- header$LABEL

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

		# add additional information at end of header for muse ecg data
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
		hz <- paste("-F", hea$FREQUENCY)

		# ADC
		adc <- paste("-G", paste0(
			'"',
			paste(hea$ADC_SATURATION / hea$GAIN, collapse = " "),
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
			paste(format(hea$START_TIME, "%H:%M:%S %d/%m/%Y"))

		# 10 columns:
		# 	>= V9 and V10 are descriptive fields
		# 		Should be a tab-delim field
		#			Can contain spaces internal to it
		# 	V3 is ADC units
		#			Can be appended with baseline value "(0)"
		# 		Can be appended with "/mV" to specify units
		header <-
			data.table::fread(file = paste0(record, ".hea"),
												skip = 1)
		header[[3]] <- paste0(header[[3]], "(0)", "/mV", sep = "")
		header <- header[, 1:9]
		header[, 9] <- hea$LABEL

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
		info <- c(paste("# low_pass", paste(hea$low_pass, collapse = " ")),
							paste("# high_pass", paste(hea$high_pass, collapse = " ")),
							paste("# color", paste(hea$color, collapse = " ")),
							paste("# source", paste(hea$source, collapse = " ")))

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

	# Generate header file path
	fp <- fs::path(record_dir, record, ext = 'hea')
	if (!fs::file_exists(fp)) {
		stop(record, " not found in ", record_dir)
	}

	# Record line (first one)
	# 10 columns:
	# 	>= V9 and V10 are descriptive fields
	# 		Should be a tab-delim field
	#			Can contain spaces internal to it
	#		V1 = File Name (*.dat)
	# 	V2 = 8-bit or 16-bit
	# 	V3 is ADC gain
	#			Can be appended with baseline value "(0)"
	# 		Can be appended with "/mV" to specify units
	# 	V4 = ADC resolution in bits (8-bits, 16-bits, etc)
	# 	V5 = ADC zero, is assumed to be zero if missing
	# 	V6 = Initial value of sample[0], present only if ADC zero is present
	# 	V7 = Checksum value (16-bit checksum of all samples)
	# 	V8 = Block size, usually 0
	# 	V9 = Description, usually ECG lead or EGM label
	record_line <- readLines(con = fp, n = 1)
	record_items <-
		record_line |>
		strsplit('\ ') |>
		unlist()

	record_name <- as.character(record_items[1])
	number_of_channels <- as.integer(record_items[2])
	frequency <- as.integer(record_items[3])
	samples <- as.integer(record_items[4])
	start_time <- parse_date_and_time(record_line)

	# Number of columns is important here
	sig_data <-
		data.table::fread(file = fp,
											skip = 1, # Skip head line
											nrows = number_of_channels) # Read in channel data
	# Number of columns is important here
	sig_data <-
		data.table::fread(file = fp,
											skip = 1, # Skip head line
											nrows = number_of_channels) # Read in channel data

	# ADC gain is in multiple parts that need to be split
	# Units will come after a forward slash `/`
	# Baseline value will be within parenthesis
	adc <- sig_data[[3]]
	ADC_gain <- stringr::str_extract(adc, '\\d+([.]\\d+)?')
	ADC_baseline <- stringr::str_extract(adc, "\\((\\d+)\\)", group = 1)
	ADC_baseline <-
		ifelse(is.na(ADC_baseline),
					 formals(header_table)$ADC_zero,
					 ADC_baseline)
	ADC_units <- stringr::str_extract(adc, "/([:alpha:]+)", group = 1)
	ADC_units <-
		ifelse(is.na(ADC_units),
					 formals(header_table)$ADC_units,
					 ADC_units)

	header_table(
		record_name = record_name,
		number_of_channels = number_of_channels,
		frequency = frequency,
		samples = samples,
		start_time = start_time,
		file_name = sig_data[[1]],
		storage_format = sig_data[[2]],
		ADC_gain = ADC_gain,
		ADC_baseline = ADC_baseline,
		ADC_units = ADC_units,
		ADC_resolution = sig_data[[4]],
		ADC_zero = sig_data[[5]],
		initial_value = sig_data[[6]],
		checksum = sig_data[[7]],
		blocksize = sig_data[[8]],
		label = sig_data[[9]]
	)
}
