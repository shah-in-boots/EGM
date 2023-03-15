#' Waveform Database (WFDB) Software Package
#'
#' @author
#' Original software: George Moody, Tom Pollard, Benjamin Moody
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
#'
#' @return On call, loads series of WFDB functions
#' @name wfdb
#' @export
wfdb <- NULL

#' Describes signals based on WFDB-formatted files
#' @param record Name of the record as a string, with no extensions
#' @param location The directory that the target record is located within. As
#'   this is related to the [PhysioNet](https://physionet.org), using the
#'   location name `mitdb` will access the online directory for the MIT
#'   Database.
#' @export
describe_wfdb <- function(record,
													location = ".") {


	if (location == "mitdb") {
		out <- reticulate::py_capture_output(wfdb$wfdbdesc(record, location))
	} else {
		fp <- file.path(location, record)
		ft <- paste0(fp, c(".ann", ".atr", ".dat", ".hea", ".sig"))
		stopifnot("The record name does not exist within the directory"
							= any(file.exists(ft)))
		out <- reticulate::py_capture_output(wfdb$wfdbdesc(r_to_py(fp)))
	}

}

#' Create WFDB-compatible signal and header files from EP recording systems
#'
#' This function allows for WFDB files to be read from specific EP recording
#' systems, as indicated by the __type__ argument below.
#'
#' @param file File path of signal data
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
#' @importFrom reticulate r_to_py
#' @export
write_wfdb <- function(file,
											 type,
											 record,
											 read_location = ".",
											 write_location = ".",
											 ...) {


	# Read in data appropriately
	switch(type,
				 lspro = {
				 	hea <- read_lspro_header(file)
				 	sig <- as.matrix(read_lspro_signal(file, n = Inf))

				 	dt <- reticulate::import("datetime", convert = FALSE)
				 	start_time <- dt$datetime(
				 		year(hea$start_time),
				 		month(hea$start_time),
				 		mday(hea$start_time),
				 		hour(hea$start_time),
				 		minute(hea$start_time),
				 		second(hea$start_time)
				 	)

				 	invisible(reticulate::py_capture_output(wfdb$wrsamp(
				 		record_name = r_to_py(record),
				 		fs = r_to_py(hea$freq),
				 		units = r_to_py(as.list(rep("mV", hea$number_of_channels))),
				 		sig_name = r_to_py(hea$channels$label),
				 		d_signal = r_to_py(sig),
				 		adc_gain = r_to_py(hea$channels$gain / hea$ADC_saturation),
				 		fmt = r_to_py(as.list(rep("16", hea$number_of_channels))),
				 		baseline = r_to_py(as.list(rep(0L, hea$number_of_channels))),
				 		base_datetime = start_time,
				 		write_dir = r_to_py(write_location)
				 	)))

				 	message("`write_wfdb` successfully wrote `",
				 					record,
				 					"` to `",
				 					write_location,
				 					"`")


				 },
				 message("`write_wfdb` not supported for the supplied `type`")
				 )

}
