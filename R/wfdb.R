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
#' 1. Signals: integer value that are at equal intervals at a certain sampling
#' frequency
#'
#' 1. Attributes: recording information such as sample number, gain,
#' sampling frequency
#'
#' 1. Annotations: information about the record such as abeat
#' labels or alarm triggers
#' @name wfdb
#' @export
wfdb <- NULL

#' Describes signals based on header file contents {wfdb}
#' @param record Name of the record as a string, with no extensions
#' @param location The directory that the target record is located within. As
#'   this is related to the [PhysioNet](https://physionet.org), using the
#'   location name `mitdb` will access the online directory for the MIT
#'   Database.
#' @export
wfdbdesc <- function(record,
										 location = ".") {

	if (location == "mitdb") {
		out <- reticulate::py_capture_output(wfdb$wfdbdesc(record, location))
	} else {
		fp <- file.path(location, record)
		ft <- paste0(fp, c(".ann", ".atr", ".dat", ".hea", ".sig"))
		stopifnot("The record name does not exist within the directory"
							= any(file.exists(ft)))
		out <- reticulate::py_capture_output(wfdb$wfdbdesc(fp))
	}

}
