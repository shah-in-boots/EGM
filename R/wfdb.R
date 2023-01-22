#' Waveform Database (WFDB) Software Package
#'
#' @author
#' Original software: George Moody, Tom Pollard, Benjamin Moody
#' R implementation: Anish S. Shah
#' Last updated: 01/22/23
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

