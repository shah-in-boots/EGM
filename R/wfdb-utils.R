#' WFDB path utilities
#'
#' @description These functions are used to help find and locate commands from the
#' installation of WFDB. They are helpful in setting and getting path options
#' and specific WFDB commands. They are primarily internal helper functions, but
#' are documented for troubleshooting purposes.
#'
#' @returns These functions are helper functions to work with the user-installed
#'   WFDB software. They do not always return an object, and are primarily used
#'   for their side effects. They are primarily developer functions, but are
#'   exposed to the user to help troubleshoot issues with their installation of
#'   WFDB.
#'
#' @param .app The name of WFDB software command or application as a `character`
#'
#' @param .path A `character` string that describes the path to the WFDB binary
#'   directory
#'
#' @name wfdb_paths
#' @export
find_wfdb_software <- function() {
  # Check to see if WFDB software path is already set
  op <- getOption("wfdb_path")

  # If NULL then needs to be set
  if (is.null(op)) {
    # Confirm operating system structure
    if (grepl("windows|Windows", utils::sessionInfo()$running)) {
      os <- "win"
      packageStartupMessage(
        "Operating system is Windows. Default installation location for WFDB will be on WSL or Cygwin. Before using any `wfdb`-based functions, please set the location of the binary directory using `set_wfdb_path()`, which modifies `options('wfdb_path')`."
      )
    } else if (grepl("mac", utils::sessionInfo()$running)) {
      os <- "mac"

      packageStartupMessage(
        "Operating system detected is Apple. Default installation location for WFDB will be on root. Before using any `wfdb`-based functions, please set the location of the binary directory using `set_wfdb_path()`, which modifies `options('wfdb_path')`."
      )
    } else if (grepl("n*x", utils::sessionInfo()$running)) {
      os <- "nix"
      packageStartupMessage(
        "Operating system detected is Unix-like. Default installation location for WFDB will be on root. Before using any `wfdb`-based functions, please set the location of the binary directory using `set_wfdb_path()`, which modifies `options('wfdb_path')`."
      )
    } else {
      os <- NA
      packageStartupMessage(
        "Operating system could not be determined. Before using any `wfdb`-based functions, please set the location of the binary directory using `set_wfdb_path()`, which modifies `options('wfdb_path')`."
      )
    }
  }

  # Return path if exists already
  invisible(op)
}

#' @rdname wfdb_paths
#' @export
set_wfdb_path <- function(.path) {
  options(wfdb_path = .path)
}

#' @rdname wfdb_paths
#' @export
find_wfdb_command <- function(.app, .path = getOption('wfdb_path')) {
  # Check for wfdb_path
  # Maybe NULL or NA
  if (is.null(.path) | is.na(.path)) {
    stop('No `wfdb_path` set. Please set using `set_wfdb_path()`')
  }

  cmd <- fs::path(.path, .app)
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
  stopifnot('Requires `x` to be a `character`' = is.character(x))

  # Time
  # 	Assumes HH:MM:SS.SSS
  tm <- stringr::str_extract(x, '\\d\\d:\\d\\d:\\d\\d')

  # Dates are more varied
  # 	DD/MM/YYYY
  dt <- stringr::str_extract(x, '\\d+/\\d+/\\d+')

  # Create date time
  as.POSIXct(strptime(paste(tm[1], dt[1]), "%H:%M:%OS %d/%m/%Y"))
}
