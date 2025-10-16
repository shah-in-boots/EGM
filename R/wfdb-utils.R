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

#' Configure the preferred WFDB backend
#'
#' @description Allows advanced users to toggle between the native
#' {cpp11}-powered backend and the external WFDB system utilities.
#'
#' @param backend One of "native" or "system" specifying the desired backend.
#'
#' @returns Invisibly returns the selected backend after updating the session
#'   option.
#'
#' @export
set_wfdb_backend <- function(backend = c("native", "system")) {
  backend <- match.arg(backend)
  options(wfdb_backend = backend)
  invisible(backend)
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

#' Normalise a backend specification
#'
#' @keywords internal
#' @noRd
wfdb_match_backend <- function(backend) {
  choices <- c("native", "system")

  if (length(backend) == 0 || is.null(backend) || is.na(backend)) {
    backend <- "native"
  }

  backend <- tolower(backend[[1]])

  if (!backend %in% choices) {
    stop(
      "`backend` must be one of 'native' or 'system'",
      call. = FALSE
    )
  }

  backend
}

#' @keywords internal
#' @noRd
annotation_resolve_channel_indices <- function(channel_values, header_labels) {
  if (length(channel_values) == 0L) {
    return(list(values = integer(), changes = character()))
  }

  header_labels <- as.character(header_labels)
  message_pairs <- character()

  if (is.character(channel_values) || is.factor(channel_values)) {
    if (!length(header_labels)) {
      stop(
        "Channel names provided but header labels are unavailable for matching",
        call. = FALSE
      )
    }

    channel_chr <- as.character(channel_values)
    idx <- native_annotation_labels_to_indices(channel_chr, header_labels)
    unresolved <- is.na(idx) & !is.na(channel_chr) & nzchar(channel_chr)
    if (any(unresolved)) {
      stop(
        "Annotation channels could not be matched to header labels: ",
        paste(unique(channel_chr[unresolved]), collapse = ", "),
        call. = FALSE
      )
    }

    idx[is.na(idx)] <- 0L

    resolved_labels <- character(length(idx))
    resolved_labels[idx > 0] <- header_labels[idx[idx > 0]]

    original_norm <- native_normalise_channel_name(channel_chr)
    resolved_norm <- native_normalise_channel_name(resolved_labels)

    changed <- idx > 0 & !is.na(original_norm) & original_norm != resolved_norm
    if (any(changed)) {
      mapping <- unique(
        sprintf(
          "'%s' -> '%s'",
          channel_chr[changed],
          resolved_labels[changed]
        )
      )
      message_pairs <- mapping
    }
  } else {
    idx <- native_annotation_normalise_integer(channel_values)
    if (length(header_labels)) {
      too_high <- idx > length(header_labels)
      if (any(too_high, na.rm = TRUE)) {
        stop(
          "Annotation channels refer to indices beyond those defined in the header",
          call. = FALSE
        )
      }
    }
  }

  list(values = as.integer(idx), changes = message_pairs)
}

annotation_table_to_lines <- function(data) {
  # Formatting for annotation tables mirrors the fixed-width output of
  # `rdann -e`/`wrann`:
  #   * Each record occupies 42 characters before any auxiliary string.
  #   * The first six fields appear left padded to fixed widths, corresponding to
  #     time (12), sample (9), type (6), subtype (5), channel (5), and number (5).
  #   * Auxiliary text, when present, follows after a single separating space.

  columns <- c("time", "sample", "type", "subtype", "channel", "number")

  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(
      "Annotation data must include columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  pad <- function(x, width) {
    stringr::str_pad(x, width = width, side = "left")
  }

  format_integer <- function(x) {
    if (length(x) == 0L) {
      return(character())
    }
    values <- suppressWarnings(as.numeric(x))
    out <- rep("", length(values))
    keep <- !is.na(values)
    out[keep] <- sprintf("%d", as.integer(round(values[keep])))
    out
  }

  to_character <- function(x) {
    if (length(x) == 0L) {
      return(character())
    }
    result <- as.character(x)
    result[is.na(result)] <- ""
    result
  }

  time <- pad(to_character(data[["time"]]), width = 12)
  sample <- pad(format_integer(data[["sample"]]), width = 9)
  type <- pad(to_character(data[["type"]]), width = 6)
  subtype <- pad(format_integer(data[["subtype"]]), width = 5)
  channel <- pad(format_integer(data[["channel"]]), width = 5)
  number <- pad(format_integer(data[["number"]]), width = 5)

  lines <- sprintf("%s%s%s%s%s%s", time, sample, type, subtype, channel, number)

  aux <- NULL
  if ("aux" %in% names(data)) {
    aux <- to_character(data[["aux"]])
  }

  if (!is.null(aux) && length(aux) && any(nzchar(aux))) {
    lines <- paste0(lines, " ", aux)
  }

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
