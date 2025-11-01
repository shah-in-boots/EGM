# Annotation -------------------------------------------------------------------

#' Read WFDB-compatible annotation file
#'
#' @description Individual annotation types are described as both a command-line
#' tool for annotating WFDB-files, as well as the extension that is appended to
#' the `record` name to notate the type. Generally, the types of annotations
#' that are supported are described below:
#'
#'   * atr = manually reviewed and corrected reference annotation files
#'
#'   * ann = general annotator file
#'
#'   * ecgpuwave = files contain surface ECG demarcation (P, QRS, and T waves)
#'
#'   * sqrs/wqrs/gqrs = standard WFDB peak detection for R waves
#'
#' A more thorough explanation is given in the details. Additionally, files when
#' being read in are converted from a binary format to a textual format. The raw
#' data however may be inadequate, as the original annotation may be erroneous.
#' In these cases, an empty `annotation_table` object will be returned.
#'
#' @details
#' # Annotation files
#'
#' The following annotation file types are described below.
#'
#' ## `ecgpuwave`
#'
#' `ecgpuwave` analyzes an ECG signal from the specified record, detecting the
#' QRS complexes and locating the beginning, peak, and end of the P, QRS, and
#' ST-T waveforms. The output of ecgpuwave is written as a standard WFDB-format
#' annotation file (the extension is "*.ecgpuwave", as would be expected). This
#' file can be converted into text format using `rdann`. Further details are
#' given at the [ECGPUWAVE](https://physionet.org/content/ecgpuwave/1.3.4/)
#' page.
#'
#' The __type__ column can be _p_, _t_, or _N_ for the peak of the P wave, T
#' wave, and QRS (R peak) directly. The output notation also includes waveform
#' onset XXX and waveform offset XXX. The __number__ column gives further
#' information about each of these __type__ labels.
#'
#' The __number__ column gives modifier information. If the __type__ classifier
#' is a T wave annotation, the __number__ column can be 0 (normal), 1
#' (inverted), 2 (positive), 3 (negative), 4 (biphasic negative-positive), 5
#' (biphasic positive-negative). If the __type__ is an waveform onset or offset,
#' then __number__ can be 0 (P wave), 1 (QRS complex), 2 (T wave).
#'
#' @returns This function will either read in an annotation using the [read_annotation()] function in the format of an `annotation_table` object, or write to file/disk an `annotation_table` to a WFDB-compatible annotation file using the [write_annotation()] function.
#'
#' __IMPORTANT__: as annotation files are created by annotators that were
#' developed independently, there is a higher chance of an erroroneous file
#' being created on disk. As such, this function will note an error an return an
#' empty `annotation_table` at times.
#'
#' @inheritParams wfdb
#'
#' @inheritParams wfdb_io
#'
#' @param data An `annotation_table` containing the 6 invariant columns required
#'   by the [annotation_table()] function
#'
#' @param begin,end A `character` in the format of *HH:MM:SS* that will be used
#'   to help parse the time of the annotation. These parameters together create
#'   the time range to extract. The default of *0* is a shortcut for *00:00:00*.
#'   The *seconds* argument can include a decimal place.
#'
#' @name wfdb_annotations
#' @export
read_annotation <- function(
  record,
  annotator,
  record_dir = ".",
  begin = 0,
  end = NA_real_,
  header = NULL
) {
  stopifnot(
    "`record` must be a single character string" = is.character(
      record
    ),
    "`annotator` must be a single character string" = is.character(
      annotator
    )
  )

  record <- record[[1]]
  annotator <- annotator[[1]]
  if (!nzchar(record)) {
    stop("`record` must be provided")
  }
  if (!nzchar(annotator)) {
    stop("`annotator` must be provided")
  }

  if (is.null(header)) {
    header <- read_header(
      record = record,
      record_dir = record_dir
    )
  } else if (!inherits(header, "header_table")) {
    stop("`header` must be a `header_table` object")
  }

  begin <- as.numeric(begin)[1]
  end <- as.numeric(end)[1]

  annotation_path <- fs::path(record_dir, record, ext = annotator)
  if (!fs::file_exists(annotation_path)) {
    stop(
      "Annotation file not found for ",
      record,
      " (",
      annotator,
      ")"
    )
  }

  record_line <- attr(header, "record_line")
  frequency <- record_line$frequency
  if (length(frequency) == 0) {
    frequency <- NA_real_
  }
  frequency <- as.numeric(frequency)

  # The native reader returns raw vectors extracted from the binary WFDB
  # annotation file. These are converted into the strongly-typed columns of
  # an `annotation_table` below.
  ann_list <- read_annotation_native_cpp(annotation_path)
  samples <- as.integer(ann_list$sample)
  types <- as.character(ann_list$type)
  subtype <- as.integer(ann_list$subtype)
  channel <- as.integer(ann_list$channel)
  number <- as.integer(ann_list$number)

  # If annotation is empty, message user
  # Return annotation table with name of annotator
  if (length(samples) == 0) {
    message(
      "Annotation file for ",
      record,
      " (",
      annotator,
      ") contains no data"
    )
    return(annotation_table(annotator = annotator))
  }

  if (!is.na(begin)) {
    # Time bounds are converted into sample numbers so we can filter
    # the annotations before constructing the table returned to R.
    if (is.na(frequency) || frequency <= 0) {
      stop(
        "`begin` requires a positive sampling frequency in the header"
      )
    }
    begin_sample <- as.integer(floor(begin * frequency))
  } else {
    begin_sample <- min(samples)
  }

  if (!is.na(end)) {
    if (is.na(frequency) || frequency <= 0) {
      stop(
        "`end` requires a positive sampling frequency in the header"
      )
    }
    end_sample <- as.integer(ceiling(end * frequency))
  } else {
    end_sample <- max(samples)
  }

  selection <- samples >= begin_sample & samples <= end_sample
  samples <- samples[selection]
  types <- types[selection]
  subtype <- subtype[selection]
  channel <- channel[selection]
  number <- number[selection]

  time_strings <- if (!is.na(frequency) && frequency > 0) {
    seconds <- samples / frequency
    hours <- floor(seconds / 3600)
    minutes <- floor((seconds - hours * 3600) / 60)
    secs <- seconds - hours * 3600 - minutes * 60
    sprintf("%02d:%02d:%06.3f", hours, minutes, secs)
  } else {
    rep("", length(samples))
  }

  annotation_table(
    annotator = annotator,
    time = time_strings,
    sample = samples,
    type = types,
    subtype = subtype,
    channel = channel,
    number = number
  )
}

#' @rdname wfdb_annotations
#' @export
write_annotation <- function(
  data,
  annotator,
  record,
  record_dir = "."
) {
  stopifnot(
    "`record` must be a single character string" = is.character(
      record
    ),
    "`annotator` must be a single character string" = is.character(
      annotator
    )
  )

  record <- record[[1]]
  annotator <- annotator[[1]]
  if (!nzchar(record)) {
    stop("`record` must be provided")
  }
  if (!nzchar(annotator)) {
    stop("`annotator` must be provided")
  }

  if (!fs::dir_exists(record_dir)) {
    # Match the behaviour of write_wfdb() by creating the directory
    # before invoking the native writer.
    fs::dir_create(record_dir, recurse = TRUE)
  }

  if (!inherits(data, "annotation_table")) {
    if (!is.data.frame(data)) {
      stop(
        "`data` must be an `annotation_table` or data frame"
      )
    }
    required_cols <- c(
      "time",
      "sample",
      "type",
      "subtype",
      "channel",
      "number"
    )
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      stop(
        "`data` is missing required columns: ",
        paste(missing_cols, collapse = ", ")
      )
    }
    # Normalise arbitrary data frames into the strongly-typed
    # annotation_table class expected elsewhere in the package.
    data <- annotation_table(
      annotator = annotator,
      time = as.character(data$time),
      sample = as.integer(data$sample),
      type = as.character(data$type),
      subtype = as.character(data$subtype),
      channel = as.integer(data$channel),
      number = as.integer(data$number)
    )
  }

  ann_dt <- data.table::as.data.table(data)
  if (!"sample" %in% names(ann_dt)) {
    stop("`data` must contain a `sample` column")
  }
  # Keep annotations in chronological order; ties fall back to the row
  # position to preserve stable ordering for duplicate sample numbers.
  ann_dt <- ann_dt[order(sample, seq_len(.N))]

  samples <- as.integer(ann_dt$sample)
  if (anyNA(samples)) {
    stop("Annotation `sample` values must not contain missing data")
  }
  if (length(samples) > 0 && any(diff(samples) < 0)) {
    stop("Annotation `sample` values must be non-decreasing")
  }

  types <- as.character(ann_dt$type)
  types[is.na(types)] <- ""
  types <- trimws(types)

  parse_optional_int <- function(x) {
    if (is.null(x)) {
      return(integer(length(samples)))
    }
    vals <- suppressWarnings(as.integer(as.character(x)))
    vals[is.na(vals)] <- 0L
    vals
  }

  subtype_vals <- parse_optional_int(ann_dt$subtype)
  channel_vals <- parse_optional_int(ann_dt$channel)
  number_vals <- parse_optional_int(ann_dt$number)

  annotation_path <- fs::path(record_dir, record, ext = annotator)
  # Offload the binary encoding to the native implementation which shares
  # logic with the reader, ensuring the two functions round-trip cleanly.
  write_annotation_native_cpp(
    annotation_path = annotation_path,
    samples = samples,
    types = types,
    subtypes = subtype_vals,
    channels = channel_vals,
    numbers = number_vals
  )

  invisible(annotation_path)
}

# Annotator Systems -----------------------------------------------------------

#' Annotator systems for WFDB objects
#'
#' @description These functions create templates for annotation in R and extend the ability for developers to create their own annotation systems that are stable for WFDB objects. They are compatible with WFDB annotations and can be written out to a WFDB-compatible file. This also allows extensibility.
#' @name annotators
NULL

# Annotation reference --------------------------------------------------------

# WFDB annotation labels are stored as internal data (.surface_annotations)
# Generated from data-raw/annotations.R

#' Standard WFDB annotation nomenclature
#'
#' @description Provides the standard label definitions used by WFDB
#'   annotation files. These helper functions make it easier to
#'   interpret the contents of the [annotation_table()] object by
#'   exposing the symbol, mnemonic, and description that correspond to
#'   each label store value defined in the WFDB Applications Guide
#'   (Moody and collaborators).
#'
#' @details The returned table is derived from the WFDB Application
#'   Guide and matches the canonical label store values used by the
#'   WFDB software distribution. Entries that are not currently defined
#'   by the specification are omitted.
#'
#' @param symbol Optional character vector of WFDB annotation symbols
#'   to filter the results.
#'
#' @param label_store Optional integer vector of WFDB label store
#'   values to filter the results.
#'
#' @param annotation An [annotation_table()] or compatible data frame
#'   whose annotation symbols or label store values should be augmented
#'   with the standard WFDB nomenclature.
#'
#' @param column Name of the column within `annotation` that contains
#'   either the WFDB symbol (default, for the `type` column) or the
#'   label store values. If the column is numeric it is matched on
#'   `label_store`, otherwise a symbol lookup is performed.
#'
#' @return `wfdb_annotation_labels()` returns a data frame with columns
#'   `label_store`, `symbol`, `mnemonic`, and `description`.
#'   `wfdb_annotation_decode()` returns the input annotation
#'   table with the WFDB nomenclature columns appended.
#'
#' @references
#' Moody GB. *WFDB Applications Guide*. PhysioNet. Available at
#' <https://www.physionet.org/physiotools/wag/>.
#'
#' @examples
#' wfdb_annotation_labels()
#'
#' wfdb_annotation_labels(symbol = c("N", "V"))
#'
#' ann <- annotation_table(
#'   annotator = "example",
#'   sample = c(100L, 200L),
#'   type = c("N", "V")
#' )
#'
#' wfdb_annotation_decode(ann)
#'
#' @export
wfdb_annotation_labels <- function(symbol = NULL, label_store = NULL) {
  labels <- .surface_annotations

  if (!is.null(symbol)) {
    symbol <- as.character(symbol)
    labels <- labels[labels$symbol %in% symbol, , drop = FALSE]
  }

  if (!is.null(label_store)) {
    label_store <- as.integer(label_store)
    labels <- labels[labels$label_store %in% label_store, , drop = FALSE]
  }

  rownames(labels) <- NULL
  labels
}

#' @rdname wfdb_annotation_labels
#' @export
wfdb_annotation_decode <- function(annotation, column = "type") {
  if (missing(annotation)) {
    stop("`annotation` must be supplied")
  }

  if (!is.data.frame(annotation)) {
    stop("`annotation` must be a data frame or annotation_table")
  }

  if (!column %in% names(annotation)) {
    stop("Column `", column, "` was not found in `annotation`")
  }

  labels <- .surface_annotations

  annotation$`..row_id..` <- seq_len(nrow(annotation))

  key_column <- if (is.numeric(annotation[[column]])) {
    "label_store"
  } else {
    "symbol"
  }

  merged <- merge(
    annotation,
    labels,
    by.x = column,
    by.y = key_column,
    all.x = TRUE,
    sort = FALSE
  )

  merged <- merged[order(merged$`..row_id..`), , drop = FALSE]
  merged$`..row_id..` <- NULL

  merged
}
