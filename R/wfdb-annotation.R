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
#' onset `(` and waveform offset `)`. The __number__ column gives further
#' information about each of these __type__ labels.
#'
#' The __number__ column gives modifier information. If the __type__ classifier
#' is a T wave annotation, the __number__ column can be 0 (normal), 1
#' (inverted), 2 (positive), 3 (negative), 4 (biphasic negative-positive), 5
#' (biphasic positive-negative). If the __type__ is an waveform onset or offset,
#' then __number__ can be 0 (P wave), 1 (QRS complex), 2 (T wave).
#'
#' @returns [read_annotation()] returns either a single `annotation_table` object
#'   (when one annotator is specified) or a named list of `annotation_table`
#'   objects (when multiple annotators are specified). [write_annotation()]
#'   writes an `annotation_table` to a WFDB-compatible annotation file.
#'
#' __IMPORTANT__: as annotation files are created by annotators that were
#' developed independently, there is a higher chance of an erroneous file
#' being created on disk. As such, this function will note an error and return an
#' empty `annotation_table` at times.
#'
#' @inheritParams wfdb
#'
#' @inheritParams wfdb_io
#'
#' @param data An `annotation_table` containing the 6 invariant columns required
#'   by the [annotation_table()] function
#'
#' @name wfdb_annotations
#' @export
read_annotation <- function(
  record,
  annotator,
  record_dir = ".",
  begin = NULL,
  end = NULL,
  header = NULL,
  interval = NULL,
  channel_zero = c("auto", "global", "signal")
) {
  channel_zero <- match.arg(channel_zero)
  stopifnot(
    "`record` must be a single character string" = is.character(
      record
    ),
    "`annotator` must be a character vector" = is.character(
      annotator
    )
  )

  record <- record[[1]]
  if (!nzchar(record)) {
    stop("`record` must be provided")
  }
  if (length(annotator) == 0 || any(!nzchar(annotator))) {
    stop("`annotator` must contain at least one non-empty string")
  }

  # Handle multiple annotators by calling single-annotator logic for each
  if (length(annotator) > 1) {
    # Read header once for efficiency
    if (is.null(header)) {
      header <- read_header(
        record = record,
        record_dir = record_dir
      )
    } else if (!inherits(header, "header_table")) {
      stop("`header` must be a `header_table` object")
    }

    # Read each annotator
    annotations <- lapply(annotator, function(ann) {
      read_annotation_single(
        record = record,
        annotator = ann,
        record_dir = record_dir,
        begin = begin,
        end = end,
        interval = interval,
        header = header,
        channel_zero = channel_zero
      )
    })
    names(annotations) <- annotator
    return(annotations)
  }

  # Single annotator - use existing logic
  annotator <- annotator[[1]]
  read_annotation_single(
    record = record,
    annotator = annotator,
    record_dir = record_dir,
    begin = begin,
    end = end,
    interval = interval,
    header = header,
    channel_zero = channel_zero
  )
}

# Internal helper function for reading a single annotator
read_annotation_single <- function(
  record,
  annotator,
  record_dir = ".",
  begin = NULL,
  end = NULL,
  header = NULL,
  interval = NULL,
  channel_zero = "auto"
) {
  if (is.null(header)) {
    header <- read_header(
      record = record,
      record_dir = record_dir
    )
  } else if (!inherits(header, "header_table")) {
    stop("`header` must be a `header_table` object")
  }

  annotation_path <- file.path(record_dir, paste0(record, ".", annotator))
  if (!file.exists(annotation_path)) {
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
  start_time <- record_line$start_time

  # The native reader returns raw vectors extracted from the binary WFDB
  # annotation file. These are converted into the strongly-typed columns of
  # an `annotation_table` below.
  ann_list <- read_annotation_native_cpp(annotation_path)
  samples <- as.integer(ann_list$sample)
  types <- as.character(ann_list$type)
  subtype <- as.integer(ann_list$subtype)
  channel <- as.integer(ann_list$channel)
  number <- as.integer(ann_list$number)
  aux <- as.character(ann_list$aux)

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

  sample_range <- wfdb_sample_range(
    begin = begin,
    end = end,
    interval = interval,
    frequency = frequency,
    total_samples = record_line$samples,
    start_time = start_time
  )
  begin_sample <- sample_range$begin
  end_sample <- sample_range$end

  selection <- samples >= begin_sample & samples < end_sample
  samples <- samples[selection]
  types <- types[selection]
  subtype <- subtype[selection]
  channel <- channel[selection]
  number <- number[selection]
  aux <- aux[selection]

  time_strings <- if (!is.na(frequency) && frequency > 0) {
    seconds <- samples / frequency
    hours <- floor(seconds / 3600)
    minutes <- floor((seconds - hours * 3600) / 60)
    secs <- seconds - hours * 3600 - minutes * 60
    sprintf("%02d:%02d:%06.3f", hours, minutes, secs)
  } else {
    rep("", length(samples))
  }

  if (identical(channel_zero, "auto")) {
    channel_zero <- resolve_channel_convention(channel, header, record, annotator)
  }

  annotation_table(
    annotator = annotator,
    time = time_strings,
    sample = samples,
    type = types,
    subtype = subtype,
    channel = channel,
    number = number,
    aux = aux,
    channel_zero = channel_zero
  )
}

#' Settle how a channel column counts its signals
#'
#' @description The one place the two conventions can be told apart, because it
#'   is the only one holding both the channels and the number of signals the
#'   header declares. Channels running `0 .. nsig-1` exactly fill the signals if
#'   `0` is the first of them, and are a global channel plus every lead but the
#'   last otherwise; nothing in the file says which.
#'
#' @details Read the wrong way round, such a file misplaces every lead by one -
#'   as global, its first lead becomes unselectable and rides along with every
#'   other; as signal, a real global channel becomes lead I - and nothing
#'   downstream can notice. So it is refused here, once, where the file is
#'   opened, with a message naming both declarations. Every other column is
#'   unambiguous: all zero is an absence of information, and one that reaches
#'   `nsig` already counts from one.
#'
#' @param channel The channel column as read.
#' @param header The record's `header_table`, or `NULL`.
#' @param record,annotator Named in the message.
#'
#' @return `"global"`, the convention every unambiguous file is read under.
#'
#' @keywords internal
resolve_channel_convention <- function(channel, header, record, annotator) {
  observed <- sort(unique(channel[!is.na(channel)]))
  nsig <- if (is.null(header)) {
    NULL
  } else {
    attr(header, "record_line")$number_of_channels
  }
  if (
    length(nsig) == 0 || is.na(nsig[1]) || length(observed) < 2L ||
      !identical(as.integer(observed), seq_len(as.integer(nsig[1])) - 1L)
  ) {
    return("global")
  }

  stop(
    "The `channel` column of ", record, ".", annotator, " runs 0 to ",
    nsig[1] - 1, " on a record of ", nsig[1], " signals, which counts them ",
    "from 0 if it is one channel per signal, and is a global channel plus ",
    "every lead but the last if it counts from 1. Nothing in the file says ",
    "which, and each reading misplaces a lead under the other. Pass ",
    "`channel_zero = \"signal\"` if the annotator numbered signals from 0, as ",
    "the WFDB tools do, or `channel_zero = \"global\"` if 0 marks annotations ",
    "belonging to no lead.",
    call. = FALSE
  )
}

#' @rdname wfdb_annotations
#' @export
write_annotation <- function(
  data,
  annotator,
  record,
  record_dir = ".",
  channel_zero = NULL
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

  if (!dir.exists(record_dir)) {
    # Match the behaviour of write_wfdb() by creating the directory
    # before invoking the native writer.
    dir.create(record_dir, recursive = TRUE)
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
    aux_col <- if ("aux" %in% names(data)) as.character(data$aux) else character()
    data <- annotation_table(
      annotator = annotator,
      time = as.character(data$time),
      sample = as.integer(data$sample),
      type = as.character(data$type),
      subtype = as.character(data$subtype),
      channel = as.integer(data$channel),
      number = as.integer(data$number),
      aux = aux_col
    )
  }

  # The table counts signals from 1 with 0 the global channel; the file goes out
  # the way it came in unless told otherwise
  if (is.null(channel_zero)) {
    channel_zero <- channel_zero(data)
  }
  channel_zero <- match.arg(channel_zero, c("global", "signal"))

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

  # A lead name in the channel column has no place in the file, which stores an
  # integer, and `parse_optional_int()` would write it as the global channel 0
  # without a word. Names are resolved when a table is added to a record.
  if (is.character(ann_dt$channel)) {
    unresolved <- unique(ann_dt$channel[
      is.na(suppressWarnings(as.integer(ann_dt$channel)))
    ])
    if (length(unresolved) > 0) {
      stop(
        "`channel` holds names (",
        paste(unresolved, collapse = ", "),
        ") which an annotation file cannot store. Add the table to its record ",
        "with `add_annotation()` first, which resolves them to signal numbers.",
        call. = FALSE
      )
    }
  }
  channel_vals <- parse_optional_int(ann_dt$channel)
  number_vals <- parse_optional_int(ann_dt$number)

  # A file counting from 0 has no place for a global annotation: the value it
  # would take is the first signal's, and the native writer would refuse the
  # -1 with a message that names neither.
  if (identical(channel_zero, "signal")) {
    if (any(channel_vals == 0L)) {
      stop(
        "`channel_zero = \"signal\"` writes channels counted from 0, and ",
        sum(channel_vals == 0L), " annotation(s) sit on the global channel 0, ",
        "which that convention cannot hold. Write with ",
        "`channel_zero = \"global\"` to keep them, or drop them first.",
        call. = FALSE
      )
    }
    channel_vals <- channel_vals - 1L
  }

  # Extract auxiliary data strings if present in the annotation table
  aux_vals <- if ("aux" %in% names(ann_dt)) {
    as.character(ann_dt$aux)
  } else {
    rep("", length(samples))
  }
  aux_vals[is.na(aux_vals)] <- ""

  annotation_path <- file.path(record_dir, paste0(record, ".", annotator))
  # Offload the binary encoding to the native implementation which shares
  # logic with the reader, ensuring the two functions round-trip cleanly.
  write_annotation_native_cpp(
    annotation_path = annotation_path,
    samples = samples,
    types = types,
    subtypes = subtype_vals,
    channels = channel_vals,
    numbers = number_vals,
    aux_strings = aux_vals
  )

  invisible(annotation_path)
}

# Annotator Systems -----------------------------------------------------------

#' Annotator systems for WFDB objects
#'
#' @description These functions create templates for annotation in R and extend the ability for developers to create their own annotation systems that are stable for WFDB objects. They are compatible with WFDB annotations and can be written out to a WFDB-compatible file. This also allows extensibility.
#'
#' @details
#'
#' # Choosing an annotator
#'
#' What this package needs from a delineating annotator is narrower than it
#' looks. Wave identity is recovered positionally by [label_waves()], from the
#' peak symbol enclosed by each `(`/`)` bracket pair, so an annotator that leaves
#' the WFDB `number` column at zero throughout - which by the letter of the
#' convention means every onset marks a P wave - is still perfectly usable. The
#' requirement is only that each wave is bracketed around a typed peak (`p`, `N`,
#' or `t`).
#'
#' An annotator run per lead writes one such set of brackets for each lead,
#' distinguished by the `channel` column, which is resolved by naming a guiding
#' `channel`; see [channels].
#'
#' @seealso [label_waves()], [read_annotation()], [channels]
#'
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

# Multi-annotator helper functions --------------------------------------------

#' Get annotation from EGM object
#'
#' @description Extract an annotator's annotation table from an `EGM` object,
#'   which may hold several.
#'
#' @details The shape returned matches [read_annotation()]: a single
#'   `annotation_table` when the record carries one annotator, and a named list
#'   of them when it carries several. That symmetry is the point of the function
#'   - code can move between a record read from disk and one held in memory
#'   without changing shape, and an empty record answers with an empty
#'   `annotation_table` rather than an empty list.
#'
#'   It is also the way to reach annotations that only exist in memory. The
#'   sample indices carried by an `EGM` are rescaled by [change_frequency()],
#'   while a copy read separately from disk is not, so after a rate change these
#'   are the annotations that agree with the signal.
#'
#' @param x An `EGM` object containing annotations
#' @param annotator Character string specifying which annotator to extract.
#'   If NULL (default), returns every annotator's table.
#'
#' @returns An `annotation_table` for the requested annotator; with `annotator =
#'   NULL`, the single `annotation_table` present, or a named list of them when
#'   the record carries more than one.
#'
#' @seealso [read_annotation()], [list_annotators()], [change_frequency()]
#'
#' @export
get_annotation <- function(x, annotator = NULL) {
  if (!inherits(x, "EGM")) {
    stop("`x` must be an egm object", call. = FALSE)
  }

  ann <- x$annotation

  # annotation is always a list now
  # Check if it's an unnamed list (no annotators)
  annotators <- names(ann)
  has_annotators <- !is.null(annotators) && length(annotators) > 0

  # Handle empty annotations
  if (!has_annotators) {
    if (is.null(annotator)) {
      # An unnamed list is how a record with no annotator is stored; the shape
      # a caller asked for is still a table, so hand back an empty one
      return(if (length(ann) == 1L) ann[[1]] else annotation_table())
    } else {
      stop("No annotations available in this egm object", call. = FALSE)
    }
  }

  # Return all if no specific annotator requested. One annotator unwraps to its
  # own table, which is what `read_annotation()` returns for the same record.
  if (is.null(annotator)) {
    if (length(ann) == 1L) {
      return(ann[[1]])
    }
    return(ann)
  }

  # Get specific annotator from list
  if (!annotator %in% annotators) {
    stop(
      "Annotator '",
      annotator,
      "' not found. Available: ",
      paste(annotators, collapse = ", "),
      call. = FALSE
    )
  }
  return(ann[[annotator]])
}

#' List annotators in EGM object
#'
#' @description Get the names of all annotators present in an `egm` object.
#'
#' @param x An `egm` object containing annotations
#'
#' @returns A character vector of annotator names
#'
#' @export
list_annotators <- function(x) {
  if (!inherits(x, "EGM")) {
    stop("`x` must be an egm object", call. = FALSE)
  }

  ann <- x$annotation

  # annotation is always a list now - return names as character vector
  # names() returns NULL for unnamed list (no annotators), so convert to character()
  ann_names <- names(ann)
  if (is.null(ann_names) || length(ann_names) == 0) {
    return(character(0))
  }
  ann_names
}

#' Add an annotation table to an EGM object
#'
#' @description Add an `annotation_table` to an `egm` object, with validation
#'   to ensure compatibility. The annotation table is added to the list of
#'   annotations using the annotator name as the list name. If an annotation
#'   with the same annotator name already exists, you can choose to overwrite
#'   it or merge the annotations.
#'
#' @param x An `egm` object to which the annotation will be added
#' @param annotation An `annotation_table` object to add to the `egm` object
#' @param overwrite Logical. If an annotation with the same annotator name
#'   already exists, should it be overwritten? If `FALSE` (default), the
#'   annotations will be merged. If `TRUE`, the existing annotation will be
#'   replaced.
#'
#' @details
#' # Validation
#'
#' The function performs several validation checks to ensure the annotation
#' table is compatible with the `egm` object:
#'
#' * **Channel validation**: A channel given as a lead name (`"II"`) is
#'   resolved against the record's header to its signal number, so the table
#'   stored on the object always counts channels the way [channels] describes.
#'   Every channel other than the global channel `0` has to exist in the
#'   header; a name the record does not carry is an error.
#' * **Sample validation**: Checks that all sample indices in the annotation
#'   table are within the valid range of samples in the recording.
#'
#' # Handling Existing Annotations
#'
#' When adding an annotation table with an annotator name that already exists:
#'
#' * **Overwrite mode** (`overwrite = TRUE`): The existing annotation table
#'   is completely replaced with the new one.
#' * **Merge mode** (`overwrite = FALSE`): The new annotations are combined
#'   with the existing ones, duplicates are removed, and the result is sorted
#'   by sample number.
#'
#' # Empty Annotations
#'
#' If the `egm` object has no existing annotations (empty list or blank
#' annotation table), the new annotation table will replace the empty
#' annotations.
#'
#' @returns The modified `egm` object with the annotation table added
#'
#' @examples
#' \dontrun{
#' # Add a new annotation table to an egm object
#' egm <- add_annotation(egm, my_annotation_table)
#'
#' # Overwrite an existing annotation
#' egm <- add_annotation(egm, new_annotation, overwrite = TRUE)
#'
#' # Merge with existing annotation (default)
#' egm <- add_annotation(egm, additional_annotation, overwrite = FALSE)
#' }
#'
#' @export
add_annotation <- function(x, annotation, overwrite = FALSE) {
  # Validate inputs
  if (!inherits(x, "EGM")) {
    stop("`x` must be an egm object", call. = FALSE)
  }

  if (!inherits(annotation, "annotation_table")) {
    stop("`annotation` must be an `annotation_table` object", call. = FALSE)
  }

  # Get annotator name from the annotation_table attribute
  annotator_name <- attr(annotation, "annotator")
  if (is.null(annotator_name) || length(annotator_name) == 0 || annotator_name == "") {
    stop(
      "`annotation_table` must have an 'annotator' attribute. ",
      "This should be set when creating the annotation_table.",
      call. = FALSE
    )
  }

  # This is where an annotation crosses into a record, so it is where a channel
  # written as a lead name becomes the signal number every consumer matches on.
  # Downstream nothing reads names: `annotation_channels()` coerces to integer
  # and drops what fails, and the writer would store a name as channel 0.
  chan <- annotation$channel
  if (is.character(chan)) {
    names <- setdiff(unique(chan), c("0", NA_character_))
    resolved <- vapply(
      names,
      function(name) {
        # A number written as text ("3") is already a channel number
        number <- suppressWarnings(as.integer(name))
        if (!is.na(number)) number else resolve_channel_spec(x, name)
      },
      integer(1)
    )
    chan <- unname(resolved[match(chan, names)])
    chan[is.na(chan)] <- 0L
    data.table::set(annotation, j = "channel", value = chan)
  }
  chan <- as.integer(chan)

  # Validate channels (except channel 0 which is global/default). The header's
  # `label` is a factor, so it is never combined with `number` here: `c()` on a
  # factor yields its integer codes, not its labels.
  header_channels <- as.integer(x$header$number)
  invalid_channels <- setdiff(unique(chan[chan != 0L]), header_channels)
  if (length(invalid_channels) > 0) {
    stop(
      "Annotation contains invalid channels: ",
      paste(invalid_channels, collapse = ", "),
      "\nAvailable channels: ",
      paste(header_channels, collapse = ", "),
      call. = FALSE
    )
  }

  # Validate samples are within range
  record_line <- attr(x$header, "record_line")
  max_samples <- record_line$samples

  if (nrow(annotation) > 0) {
    annotation_samples <- annotation$sample
    invalid_samples <- annotation_samples[
      annotation_samples < 0 | annotation_samples >= max_samples
    ]

    if (length(invalid_samples) > 0) {
      stop(
        "Annotation contains samples outside valid range [0, ",
        max_samples,
        "): ",
        paste(utils::head(invalid_samples, 5), collapse = ", "),
        if (length(invalid_samples) > 5) "..." else "",
        call. = FALSE
      )
    }
  }

  # Get current annotations
  current_annotations <- x$annotation

  # Check if we have any existing annotations
  existing_annotators <- names(current_annotations)
  has_existing <- !is.null(existing_annotators) && length(existing_annotators) > 0

  # Handle empty annotations - replace with new annotation
  if (!has_existing) {
    x$annotation <- stats::setNames(list(annotation), annotator_name)
    message("Added annotation table '", annotator_name, "' to egm object")
    return(x)
  }

  # Check if annotator already exists
  if (annotator_name %in% existing_annotators) {
    if (overwrite) {
      # Overwrite mode - replace existing annotation
      current_annotations[[annotator_name]] <- annotation
      x$annotation <- current_annotations
      message("Replaced annotation table '", annotator_name, "' in egm object")
    } else {
      # Merge mode - combine annotations
      existing_ann <- current_annotations[[annotator_name]]

      # Combine the two annotation tables
      merged <- data.table::rbindlist(list(existing_ann, annotation))

      # Remove duplicates and sort by sample
      merged <- unique(merged)
      data.table::setorderv(merged, "sample")

      # Preserve the annotation_table class and attributes
      aux_col <- if ("aux" %in% names(merged)) merged$aux else rep("", nrow(merged))
      merged <- new_annotation_table(
        x = list(
          time = merged$time,
          sample = merged$sample,
          type = merged$type,
          subtype = merged$subtype,
          channel = merged$channel,
          number = merged$number,
          aux = aux_col
        ),
        annotator = annotator_name
      )

      current_annotations[[annotator_name]] <- merged
      x$annotation <- current_annotations
      message("Merged new annotations with existing '", annotator_name, "' in egm object")
    }
  } else {
    # Annotator doesn't exist - add it
    current_annotations[[annotator_name]] <- annotation
    x$annotation <- current_annotations
    message("Added annotation table '", annotator_name, "' to egm object")
  }

  return(x)
}

#' Merge multiple annotations into a single table
#'
#' @description Combine multiple annotation tables from an `egm` object into
#'   a single annotation table with an additional `annotator` column to
#'   identify the source of each annotation.
#'
#' @param x An `egm` object containing annotations, or a named list of
#'   `annotation_table` objects
#' @param annotators Optional character vector specifying which annotators
#'   to merge. If NULL (default), merges all available annotators.
#'
#' @returns A single `annotation_table` object with all annotations combined
#'   and an additional `annotator` column
#'
#' @export
merge_annotations <- function(x, annotators = NULL) {
  # Handle both egm objects and lists of annotation_tables
  if (inherits(x, "EGM")) {
    ann_list <- x$annotation
  } else if (is.list(x)) {
    # Validate that all elements are annotation_tables
    valid <- all(vapply(x, inherits, logical(1), "annotation_table"))
    if (!valid) {
      stop(
        "All elements must be annotation_table objects",
        call. = FALSE
      )
    }
    ann_list <- x
  } else {
    stop(
      "`x` must be an egm object or a list of annotation_table objects",
      call. = FALSE
    )
  }

  # Check if it's an unnamed list (no annotators)
  ann_names <- names(ann_list)
  has_annotators <- !is.null(ann_names) && length(ann_names) > 0

  # Handle empty annotations (unnamed list)
  if (!has_annotators) {
    return(annotation_table())
  }

  # Filter to requested annotators
  if (!is.null(annotators)) {
    missing <- setdiff(annotators, ann_names)
    if (length(missing) > 0) {
      stop(
        "Annotators not found: ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
    ann_list <- ann_list[annotators]
    ann_names <- names(ann_list)
  }

  # Add annotator column to each table and combine
  ann_with_source <- lapply(ann_names, function(ann_name) {
    dt <- data.table::as.data.table(ann_list[[ann_name]])
    dt$annotator <- ann_name
    dt
  })

  # Combine all tables
  combined <- data.table::rbindlist(ann_with_source, fill = TRUE)

  # Reorder columns to put annotator after the standard columns
  standard_cols <- c("time", "sample", "type", "subtype", "channel", "number", "aux")
  available_standard <- intersect(standard_cols, names(combined))
  other_cols <- setdiff(names(combined), c(available_standard, "annotator"))
  new_order <- c(available_standard, "annotator", other_cols)
  data.table::setcolorder(combined, new_order)

  # Sort by sample then annotator for consistent ordering
  data.table::setorderv(combined, c("sample", "annotator"))

  combined
}
