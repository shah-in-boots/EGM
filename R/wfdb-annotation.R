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
read_annotation <- function(record,
                                                                                                                record_dir = ".",
                                                                                                                annotator,
                                                                                                                wfdb_path = getOption("wfdb_path"),
                                                                                                                begin = "00:00:00",
                                                                                                                end = NA_character_,
                                                                                                                backend = getOption("wfdb_backend", "native"),
                                                                                                                ...) {

        backend <- wfdb_match_backend(backend)

        if (identical(backend, "native")) {
                convert_time <- function(x) {
                        if (length(x) == 0 || is.null(x)) {
                                return(NA_real_)
                        }
                        x <- x[[1]]
                        if (is.numeric(x)) {
                                return(as.numeric(x))
                        }
                        if (is.na(x)) {
                                return(NA_real_)
                        }
                        x <- trimws(as.character(x))
                        if (!nzchar(x)) {
                                return(0)
                        }
                        if (!grepl(":", x, fixed = TRUE)) {
                                suppressWarnings(val <- as.numeric(x))
                                if (!is.na(val)) {
                                        return(val)
                                }
                        }
                        parts <- strsplit(x, ":", fixed = TRUE)[[1]]
                        parts <- as.numeric(parts)
                        parts <- rev(parts)
                        multipliers <- c(1, 60, 3600)
                        sum(parts * multipliers[seq_along(parts)], na.rm = TRUE)
                }

                return(
                        read_annotation_native(
                                record = record,
                                annotator = annotator,
                                record_dir = record_dir,
                                begin = convert_time(begin),
                                end = convert_time(end),
                                ...
                        )
                )
        }

        read_annotation_system(
                record = record,
                record_dir = record_dir,
                annotator = annotator,
                wfdb_path = wfdb_path,
                begin = begin,
                end = end,
                ...
        )
}

read_annotation_system <- function(record,
                                                                                                                record_dir = ".",
                                                                                                                annotator,
                                                                                                                wfdb_path = getOption("wfdb_path"),
                                                                                                                begin = "00:00:00",
                                                                                                                end = NA_character_,
                                                                                                                ...) {

        # Validate:
        #               WFDB software command
        #       Current or parent working directory
        #       Directory of the record/WFDB files
        #       Variable definitions
        rdann <- find_wfdb_command('rdann', wfdb_path)

        # If the annotation is inadequate, need to check here before going further
        # Example would be a file that is very small
        annPath <- fs::path(record_dir, record, ext = annotator)
        fileSize <- fs::file_size(annPath) # returns in bytes

        if (fileSize < 8) {
                # Unlikely to be readable?
                # If its only a few bytes then annotation is unlikely
                message('The annotation for ',
                                                record,
                                                ' is unlikely to be legible. ',
                                                'An empty annotation table was returned instead.')
                return(annotation_table())
        }

        # Ensure appropriate working directory
        if (fs::dir_exists(record_dir)) {
                wd <- fs::path(record_dir)
        } else {
                wd <- getwd()
        }

        stopifnot('Expected `character`' = is.character(begin))
        stopifnot('Expected `character`' = is.character(end))

        # Create all the necessary parameters for rdann
        #               -f                      Start time
        #               -t                      End time
        #               -v                      Column headings
        #               -e                      Elapsed time as (versus absolute time)
        # TODO filtering flags not yet included
        cmd <-
                paste(rdann, '-r', record, '-a', annotator) |>
                {
                        \(.) {
                                if (begin != 0) {
                                        paste(., '-f', begin)
                                } else {
                                        .
                                }
                        }
                }() |>
                {
                        \(.) {
                                if (!is.na(end)) {
                                        paste(., '-t', end)
                                } else {
                                        .
                                }
                        }
                }() |>
                paste('-e')


        # Temporary local/working directory, to reset at end of function
        withr::with_dir(new = wd, code = {
                dat <-
                        data.table::fread(cmd = cmd, header = FALSE)
        })

        # Rename annotation table
        names(dat) <- c('time', 'sample', 'type', 'subtype', 'channel', 'number')

        # The time is raw, and converted to HH:MM:SS.SSS format, without a date
        #       This could be just M:S format or H:M:S format
        # This is a discrepancy between `rdann` and this software
        # Can temporarily convert this to a POSIX format here
        hea <- read_header(record, record_dir)
        start_time <- attributes(hea)$record_line$start_time

        # Extract time appropriately
        # Also make sure the type is appropriate
        timeType <- stringr::str_count(dat$time, ':')
        dat$time <- ifelse(timeType == 1, paste0('0:', dat$time), dat$time)

        # Now split the strings by the position and check the times
        datHMS <-
                stringr::str_split(dat$time, ':', simplify = TRUE) |>
                as.data.frame()
        hours <- as.numeric(datHMS[[1]])
        minutes <- as.numeric(datHMS[[2]])
        seconds <- as.numeric(datHMS[[3]])

        # Convert to characters
        hours <- ifelse(hours < 10, paste0('0', hours), hours)
        minutes <- ifelse(minutes < 10, paste0('0', minutes), minutes)
        seconds <- ifelse(seconds < 10, paste0('0', seconds), seconds)

        dat$time <- paste0(hours, ':', minutes, ':', seconds)

        # Return
        new_annotation_table(df_list(dat), annotator)
}


#' @rdname wfdb_annotations
#' @export
write_annotation <- function(data,
                                                                                                                 annotator,
                                                                                                                 record,
                                                                                                                 record_dir = '.',
                                                                                                                 wfdb_path = getOption('wfdb_path'),
                                                                                                                 backend = getOption('wfdb_backend', 'native'),
                                                                                                                 overwrite = FALSE,
                                                                                                                 ...) {

        backend <- wfdb_match_backend(backend)

        if (identical(backend, 'native')) {
                return(
                        write_annotation_native(
                                data = data,
                                record = record,
                                annotator = annotator,
                                record_dir = record_dir,
                                overwrite = overwrite,
                                ...
                        )
                )
        }

        write_annotation_system(
                data = data,
                annotator = annotator,
                record = record,
                record_dir = record_dir,
                wfdb_path = wfdb_path,
                overwrite = overwrite,
                ...
        )
}

write_annotation_system <- function(data,
                                                                                                                 annotator,
                                                                                                                 record,
                                                                                                                 record_dir = '.',
                                                                                                                 wfdb_path = getOption('wfdb_path'),
                                                                                                                 overwrite = FALSE,
                                                                                                                 ...) {

        # Validate:
        #               WFDB software command
        #       Current or parent working directory
        #       Variable definitions
        wrann <- find_wfdb_command('wrann', wfdb_path)

        if (fs::dir_exists(record_dir)) {
                wd <- fs::path(record_dir)
        } else {
                wd <- getwd()
        }

        if (inherits(data, 'annotation_table')) {
                data <- data.table::as.data.table(data)
        } else {
                stopifnot('Expected `data.frame`' = inherits(data, 'data.frame'))
        }

        annPath <- fs::path(record_dir, record, ext = annotator)
        if (isTRUE(overwrite) && fs::file_exists(annPath)) {
                fs::file_delete(annPath)
        }

        # Take annotation data and write to temporary file
        #       This later is sent to `wrann` through `cat` with a pipe
        #               The temp file must be deleted after
        tmpFile <- fs::file_temp('annotation_', ext = 'txt')
        withr::defer(fs::file_delete(tmpFile))

        data |>
                annotation_table_to_lines() |>
                writeLines(tmpFile)

        # Prepare the command for writing this into a WFDB format
        #               Cat annotation file
        #               Pipe
        #       Write out file
        cat_cmd <- paste('cat', tmpFile)
        wfdb_cmd <- paste(wrann, '-r', record, '-a', annotator)
        cmd <- paste(cat_cmd, wfdb_cmd, sep = ' | ')
        withr::with_dir(new = wd, code = system(cmd))

}


#' @rdname wfdb_annotations
#' @export
annotate_wfdb <- function(record,
													record_dir,
													annotator,
													wfdb_path = getOption('wfdb_path'),
													...) {

	# Validate
	# 	WFDB software - must be an ECG detector software
	#		WFDB must be on path
	# 	Reading/writing directory must be on path

	if (fs::dir_exists(record_dir)) {
		wd <- fs::path(record_dir)
	} else {
		wd <- getwd()
	}

	cmd <- find_wfdb_command(annotator)
	rec <- paste("-r", record)
	ann <- paste("-a", annotator)

	# Switch based on annotator system
 	# Change working directory for writing purposes
 	# This should change back at end of writing process
	switch(annotator,
				 ecpugwave = {
				 	withr::with_dir(new = wd,
				 									code = {
				 										# System call to beat detector/annotator
				 										system2(
				 											command = cmd,
				 											args = c(rec, ann),
				 											stdout = FALSE,
				 											stderr = FALSE
				 										)

				 										if (fs::file_exists('fort.20')) {
				 											fs::file_delete('fort.20')
				 										}
				 										if (fs::file_exists('fort.21')) {
				 											fs::file_delete('fort.21')
				 										}
				 									})

				 },
				 wqrs = {
				 	withr::with_dir(new = wd,
				 									code = system2(
				 										command = cmd,
				 										args = c(rec, ann),
				 										stdout = FALSE,
				 										stderr = FALSE
				 									))
				 },
				 gqrs = {
				 	withr::with_dir(new = wd,
				 									code = system2(
				 										command = cmd,
				 										args = c(rec, ann),
				 										stdout = FALSE,
				 										stderr = FALSE
				 									))
				 },
				 sqrs = {
				 	withr::with_dir(new = wd,
				 									code = system2(
				 										command = cmd,
				 										args = c(rec, ann),
				 										stdout = FALSE,
				 										stderr = FALSE
				 									))
				 },
	)
}

# Annotator Systems -----------------------------------------------------------

#' Annotator systems for WFDB objects
#'
#' @description These functions create templates for annotation in R and extend the ability for developers to create their own annotation systems that are stable for WFDB objects. They are compatible with WFDB annotations and can be written out to a WFDB-compatible file. This also allows extensibility.
#' @name annotators
NULL
