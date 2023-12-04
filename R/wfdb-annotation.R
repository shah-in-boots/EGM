# Annotation -------------------------------------------------------------------

#' Read WFDB-compatible annotation file
#'
#' @description
#'
#' Individual annotation types are described as both a command-line tool for
#' annotating WFDB-files, as well as the extension that is appended to the
#' `record` name to notate the type. Generally, the types of annotations that
#' are supported are described below:
#'
#'   * atr = manually reviewed and corrected reference annotation files
#'
#'   * ann = general annotator file
#'
#'   * ecgpuwave = files contain surface ECG demarcation (P, QRS, and T waves)
#'
#'   * sqrs/wqrs/gqrs = standard WFDB peak detection for R waves
#'
#' A more detailed explanation is given below.
#'
#' Additionally, files when being read in are converted from a binary format to
#' a textual format. The raw data however may be inadequate, as the original
#' annotation may be erroneous. In these cases, an empty `annotation_table`
#' object will be returned.
#'
#' @details
#' # Annotation files
#'
#' The following annotation file types are described below.
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
#' @inheritParams wfdb
#' @inheritParams wfdb_io
#'
#' @name wfdb_annotations
#' @export
read_annotation <- function(record,
														record_dir = ".",
														annotator,
														wfdb_path = getOption("wfdb_path"),
														begin = 0,
														end = NULL,
														...) {

	# Validate:
	#		WFDB software command
	# 	Current or parent working directory
	# 	Directory of the record/WFDB files
	# 	Variable definitions
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

	checkmate::assert_number(begin)

	# Create all the necessary parameters for rdann
	#		-f			Start time
	#		-t			End time
	#		-v			Column headings
	#		-e			Elapsed time as (versus absolute time)
	# TODO filtering flags not yet included
	cmd <-
		paste(rdann, '-r', record, '-a', annotator) |>
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
				if (!is.null(end)) {
					paste(., "-t", end)
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

	# Rename and return as annotation table
	names(dat) <- c('time', 'sample', 'type', 'subtype', 'channel', 'number')
	new_annotation_table(df_list(dat), annotator)
}

#' @param data A table containing 6 columns
#' @return Outputs a WFDB with the provided extension
#' @rdname wfdb_annotations
#' @export
write_annotation <- function(data,
														 annotator,
														 record,
														 record_dir = ".",
														 wfdb_path = getOption("wfdb_path"),
														 ...) {


	# Validate:
	#		WFDB software command
	# 	Current or parent working directory
	# 	Variable definitions
	wrann <- find_wfdb_command('wrann')

	if (fs::dir_exists(record_dir)) {
		wd <- fs::path(record_dir)
	} else {
		wd <- getwd()
	}

	checkmate::assert_data_frame(data)

	# Take annotation data and write to temporary file
	# 	This later is sent to `wrann` through `cat` with a pipe
	#		The temp file must be deleted after
	tmpFile <- fs::file_temp("annotation_", ext = "txt")
	withr::defer(fs::file_delete(tmpFile))

	data |>
		annotation_table_to_lines() |>
		writeLines(tmpFile)

	# Prepare the command for writing this into a WFDB format
	#		Cat annotation file
	#		Pipe
	# 	Write out file
	cat_cmd <- paste('cat', tmpFile)
	wfdb_cmd <- paste(wrann, '-r', record, '-a', annotator)
	cmd <- paste(cat_cmd, wfdb_cmd, sep = " | ")
	withr::with_dir(new = wd, code = system(cmd))

}
#' @rdname wfdb_annotations
#' @inheritParams wfdb
#' @inheritParams wfdb_io
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

