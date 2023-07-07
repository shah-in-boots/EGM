# Annotation -------------------------------------------------------------------

#' Read WFDB-compatible annotation file
#'
#' @inheritParams rewrite_wfdb
#' @inheritParams read_wfdb
#'
#' @details
#'
#' # Annotation files
#'
#' The types of annotations that are supported are described below:
#'
#' * atr = manually reviewed and corrected reference annotation files
#'
#' * ann = general annotator file
#' @export
read_annotation <- function(record,
														annotator,
														record_dir = ".",
														wfdb_path = getOption("wfdb_path"),
														begin = 0,
														end = NULL,
														...) {

	# Validate:
	#		WFDB software command
	# 	Current or parent working directory
	# 	Directory of the record/WFDB files
	# 	Variable definitions
	rdann <- find_wfdb_command('rdann')

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

	# Rename and return
	names(dat) <- c("time", "sample", "type", "subtype", "channel", "number")
	dat
}

#' Write an annotation into a WFDB-compatible file
#' @param data A table containing 6 columns
#' @return Outputs a WFDB with the provided extension
#' @export
write_annotation <- function(data,
														 record,
														 annotator,
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
