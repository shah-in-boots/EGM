# nocov start
wfdb <- NULL

#' Describes signals based on WFDB-formatted files
#'
#' @param record String that will be used to name the WFDB record. Cannot
#'   include extensions, and is not a filepath. alphanumeric characters are
#'   acceptable, as well as hyphens (-) and underscores (_)
#'
#' @param location The directory that the target record is located within. As
#'   this is related to the [PhysioNet](https://physionet.org), using the
#'   location name `mitdb` will access the online directory for the MIT
#'   Database.
#'
describe_wfdb2 <- function(record,
													 location = ".") {
	if (location == "mitdb") {
		# nocov start
		out <-
			reticulate::py_capture_output(wfdb$wfdbdesc(record, location))
		# nocov end
	} else {
		fp <- file.path(location, record)
		ft <- paste0(fp, c(".ann", ".atr", ".dat", ".hea", ".sig"))
		stopifnot("The record name does not exist within the directory"
							= any(file.exists(ft)))
		# nocov start
		out <-
			reticulate::py_capture_output(wfdb$wfdbdesc(reticulate::r_to_py(fp)))
		# nocov end
	}

}


write_wfdb2 <- function(file,
												type,
												record,
												write_location = ".",
												...) {
	# Read in data appropriately
	switch(type,
				 lspro = {

				 	hea <- read_lspro_header(file)
				 	sig <- as.matrix(read_lspro_signal(file, n = Inf))

				 	# nocov start
				 	dt <- reticulate::import("datetime", convert = FALSE)
				 	start_time <- dt$datetime(
				 		year(hea$start_time),
				 		month(hea$start_time),
				 		mday(hea$start_time),
				 		hour(hea$start_time),
				 		minute(hea$start_time),
				 		second(hea$start_time)
				 	)

				 	invisible(reticulate::py_capture_output(
				 		wfdb$wrsamp(
				 			record_name = reticulate::r_to_py(record),
				 			fs = reticulate::r_to_py(hea$freq),
				 			units = reticulate::r_to_py(as.list(rep(
				 				"mV", hea$number_of_channels
				 			))),
				 			sig_name = reticulate::r_to_py(hea$channels$label),
				 			d_signal = reticulate::r_to_py(sig),
				 			adc_gain = reticulate::r_to_py(hea$channels$gain / hea$ADC_saturation),
				 			fmt = reticulate::r_to_py(as.list(rep(
				 				"16", hea$number_of_channels
				 			))),
				 			baseline = reticulate::r_to_py(as.list(rep(
				 				0L, hea$number_of_channels
				 			))),
				 			base_datetime = start_time,
				 			write_dir = reticulate::r_to_py(write_location)
				 		)
				 	))
				 	# nocov end

				 	message("`write_wfdb` successfully wrote `",
				 					record,
				 					"` to `",
				 					write_location,
				 					"`")


				 },
				 message("`write_wfdb` not supported for the supplied `type`"))

}

#' Read in WFDB-compatible signal files
#'
#' `read_wfdb()` reads in signal files based on the `rdsamp` function provided
#' by the original WFDB toolbox. It requires a signal file, a header file, and
#' an annotation file (if applicable).
#'
#' @return Returns a list of two objects. The first object is a matrix of
#'   signals, with each column representing a specific channel. Each row is a
#'   sample point. The second object is a summary of the header field, with
#'   information on channel names/number, signal length, sampling frequency,
#'   etc.
read_wfdb <-
	function(record,
					 location = ".",
					 channels = NULL,
					 ...) {
		fp <- file.path(location, record)
		ft <- paste0(fp, c(".ann", ".atr", ".dat", ".hea", ".sig"))
		stopifnot("The record name does not exist within the directory" =
								any(file.exists(ft)))

		# nocov start
		# Return list of 2 (from py to r)
		reticulate::py_to_r(
			wfdb$rdsamp(
				record_name = reticulate::r_to_py(fp),
				channel_names = reticulate::r_to_py(channels)
			)
		)
		# nocov end
	}

#' Read in WFDB-compatible annotation files
#'
#' @param annotator A character string (usually 3 letters) that represents the
#'   type of annotation file or algorithm that generated it. These are the
#'   naming conventions used in the WFDB toolkits
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
#'
#' @return Annotation file
read_annotation2 <- function(record, annotator, location,  ...) {
	fp <- file.path(location, record)

	# nocov start
	annotation <-
		reticulate::py_to_r(wfdb$rdann(
			record_name = reticulate::r_to_py(fp),
			extension = reticulate::r_to_py(extension)
		))


	# Index position
	annotationSample <- annotation$sample

	# Labels or annotations (an array)
	annotationSymbol <- annotation$symbol

	# Subtype of annotation or category
	annotationSubtype <- annotation$subtype

	# Length or number of annotations
	annotationNumber <- annotation$ann_len

	# The channel the annotation was applied to
	annotationChannel <- annotation$chan

	annotationNumber <- annotation$num

	# Additional notes obatined for the rhythm as a string
	annotationNotes <- annotation$aux_note
	# nocov end

	list(
		sample = annotationSample,
		label = annotationSymbol,
		subtype = annotationSubtype,
		channel = annotationChannel,
		number = annotationNumber,
		auxillary_note = annotationNotes
	)
}

detect_beats2 <- function(record, location, detector, ...) {
	# ECGPUWAVE command
	detector <- '/usr/local/bin/ecgpuwave'
	rec <- paste('-r', file.path(location, record))
	ann <- paste('-a', basename(detector))

	if (ann == "ecgpuwave") {
		ann <- "blm"
	}


	# Writing files in same location as the original location
	system2(command = detector,
					args = c(rec, ann))

}

# nocov end
