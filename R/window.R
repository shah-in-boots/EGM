#' Window signal data based on different methods
#'
#' @description
#' Creates windows of signal data using various methods, such as rhythm patterns,
#' time intervals, or reference points. Each window is returned as an individual
#' `egm` object for further analysis.
#'
#' @details
#' This function provides a modular approach to windowing electrophysiological
#' signals. The method parameter determines the windowing strategy, with each
#' method requiring its own set of additional parameters.
#'
#' @param object Object of the `egm` class, which includes header, signal
#'   information, and annotation information.
#'
#' @param method A `character` string specifying the windowing method. Options include:
#'   * "rhythm" - Windows based on rhythm patterns (requires rhythm_type and criteria)
#'
#' @param rhythm_type A `character` string specifying the rhythm type (e.g., "sinus").
#'   Currently supported: "sinus" (requires reference check).
#'
#' @param onset_criteria A named list of criteria to identify onset points.
#'   Names should match column names in the annotation table.
#'
#' @param offset_criteria A named list of criteria to identify offset points.
#'   Names should match column names in the annotation table.
#'
#' @param reference_criteria A named list of criteria to identify reference points
#'   that must exist between onset and offset. Set to NULL to skip reference validation.
#'
#' @param adjust_sample_indices Logical, whether to adjust annotation sample indices
#'   in the returned windows to be relative to the window start. Default is TRUE.
#'
#' @param ... Additional arguments passed to specific windowing methods.
#'
#' @return A list of `egm` objects, each representing a window of the original signal.
#'
#' @export
window_signal <- function(object,
													method = c("rhythm"),
													...) {

	# Validate input
	stopifnot("Requires object of <egm> class for evaluation" = inherits(object, 'egm'))

	# Match the method argument
	method <- match.arg(method)

	# Dispatch to the appropriate method handler
	switch(method,
				 rhythm = window_by_rhythm(object, ...),
				 # Add additional methods here in the future
				 stop("Unsupported windowing method: ", method))
}

#' Window signal by rhythm patterns
#' Creates windows around rhythm patterns using flexible annotation criteria.
#' @keywords internal
window_by_rhythm <- function(object,
														 rhythm_type = "sinus",
														 onset_criteria,
														 offset_criteria,
														 reference_criteria = NULL,
														 adjust_sample_indices = TRUE,
														 ...) {

	# Validate required parameters
	if (missing(onset_criteria)) {
		stop("onset_criteria is required for rhythm-based windowing")
	}
	if (missing(offset_criteria)) {
		stop("offset_criteria is required for rhythm-based windowing")
	}

	# Verify rhythm-specific requirements
	if (rhythm_type == "sinus" && is.null(reference_criteria)) {
		stop("Sinus rhythm windowing requires reference_criteria (typically QRS complex)")
	}

	# Get signal, header, and annotation data
	sig <- data.table::copy(object$signal)
	hea <- data.table::copy(object$header)
	ann <- data.table::copy(object$annotation)

	# Helper function to filter annotations by criteria
	filter_annotations <- function(ann, criteria) {
		result <- ann
		for (col_name in names(criteria)) {
			if (!col_name %in% colnames(ann)) {
				stop("Column '", col_name, "' not found in annotation table")
			}
			result <- result[result[[col_name]] == criteria[[col_name]], ]
		}
		return(result)
	}

	# Get onset, offset, and reference points
	onset_points <- filter_annotations(ann, onset_criteria)
	offset_points <- filter_annotations(ann, offset_criteria)

	if (nrow(onset_points) == 0) {
		warning("No onset points found with specified criteria")
		return(list())
	}

	if (nrow(offset_points) == 0) {
		warning("No offset points found with specified criteria")
		return(list())
	}

	if (!is.null(reference_criteria)) {
		reference_points <- filter_annotations(ann, reference_criteria)
		if (nrow(reference_points) == 0) {
			warning("No reference points found with specified criteria")
			return(list())
		}
	} else {
		reference_points <- NULL
	}

	# Identify rhythm windows
	windows <- list()
	window_count <- 0

	# Extract sample points
	onset_samples <- onset_points$sample
	offset_samples <- offset_points$sample
	if (!is.null(reference_points)) {
		reference_samples <- reference_points$sample
	}

	for (i in seq_along(onset_samples)) {
		onset <- onset_samples[i]

		# Find the next offset after this onset
		# If there are no further offsets, this will not be a window
		next_offsets <- offset_samples[offset_samples > onset]
		if (length(next_offsets) == 0) {
			break
		}

		offset <- next_offsets[1]

		# Check if there's a reference point between onset and offset (if required)
		if (!is.null(reference_points)) {
			refs_between <- reference_samples[reference_samples > onset &
																					reference_samples < offset]
			if (length(refs_between) == 0) {
				# No reference point between onset and offset, skip this window
				next
			}
			# For sinus rhythm, store the reference point (QRS complex)
			if (rhythm_type == "sinus") {
				qrs <- refs_between[1]
			}
		}

		# Apply rhythm-specific validation
		if (rhythm_type == "sinus") {
			# For sinus, check if there's another onset between this onset and offset
			# (which might indicate overlap)
			onset_between <- onset_samples[onset_samples > onset &
																		 	onset_samples < offset]
			if (length(onset_between) > 0) {
				# For sinus, we typically want clean non-overlapping beats
				next
			}
		}

		# We have a valid window
		# C++ style increment
		window_count <- window_count + 1

		# Create window for this rhythm segment
		window_signal <- sig[sample >= onset & sample <= offset, ]

		# Create header for this window
		info_string <- paste0(rhythm_type, " window ", window_count,
													", onset: ", onset,
													", offset: ", offset)

		if (rhythm_type == "sinus") {
			info_string <- paste0(info_string, ", QRS: ", qrs)
		}

		window_header <- header_table(
			record_name = paste0(attributes(hea)$record_line$record_name, "_",
													 rhythm_type, window_count),
			number_of_channels = attributes(hea)$record_line$number_of_channels,
			frequency = attributes(hea)$record_line$frequency,
			samples = nrow(window_signal),
			ADC_gain = hea$gain,
			label = hea$label,
			info_strings = c(attributes(hea)$info_strings,
											 window_info = info_string)
		)

		# Create annotation for this window
		window_annotation <- ann[sample >= onset & sample <= offset, ]

		# Adjust annotation sample indices to be relative to window start if requested
		if (adjust_sample_indices) {
			window_annotation$sample <- window_annotation$sample - onset + 1
		}

		# Add to list of windows
		windows[[window_count]] <- egm(
			signal = window_signal,
			header = window_header,
			annotation = window_annotation
		)
	}

	if (length(windows) == 0) {
		warning("No complete ", rhythm_type, " windows found with the specified criteria")
		return(list())
	}

	# Return list of windows
	windows
}
