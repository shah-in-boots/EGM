# Class definition for `windowed` objects -------------------------------------

#' Create a `windowed` object containing a list of egm segments
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `windowed` objects are lists of `egm` objects that represent segments or
#' windows of the original signal. This allows for specialized methods to be
#' applied to collections of signal windows. This function primarily serves as
#' the class generation function, and only applies class attributes. It is used
#' by the [window()] function to ensure appropriate class and properties.
#'
#' @param x A list of `egm` objects
#' @param window_method The windowing method used to create the list
#' @param source_record The name of the original record
#' @param ... Additional arguments passed to methods
#'
#' @return An object of class `windowed` which inherits from `list`
#'
#' @export
windowed <- function(x = list(),
										 window_method = "rhythm",
										 source_record = character(),
										 ...) {
	# Validate input
	if (!is.list(x)) {
		stop("x must be a list")
	}

	if (length(x) > 0) {
		# Check that all elements are egm objects
		is_egm_list <- all(sapply(x, inherits, "egm"))
		if (!is_egm_list) {
			stop("All elements of x must be of class 'egm'")
		}
	}

	# Create the windowed object
	structure(
		x,
		class = c("windowed", "list"),
		window_method = window_method,
		source_record = source_record,
		window_count = length(x),
		creation_time = Sys.time()
	)
}

#' Test if an object is a windowed object
#'
#' @param x An object to test
#'
#' @return TRUE if x is a windowed object, FALSE otherwise
#'
#' @export
is_windowed <- function(x) {
	inherits(x, "windowed")
}

#' Format a windowed object for printing
#'
#' @param x A windowed object
#' @param ... Additional arguments passed to methods
#'
#' @return Invisibly returns x
#'
#' @export
format.windowed <- function(x, ...) {
	cat("<windowed: ", length(x), " EGM segments>\n", sep = "")
	cat("Method: ", attr(x, "method"), "\n", sep = "")
	cat("Source: ", attr(x, "source_record"), "\n", sep = "")
	cat("Created: ", format(attr(x, "creation_time")), "\n", sep = "")

	invisible(x)
}

#' Print a windowed object
#'
#' @param x A windowed object
#' @param ... Additional arguments passed to methods
#'
#' @return Invisibly returns x
#'
#' @export
print.windowed <- function(x, ...) {
	format(x, ...)
	invisible(x)
}

#' Subset a windowed object
#'
#' @param x A windowed object
#' @param i Index to subset
#' @param ... Additional arguments passed to methods
#'
#' @return A windowed object with the specified subset of elements
#'
#' @export
`[.windowed` <- function(x, i, ...) {
	# Get original attributes
	attrs <- attributes(x)
	class_val <- attrs$class
	method_val <- attrs$method
	source_record_val <- attrs$source_record
	creation_time_val <- attrs$creation_time

	# Subset the list
	result <- NextMethod()

	# Restore the windowed class and update attributes
	structure(
		result,
		class = class_val,
		method = method_val,
		source_record = source_record_val,
		window_count = length(result),
		creation_time = creation_time_val
	)
}

#' Concatenate windowed objects
#'
#' @param ... windowed objects to concatenate
#'
#' @return A windowed object containing all the elements of the input objects
#'
#' @export
c.windowed <- function(...) {
	args <- list(...)

	# Check that all arguments are windowed objects
	if (!all(sapply(args, is_windowed))) {
		stop("All arguments must be `windowed` objects")
	}

	# TODO
	# Consider additional validation methods for making sure windows are similar

	# Get the first non-empty object's attributes
	first_non_empty <- which(sapply(args, length) > 0)[1]
	if (is.na(first_non_empty)) {
		first_non_empty <- 1  # All objects are empty, use first object's attributes
	}

	method_val <- attr(args[[first_non_empty]], "method")
	source_record_val <- attr(args[[first_non_empty]], "source_record")

	# Concatenate the lists
	result <- do.call(c, lapply(args, unclass))

	# Create the windowed object
	structure(
		result,
		class = c("windowed", "list"),
		method = method_val,
		source_record = source_record_val,
		window_count = length(result),
		creation_time = Sys.time()
	)
}

#' Apply a function to each element of a windowed object
#'
#' @param X A windowed object
#' @param FUN A function to apply to each element
#' @param ... Additional arguments passed to FUN
#'
#' @return A list of the results of applying FUN to each element of X,
#'   or a new windowed object if all results are egm objects
#'
#' @export
lapply.windowed <- function(X, FUN, ...) {
	# Apply the function to each element
	results <- NextMethod()

	# Check if results are all egm objects
	if (all(sapply(results, inherits, "egm"))) {
		# Return a new windowed object
		return(windowed(
			results,
			method = attr(X, "method"),
			source_record = attr(X, "source_record")
		))
	} else {
		# Return the results as a regular list
		return(results)
	}
}

# Windowing function(s) --------------------------------------------------------

#' Window signal data based on different methods
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
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
#' @param window_method A `character` string specifying the windowing method.
#'   Options include:
#'
#'   - **rhythm** - Windows based on rhythm patterns (requires rhythm_type and criteria)
#'
#' @param rhythm_type A `character` string specifying the rhythm type (e.g.,
#'   "sinus"). Currently supported: "sinus" (requires reference check).
#'
#' @param onset_criteria A named list of criteria to identify onset points.
#'   Names should match column names in the annotation table.
#'
#' @param offset_criteria A named list of criteria to identify offset points.
#'   Names should match column names in the annotation table.
#'
#' @param reference_criteria A named list of criteria to identify reference
#'   points that must exist between onset and offset. Set to NULL to skip
#'   reference validation.
#'
#' @param adjust_sample_indices Logical, whether to adjust annotation sample
#'   indices in the returned windows to be relative to the window start. Default
#'   is TRUE.
#'
#' @param ... Additional arguments passed to specific windowing methods.
#'
#' @return A list of `egm` objects, each representing a window of the original
#'   signal.
#'
#' @export
window <- function(object, window_method = c("rhythm"), ...) {

	# Validate input
	stopifnot("Requires object of <egm> class for evaluation" = inherits(object, 'egm'))

	# Match the method argument
	window_method <- match.arg(window_method)

	# Dispatch to the appropriate method handler
	windows <-
		# Can add specific methods here in the future
		switch(window_method,
					 rhythm = window_by_rhythm(object, ...),
					 stop("Unsupported windowing method: ", window_method))

	# Extract source record name
	source_record <-
		if (!is.null(object$header$record_name)) {
			object$header$record_name
		} else if (!is.null(attributes(object$header)$record_line$record_name)) {
			attributes(object$header)$record_line$record_name
		} else {
			"unknown"
		}

	# Return as `windowed` object
	# This is an internal class to allow for lists of `egm` objects
	windowed(windows, window_method = window_method, source_record = source_record)
}

#' @rdname window
#' @export
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
		info_string <- paste0(rhythm_type,
													" window ",
													window_count,
													", onset: ",
													onset,
													", offset: ",
													offset)

		if (rhythm_type == "sinus") {
			info_string <- paste0(info_string, ", QRS: ", qrs)
		}

		window_header <- header_table(
			record_name = paste0(
				attributes(hea)$record_line$record_name,
				"_",
				rhythm_type,
				window_count
			),
			number_of_channels = attributes(hea)$record_line$number_of_channels,
			frequency = attributes(hea)$record_line$frequency,
			samples = nrow(window_signal),
			ADC_gain = hea$gain,
			label = hea$label,
			info_strings = c(attributes(hea)$info_strings, window_info = info_string)
		)

		# Create annotation for this window
		window_annotation <- ann[sample >= onset & sample <= offset, ]

		# Adjust annotation sample indices to be relative to window start if requested
		if (adjust_sample_indices) {
			window_annotation$sample <- window_annotation$sample - onset + 1
		}

		# Add to list of windows
		windows[[window_count]] <- egm(signal = window_signal,
																	 header = window_header,
																	 annotation = window_annotation)
	}

	if (length(windows) == 0) {
		warning("No complete ",
						rhythm_type,
						" windows found with the specified criteria")
		return(list())
	}

	# Return list of windows
	windows
}

# Standardization and normalization of windows ---------------------------------

#' Standardize windows of signal data
#'
#' @description Standardizes `windowed` objects by applying various
#' transformations to each window. This function converts each `egm` object in a
#' `windowed` list to a standardized data frame with uniform properties,
#' facilitating comparison and analysis.
#'
#' @details Currently supported standardization methods:
#'
#' * `time_normalize` - Resamples each window to a standard length by either
#' dilating or contracting the signal. The result is a signal with a consistent
#' number of samples regardless of the original window duration.
#'
#' Additional options:
#'
#' * `align_feature` - If provided, windows will be aligned to center around this
#' feature (e.g., a specific annotation type like "N" for R-peak). Can be a
#' character string matching an annotation type or a list of criteria for
#' annotation matching.
#'
#' * `preserve_amplitude` - If TRUE (default), maintains the original amplitude
#' range after resampling. If FALSE, the amplitudes may change due to
#' interpolation.
#'
#' @param x A `windowed` object to standardize
#' @param standardization_method A `character` string specifying the standardization method.
#'   Currently supported: "time_normalize".
#' @param target_samples The desired number of samples for each standardized
#'   window. Default is 500 samples. This parameter takes precedence if both
#'   target_samples and target_ms are provided.
#' @param target_ms Alternative specification in milliseconds. If provided and
#'   target_samples is NULL, the function will convert this to samples based on
#'   the signal's sampling frequency.
#' @param interpolation_method The method used for interpolation when
#'   resampling. Options are "linear" (default), "spline", or "step".
#' @param align_feature Feature to align windows around, either a character
#'   string matching an annotation type or a list of criteria for finding a
#'   specific annotation. Default is NULL (no alignment).
#' @param preserve_amplitude Logical. If TRUE (default), maintains original
#'   amplitude range after resampling.
#' @param preserve_class Logical. If TRUE, returns a `windowed` object with
#'   standardized data frames. If FALSE (default), returns a plain list of data
#'   frames.
#' @param ... Additional arguments passed to specific standardization methods.
#'
#' @return If `preserve_class=TRUE`, a `windowed` object containing standardized
#'   data frames. If `preserve_class=FALSE`, a plain list of standardized data
#'   frames.
#'
#' @examples
#' \dontrun{
#' # Read in ECG data
#' ecg <- read_wfdb("ecg", test_path(), "ecgpuwave")
#'
#' # Create windows based on sinus rhythm
#' windows <- window_signal(
#'   ecg,
#'   method = "rhythm",
#'   rhythm_type = "sinus",
#'   onset_criteria = list(type = "(", number = 0),
#'   offset_criteria = list(type = ")", number = 2),
#'   reference_criteria = list(type = "N")
#' )
#'
#' # Standardize windows to exactly 500 samples
#' std_windows <- standardize_windows(
#'   windows,
#'   method = "time_normalize",
#'   target_samples = 500
#' )
#'
#' # Alternatively, standardize to 500 milliseconds (depends on sampling frequency)
#' std_windows_ms <- standardize_windows(
#'   windows,
#'   method = "time_normalize",
#'   target_ms = 500
#' )
#'
#' # Standardize windows with QRS alignment
#' aligned_windows <- standardize_windows(
#'   windows,
#'   method = "time_normalize",
#'   target_samples = 500,
#'   align_feature = "N"  # Align on QRS complexes
#' )
#' }
#'
#' @export
standardize_windows <- function(x,
																standardization_method = c("time_normalize"),
																target_samples = 500,
																target_ms = NULL,
																interpolation_method = c("linear", "spline", "step"),
																align_feature = NULL,
																preserve_amplitude = TRUE,
																preserve_class = FALSE,
																...) {
	# Validate input
	if (!is_windowed(x)) {
		stop("Input must be a windowed object")
	}

	# Match the method argument
	# Allows for multiple matches if we want to expand this in future
	standardization_method <- match.arg(standardization_method)
	interpolation_method <- match.arg(interpolation_method)

	# Dispatch to the appropriate standardization method
	# Keep the dots to pass additional features in the future
	standardized <- switch(
		standardization_method,
		time_normalize = time_normalize_windows(
			x,
			target_samples = target_samples,
			target_ms = target_ms,
			interpolation_method = interpolation_method,
			align_feature = align_feature,
			preserve_amplitude = preserve_amplitude,
			...
		),
		# Add additional methods here in the future
		stop("Unsupported standardization method: ", standardization_method)
	)

	# Return as appropriate class
	if (preserve_class) {
		return(windowed(
			standardized,
			window_method = paste0("standardized_", standardization_method),
			source_record = attr(x, "source_record")
		))
	} else {
		return(standardized)
	}
}

#' Time normalize windows to a standard length
#' @keywords internal
time_normalize_windows <- function(x,
																	 target_samples = 500,
																	 target_ms = NULL,
																	 interpolation_method = "linear",
																	 align_feature = NULL,
																	 preserve_amplitude = TRUE,
																	 ...) {
	if (length(x) == 0) {
		return(list())
	}

	# Get the sampling frequency from the first window
	first_window <- x[[1]]
	if (!inherits(first_window, "egm")) {
		stop("Windows must be egm objects")
	}

	frequency <- attributes(first_window$header)$record_line$frequency

	# Determine target samples - either directly specified or converted from ms
	if (is.null(target_samples) && !is.null(target_ms)) {
		target_samples <- ceiling((target_ms / 1000) * frequency)
	} else if (is.null(target_samples)) {
		target_samples <- 500  # Default fallback
	}

	# Process each window
	standardized <- lapply(x, function(window) {
		# Extract the signal data
		signal_data <- window$signal

		# Find the sample column index
		sample_col_idx <- which(names(signal_data) == "sample")
		signal_cols <- setdiff(1:ncol(signal_data), sample_col_idx)

		# Create a data frame to store the resampled data
		resampled_data <- data.frame(sample = 1:target_samples)

		# Feature alignment (if requested)
		if (!is.null(align_feature) && !is.null(window$annotation)) {
			# Find the feature in the annotations
			feature_idx <- NULL

			if (is.list(align_feature)) {
				# Filter annotations by criteria
				filtered_ann <- window$annotation
				for (col_name in names(align_feature)) {
					if (col_name %in% names(filtered_ann)) {
						filtered_ann <- filtered_ann[filtered_ann[[col_name]] == align_feature[[col_name]], ]
					}
				}

				if (nrow(filtered_ann) > 0) {
					feature_idx <- filtered_ann$sample[1]
				}
			} else if (is.character(align_feature)) {
				# Check for a specific annotation type
				if ("type" %in% names(window$annotation)) {
					type_match <- window$annotation[window$annotation$type == align_feature, ]
					if (nrow(type_match) > 0) {
						feature_idx <- type_match$sample[1]
					}
				}
			}

			if (!is.null(feature_idx)) {
				# Normalize around the feature - center it in the window
				center_point <- ceiling(target_samples / 2)
				original_samples <- nrow(signal_data)

				# Calculate shift needed to center the feature
				shift_samples <- center_point - feature_idx

				# Create new sample indices that center the feature
				original_indices <- 1:original_samples
				shifted_indices <- original_indices + shift_samples

				# Create new sample sequence that's centered on the feature
				new_samples <- seq(shifted_indices[1], shifted_indices[length(shifted_indices)], length.out = target_samples)

				# Proceed with interpolation
				for (col in signal_cols) {
					col_name <- names(signal_data)[col]
					original_values <- signal_data[[col]]

					# Use the specified interpolation method
					if (interpolation_method == "linear") {
						resampled_values <- stats::approx(
							x = original_indices,
							y = original_values,
							xout = new_samples,
							method = "linear",
							rule = 2
						)$y
					} else if (interpolation_method == "spline") {
						resampled_values <- stats::spline(
							x = original_indices,
							y = original_values,
							xout = new_samples,
							method = "natural"
						)$y
					} else if (interpolation_method == "step") {
						resampled_values <- stats::approx(
							x = original_indices,
							y = original_values,
							xout = new_samples,
							method = "constant",
							rule = 2
						)$y
					}

					# Add to output
					resampled_data[[col_name]] <- resampled_values
				}
			} else {
				# Feature not found, fall back to regular resampling
				warning(
					"Specified alignment feature not found in annotations, using standard resampling"
				)
				original_samples <- nrow(signal_data)
				new_samples <- seq(1, original_samples, length.out = target_samples)

				for (col in signal_cols) {
					col_name <- names(signal_data)[col]
					original_values <- signal_data[[col]]

					# Apply interpolation method
					resampled_values <- interpolate_signal(original_samples,
																								 original_values,
																								 new_samples,
																								 interpolation_method)

					# Add the resampled column to the output data frame
					resampled_data[[col_name]] <- resampled_values
				}
			}
		} else {
			# No feature alignment, standard resampling
			original_samples <- nrow(signal_data)
			new_samples <- seq(1, original_samples, length.out = target_samples)

			for (col in signal_cols) {
				col_name <- names(signal_data)[col]
				original_values <- signal_data[[col]]

				# Apply interpolation method
				resampled_values <- interpolate_signal(original_samples,
																						 original_values,
																						 new_samples,
																						 interpolation_method)

				# Add the resampled column to the output data frame
				resampled_data[[col_name]] <- resampled_values
			}
		}

		# Preserve amplitude scale if requested
		if (preserve_amplitude) {
			for (col in signal_cols) {
				col_name <- names(signal_data)[col]
				original_range <- range(signal_data[[col]], na.rm = TRUE)
				resampled_range <- range(resampled_data[[col_name]], na.rm = TRUE)

				# Rescale to match original amplitude range
				if (diff(resampled_range) != 0) {
					# Avoid division by zero
					resampled_data[[col_name]] <-
						((resampled_data[[col_name]] - resampled_range[1]) / diff(resampled_range)) * diff(original_range) +  original_range[1]
				}
			}
		}

		# Return the resampled data frame
		resampled_data
	})

	# Return the list of standardized windows
	standardized
}

#' Helper function to apply interpolation
#' @keywords internal
interpolate_signal <- function(original_samples,
															 original_values,
															 new_samples,
															 interpolation_method) {
	original_indices <- 1:original_samples

	if (interpolation_method == "linear") {
		return(
			stats::approx(
				x = original_indices,
				y = original_values,
				xout = new_samples,
				method = "linear",
				rule = 2
			)$y
		)
	} else if (interpolation_method == "spline") {
		return(
			stats::spline(
				x = original_indices,
				y = original_values,
				xout = new_samples,
				method = "natural"
			)$y
		)
	} else if (interpolation_method == "step") {
		return(
			stats::approx(
				x = original_indices,
				y = original_values,
				xout = new_samples,
				method = "constant",
				rule = 2
			)$y
		)
	}
}
