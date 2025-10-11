#' Extract F wave features from ECG
#'
#' This function analyzes F waves in an ECG signal, extracting various
#' characteristics.
#'
#' @param object An object of class `egm` or of subclass `ecg`
#'
#' @param lead Optional. A character string specifying the lead to analyze. If
#'   NULL (default), all available surface leads will be processed.
#'
#' @param qrs_method Method for ventricular signal removal. Default is
#'   "adaptive_svd" for adaptive singular value decomposition.
#'
#' @param f_characteristics Vector of characteristics to analyze from ECG
#'   signal. Options: "amplitude", "approximate_entropy", "dominant_frequency".
#'   Please see [calculate_approximate_entropy()] and
#'   [calculate_dominant_frequency()] for more details.
#'
#' @param verbose Logical. If TRUE, print information about which leads will be
#'   analyzed. Default is TRUE.
#'
#' @param .force_all Logical. If FALSE (default), only process surface ECG leads.
#'   If TRUE, process all available leads. This parameter is ignored if the object
#'   is of class 'ecg', in which case all leads are processed.
#'
#' @param ... Additional arguments passed to methods
#'
#' @references
#'
#' Park, Junbeom, Chungkeun Lee, Eran Leshem, Ira Blau, Sungsoo Kim, Jung Myung
#' Lee, Jung-A Hwang, Byung-il Choi, Moon-Hyoung Lee, and Hye Jin Hwang. "Early
#' Differentiation of Long-Standing Persistent Atrial Fibrillation Using the
#' Characteristics of Fibrillatory Waves in Surface ECG Multi-Leads." Scientific
#' Reports 9 (February 26, 2019): 2746.
#' https://doi.org/10.1038/s41598-019-38928-6.
#'
#' Hyvarinen, A., and Oja, E. (2000). Independent component analysis: algorithms
#' and applications. *Neural Networks*, 13(4-5), 411-430.
#'
#' @return A list containing F wave features for each processed lead
#' @export
extract_f_waves <- function(object,
														lead = NULL,
														qrs_method = "adaptive_svd",
														f_characteristics = "amplitude",
														verbose = TRUE,
														.force_all = FALSE,
														...) {
	# Validate input
	if (!inherits(object, "egm")) {
		stop("Input must be of class 'egm'")
	}

	# Validate atrial / F wave characteristics
	valid_characteristics <- c(
		"amplitude",
		"approximate_entropy",
		"dominant_frequency"
	)
	if (!all(f_characteristics %in% valid_characteristics)) {
		stop(
			"Invalid characteristic specified. Choose from: ",
			paste(valid_characteristics, collapse = ", ")
		)
	}

	# Get available leads (assuming first column is 'sample')
	available_leads <- names(object$signal)[-1]

	# If lead is specified, validate and use it
	if (!is.null(lead)) {
		if (!lead %in% available_leads) {
			stop("Specified lead not found in the signal data")
		}
		leads_to_process <- lead
		if (verbose) {
			message("Analyzing specified lead: ", lead)
		}
	} else {
		# Check if object is ecg class (which would already only have surface leads)
		is_ecg_object <- inherits(object, "ecg")

		# If it's an ECG object or .force_all is TRUE, process all available leads
		if (is_ecg_object || .force_all) {
			leads_to_process <- available_leads

			if (verbose) {
				if (is_ecg_object) {
					message("Object is of class 'ecg'. Analyzing all ", length(leads_to_process), " leads.")
				} else {
					message("Analyzing all ", length(leads_to_process), " available leads.")
				}
			}
		} else {
			# Standard ECG lead names (case-insensitive matching)
			std_leads <- .leads$ECG

			# Normalize lead names for comparison
			normalized_available <- toupper(gsub("[_\\s-]", "", available_leads))
			normalized_std <- toupper(gsub("[_\\s-]", "", std_leads))

			# Find which available leads match standard ECG leads
			surface_indices <- which(normalized_available %in% normalized_std)
			surface_leads <- available_leads[surface_indices]
			non_surface_leads <- available_leads[-surface_indices]

			if (length(surface_leads) == 0) {
				warning("No surface leads found in the signal data")
				return(list())
			}

			leads_to_process <- surface_leads

			if (verbose) {
				message("Analyzing ", length(leads_to_process), " surface leads: ",
								paste(leads_to_process, collapse = ", "))

				if (length(non_surface_leads) > 0) {
					message("Skipping ", length(non_surface_leads), " non-surface leads: ",
									paste(non_surface_leads, collapse = ", "))
				}
			}
		}
	}

	# Process each lead
	results <- lapply(leads_to_process, function(l) {
		process_single_lead(object, l, qrs_method, f_characteristics)
	})

	# Name the results
	names(results) <- leads_to_process
	results
}

# Signal cleaning ----

#' Helper function to process a single lead
#' @noRd
process_single_lead <- function(object, lead, qrs_method, f_characteristics) {
  # Extract signal for the specified lead
  signal <- object$signal[[lead]]
  hz <- attributes(object$header)$record_line$frequency

  # Preprocess signal and apply band pass filter
  upsampled_signal <-
    upsample_signal(signal, original_frequency = hz, new_frequency = 1000) |>
    filter_bandpass(signal = _, frequency = 1000)

  # Ventricular signal removal (QRST cancellation)
  atrial_activity <- remove_ventricular_signal(upsampled_signal, method = qrs_method)

  # Atrial signal analysis and feature extraction
  features <- analyze_atrial_signal(
    atrial_activity,
    frequency = 1000,
    characteristics = f_characteristics
  )

  # Returns a list of features
  features
}

#' Upsampling signal approach
#' @noRd
upsample_signal <- function(signal, original_frequency, new_frequency) {
  # Increase sampling rate if necessary (e.g., from 500 Hz to 1000 Hz)
  if (original_frequency < new_frequency) {
    original_length <- length(signal)
    t <- seq(0, (original_length - 1) / original_frequency, length.out = original_length)
    t_new <- seq(0, (original_length - 1) / original_frequency, length.out = original_length * (new_frequency / original_frequency))
    upsampled <- stats::approx(t, signal, xout = t_new, method = "linear")$y
    frequency <- new_frequency
  }

  # Return new upsampled signal
  upsampled
}

#' Apply bandpass filter (0.5-30 Hz is the default)
#' @noRd
filter_bandpass <- function(signal, frequency, low = 0.5, high = 30) {
  nyquist_freq <- frequency / 2
  low <- low / nyquist_freq
  high <- high / nyquist_freq
  bf <- signal::butter(3, c(low, high), type = "pass")
  signal::filtfilt(bf, signal)
}

# QRS Methods ----

#' Ventricular signal removal
#' Selection method for how to remove ventricular signal
#' @noRd
remove_ventricular_signal <- function(signal, method = "adaptive_svd") {
  if (method == "adaptive_svd") {
    return(remove_qrs_with_adaptive_svd(signal))
  } else if (method == "ica") {
    return(remove_qrs_with_ica(signal))
  } else {
    stop("Unsupported method. Choose 'adaptive_svd' or 'ica'")
  }
}

#' Helper function to perform adaptive SVD cancellation
#' Add protections to stop matrix conformation issues, ASS @2025-02-18
#' Added adjustment for aberrancy, ASS @2025-03-28
#' @noRd
remove_qrs_with_adaptive_svd <- function(signal,
																				 frequency = 1000,
																				 qrs_loc = NULL,
																				 adaptive_window = TRUE,
																				 aberrant_beats = TRUE,
																				 smoothing = TRUE) {


  # Detect QRS complexes if not provided
  if (is.null(qrs_loc)) {
    qrs_loc <- detect_QRS(signal, frequency)
  }

  # Handle edge case of insufficient QRS complexes
  if (length(qrs_loc) < 2) {
    warning("Insufficient QRS complexes detected for SVD. Returning original signal.")
    return(signal)
  }

  # Calculate RR intervals for adaptive windowing and abnormal beat detection
	rr_intervals <- diff(qrs_loc)
  median_rr <- median(rr_intervals)

  # Have to consider aberrancy as well
  # Identify potential abnormal beats (PVCs, etc.) based on RR intervals
  # PVCs typically have a shorter preceding RR and longer following RR
  # If interpolated, may still lead to abnormally short RR intervals
  # Will change the QRS windowing
  is_aberrant <- c(FALSE, abs(rr_intervals - median_rr) > (0.4 * median_rr))

  # Determine window size based on heart rate
  if (adaptive_window) {
    # Use heart rate to determine window size (faster rate = smaller window)
    base_window_ms <- min(500, max(250, 60000 / (median_rr / frequency * 1000) * 0.2))
    base_window <- round(base_window_ms * frequency / 1000)
  } else {
    # Fixed 500ms window
    base_window <- round(0.5 * frequency)
  }

  half_window <- floor(base_window / 2)

  # Process beats (either separately or together based on morphology)
  atrial_signal <- signal

	# Helper function to process a group of similar beats
  # This is an internal function because it may not work outside
  # Returns a list of segments and its attributes
	process_beat_group <- function(signal,
																 beat_indices,
																 half_window,
																 frequency,
																 smoothing) {

	  # Extract beat segments
	  segments <- lapply(beat_indices, function(idx) {
	    start_idx <- max(1, idx - half_window)
	    end_idx <- min(length(signal), idx + half_window)
	    segment <- signal[start_idx:end_idx]

	    # Ensure uniform size for matrix operations
	    max_length <- 2 * half_window + 1
	    if (length(segment) < max_length) {
	      segment <- c(segment, rep(0, max_length - length(segment)))
	    } else if (length(segment) > max_length) {
	      segment <- segment[1:max_length]
	    }

	    # Return the list of relevant segment attributes
	    list(
	      segment = segment,
	      start = start_idx,
	      end = end_idx,
	      length = end_idx - start_idx + 1
	    )
	  })

	  # Create segment matrix for SVD
	  segment_matrix <- do.call(rbind, lapply(segments, function(x) x$segment))

	  # Apply SVD
	  svd_result <- tryCatch({
	    svd(segment_matrix)
	  }, error = function(x) {
	    warning("SVD failed: ", x$message, ". Using original signal for this segment group.")
	    return(NULL)
	  })

	  if (is.null(svd_result)) {
	    return(signal)
	  }

	  # Determine number of components to keep (95% variance explained)
	  variance <- svd_result$d^2 / sum(svd_result$d^2)
	  cum_variance <- cumsum(variance)
	  n_components <- min(which(cum_variance >= 0.95))

	  # Reconstruct template matrix
	  template_matrix <- svd_result$u[, 1:n_components, drop = FALSE] %*%
	    diag(svd_result$d[1:n_components], nrow=n_components) %*%
	    t(svd_result$v[, 1:n_components, drop = FALSE])

	  # Subtract templates and apply smoothing
	  result_signal <- signal

	  # Transition zone for smoothing (in samples)
	  transition_length <- round(0.03 * frequency)  # 30ms transition

	  for (i in seq_along(segments)) {
	    seg <- segments[[i]]
	    start_idx <- seg$start
	    end_idx <- seg$end
	    segment_length <- seg$length

	    # Get template for this beat
	    if (segment_length <= ncol(template_matrix)) {
	      template <- template_matrix[i, 1:segment_length]
	    } else {
	      # Edge case handling
	      template <- c(template_matrix[i, ], rep(0, segment_length - ncol(template_matrix)))
	    }

	    if (smoothing) {
	      # Store original values for transition regions
	      pre_region <- max(1, start_idx - transition_length):(start_idx - 1)
	      post_region <- (end_idx + 1):min(length(signal), end_idx + transition_length)

	      pre_values <- result_signal[pre_region]
	      post_values <- result_signal[post_region]
	    }

	    # Subtract template
	    result_signal[start_idx:end_idx] <- result_signal[start_idx:end_idx] - template

	    # Apply smooth transitions to avoid discontinuities
	    # Ensure appropriate length of vectors
	    # Apply smooth transitions to avoid discontinuities
	    if (smoothing) {
	    	# Pre-region (before QRS)
	    	pre_start <- max(1, start_idx - transition_length)
	    	pre_end <- max(1, start_idx - 1)  # Ensure pre_end is at least 1

	    	# Only process if we have a valid range with at least 1 point
	    	if (pre_start < pre_end) {
	    		pre_region <- pre_start:pre_end
	    		pre_length <- length(pre_region)

	    		pre_values <- result_signal[pre_region]
	    		pre_weights <- seq(0, 1, length.out = pre_length)

	    		# Vector operation is cleaner than a loop when lengths match
	    		result_signal[pre_region] <- (1 - pre_weights) * pre_values +
	    			pre_weights * result_signal[pre_region]
	    	}

	    	# Post-region (after QRS)
	    	post_start <- min(end_idx + 1, length(signal))
	    	post_end <- min(length(signal), end_idx + transition_length)

	    	# Only process if we have a valid range with at least 1 point
	    	if (post_start < post_end) {
	    		post_region <- post_start:post_end
	    		post_length <- length(post_region)

	    		post_values <- result_signal[post_region]
	    		post_weights <- seq(1, 0, length.out = post_length)

	    		result_signal[post_region] <-
	    			post_weights * result_signal[post_region] +
	    			(1 - post_weights) * post_values
	    	}
	    }
	    }

	  # Return smoothed signal for that beat group
	  result_signal
	}

  # Handle normal and abnormal beats separately if requested
  if (aberrant_beats) {
    # Process normal beats
    normal_indices <- which(!is_aberrant)
    if (length(normal_indices) > 1) {
    	atrial_signal <- process_beat_group(atrial_signal, qrs_loc[normal_indices], half_window, frequency, smoothing)
    }

    # Process abnormal beats with wider window (PVCs often have wider QRS)
    abnormal_indices <- which(is_aberrant)
    if (length(abnormal_indices) > 1) {
      # Use 50% wider window for abnormal beats
      abnormal_half_window <- round(half_window * 1.5)
      atrial_signal <- process_beat_group(
        atrial_signal, qrs_loc[abnormal_indices], abnormal_half_window, frequency, smoothing)
    } else if (length(abnormal_indices) == 1) {
      # Special handling for single abnormal beat
      idx <- qrs_loc[abnormal_indices]
      abnormal_half_window <- round(half_window * 1.5)
      start_idx <- max(1, idx - abnormal_half_window)
      end_idx <- min(length(signal), idx + abnormal_half_window)

      # Simple linear interpolation for single abnormal beat
      atrial_signal[start_idx:end_idx] <- approx(
        c(start_idx-1, end_idx+1),
        c(atrial_signal[max(1, start_idx-1)], atrial_signal[min(length(signal), end_idx+1)]),
        start_idx:end_idx
      )$y
    }
  } else {
    # Process all beats together
    atrial_signal <- process_beat_group(
      atrial_signal, qrs_loc, half_window, frequency, smoothing)
  }

  # Apply final smoothing to reduce any remaining artifacts
  smooth_window <- round(0.015 * frequency)  # 15ms window
  if (smooth_window > 2) {
    # Use Savitzky-Golay filter if signal package is available
    if (requireNamespace("signal", quietly = TRUE)) {
      if (smooth_window %% 2 == 0) smooth_window <- smooth_window + 1  # Ensure odd window size
      kern <- signal::sgolay(p = 3, n = smooth_window, m = 0)
      smoothed <- signal::filter(kern, atrial_signal)
    } else {
      # Otherwise use simple moving average
      smoothed <- stats::filter(atrial_signal, rep(1/smooth_window, smooth_window), sides = 2)
    }

    # Replace NA values from filtering with original signal
    na_indices <- which(is.na(smoothed))
    if (length(na_indices) > 0) {
      smoothed[na_indices] <- atrial_signal[na_indices]
    }

    atrial_signal <- smoothed
  }

  # Final, smoothed atrial signal
  atrial_signal
}

#' Remove ventricular activity (QRS complexes) using Independent Component Analysis (ICA)
#'
#' @description
#' This function implements ICA-based QRS removal for extracting atrial activity
#' from ECG signals, particularly useful in atrial fibrillation analysis.
#'
#' @details
#' Independent Component Analysis (ICA) separates a multivariate signal into
#' statistically independent components. In ECG signals, this can help isolate
#' atrial activity from ventricular activity (QRS complexes).
#'
#' @param signal Numeric vector of the ECG signal
#' @param frequency Sampling frequency in Hz (default 1000)
#' @param qrs_loc Optional vector of QRS complex locations (sample indices)
#' @param num_lags Number of delayed signals to use (default 7)
#' @param lag_ms Maximum lag in milliseconds (default 200)
#'
#' @return A numeric vector containing the atrial activity signal
#'
#' @noRd
ica_removal <- function(signal,
												frequency = 1000,
												qrs_loc = NULL,
												num_lags = 7,
												lag_ms = 200) {

	# Check for fastICA package
	if (!requireNamespace("fastICA", quietly = TRUE)) {
		stop("The fastICA package is required for ICA removal.")
	}

	# Detect QRS complexes if not provided
	if (is.null(qrs_loc)) {
		qrs_loc <- detect_QRS(signal, frequency)
	}

	# Handle the case of no QRS complexes detected
	if (length(qrs_loc) < 3) {
		warning("Too few QRS complexes detected for ICA. Returning original signal.")
		return(signal)
	}

	# Create a multivariate signal using delayed versions
	signal_length <- length(signal)

	# Calculate lag samples from lag_ms
	max_lag_samples <- round(lag_ms * frequency / 1000)

	# Create evenly spaced lags from 0 to max_lag_samples
	lags <- round(seq(0, max_lag_samples, length.out = num_lags))

	# Create the multivariate signal matrix
	X <- matrix(0, nrow = signal_length - max(lags), ncol = length(lags))

	for (i in seq_along(lags)) {
		X[, i] <- signal[(max(lags) - lags[i] + 1):(signal_length - lags[i])]
	}

	# Center the data (important for ICA)
	X_mean <- colMeans(X)
	X_centered <- scale(X, center = TRUE, scale = FALSE)

	# Apply FastICA
	n_components <- min(ncol(X), 5)  # Use at most 5 components

	# Try FastICA with different parameters if needed
	ica_result <- NULL
	for (alg in c("parallel", "deflation")) {
		ica_attempt <- try(fastICA::fastICA(X_centered, n.comp = n_components,
																				alg.typ = alg, fun = "logcosh",
																				alpha = 1, method = "R",
																				maxit = 200, tol = 0.0001, verbose = FALSE),
											 silent = TRUE)

		if (!inherits(ica_attempt, "try-error")) {
			ica_result <- ica_attempt
			break
		}
	}

	if (is.null(ica_result)) {
		warning("ICA algorithm failed to converge. Returning original signal.")
		return(signal)
	}

	# Extract the components from the ICA result
	# In FastICA:
	# X = IC * K, where:
	# - X is the centered data
	# - IC (S) are the independent components
	# - K is the unmixing matrix

	S <- ica_result$S # Independent components (n samples * q components)
	K <- ica_result$K # Projection/Unmixing matrix (q components * p variables)
	A <- ica_result$A # Mixing matrix (p variables * q components)
	W <- ica_result$W # Estimated unmixing matrix (q components * p variables)

	# Create QRS reference signal based on known QRS locations
	qrs_sparse <- numeric(nrow(X))
	valid_qrs <- qrs_loc[qrs_loc >= max(lags) & qrs_loc <= signal_length]
	valid_indices <- valid_qrs - max(lags) + 1
	valid_indices <- valid_indices[valid_indices > 0 & valid_indices <= length(qrs_sparse)]

	if (length(valid_indices) > 0) {
		qrs_sparse[valid_indices] <- 1

		# Smooth QRS reference signal
		qrs_width <- round(0.1 * frequency)  # Typical QRS width in samples
		if (qrs_width > 2) {
			qrs_kernel <- exp(-((-(qrs_width):qrs_width)^2) / (2 * (qrs_width/3)^2))
			qrs_kernel <- qrs_kernel / sum(qrs_kernel)
			qrs_smooth <- stats::filter(qrs_sparse, qrs_kernel, sides = 2)
			qrs_smooth[is.na(qrs_smooth)] <- 0
		} else {
			qrs_smooth <- qrs_sparse
		}
	} else {
		qrs_smooth <- numeric(nrow(X))
	}

	# Identify ventricular components using correlations with QRS reference
	component_qrs_corr <- apply(S, 2, function(x) {
		abs(cor(x^2, qrs_smooth))  # Square to focus on high amplitude segments
	})

	# Find components with high correlation to QRS
	threshold <- max(0.3, max(component_qrs_corr) * 0.7)
	ventricular_idx <- which(component_qrs_corr >= threshold)

	# If no components exceeded threshold, take the highest correlated one
	if (length(ventricular_idx) == 0) {
		ventricular_idx <- which.max(component_qrs_corr)
	}

	# Create filtered version of S with ventricular components zeroed out
	S_filtered <- S
	S_filtered[, ventricular_idx] <- 0

	# Reconstruct the signal WITHOUT ventricular components
	# Using FastICA model: X = S %*% t(W)
	X_filtered <- S_filtered %*% t(W)

	# Add back the mean
	for (i in 1:ncol(X_filtered)) {
		X_filtered[, i] <- X_filtered[, i] + X_mean[i]
	}

	# Use the first column as our atrial signal
	atrial_signal <- X_filtered[, 1]

	# Ensure the output has the same length as the original signal
	atrial_full <- numeric(signal_length)
	start_idx <- max(lags) + 1
	end_idx <- min(signal_length, start_idx + length(atrial_signal) - 1)
	segment_length <- end_idx - start_idx + 1

	# Place the processed segment in the full signal
	atrial_full[start_idx:end_idx] <- atrial_signal[1:segment_length]

	# Fill in the start of the signal (which we couldn't process due to lag)
	if (start_idx > 1) {
		# Linear fade from original to processed signal
		fade_length <- min(round(0.1 * frequency), start_idx - 1)
		fade_start <- max(1, start_idx - fade_length)
		fade_weights <- seq(0, 1, length.out = start_idx - fade_start + 1)

		# Mix original and processed at the boundary
		for (i in fade_start:(start_idx-1)) {
			weight_idx <- i - fade_start + 1
			# Use nearest processed sample as reference
			atrial_full[i] <- (1 - fade_weights[weight_idx]) * signal[i] +
				fade_weights[weight_idx] * atrial_full[start_idx]
		}

		# Direct copy for very beginning
		if (fade_start > 1) {
			atrial_full[1:(fade_start-1)] <- signal[1:(fade_start-1)]
		}
	}

	# Fill in the end of the signal if needed
	if (end_idx < signal_length) {
		atrial_full[(end_idx+1):signal_length] <- signal[(end_idx+1):signal_length]
	}

	# Apply final smoothing to reduce artifacts
	smooth_window <- round(0.01 * frequency)  # 10ms window
	if (smooth_window > 2) {
		if (requireNamespace("signal", quietly = TRUE)) {
			if (smooth_window %% 2 == 0) smooth_window <- smooth_window + 1  # Ensure odd window
			kern <- signal::sgolay(p = 3, n = smooth_window, m = 0)
			atrial_full <- signal::filter(kern, atrial_full)

			# Replace NA values from filtering
			na_indices <- which(is.na(atrial_full))
			if (length(na_indices) > 0) {
				atrial_full[na_indices] <- signal[na_indices]
			}
		}
	}

	# Ensure the mean and standard deviation match the original signal
	# This helps maintain consistency with the original scale
	atrial_full <- atrial_full - mean(atrial_full) + mean(signal)
	atrial_full <- atrial_full * (sd(signal) / sd(atrial_full))

	return(atrial_full)
}

#' Remove QRST using Independent Component Analysis (ICA)
#'
#' A single-lead signal is first embedded (time delay, default = 5) to create a
#' pseudo-multichannel matrix, then decomposed with *FastICA* algorithm from
#' [fastICA::fastICA()]. Components whose high frequency energy (20 to 50 Hz)
#' rises >= `threshold`-fold inside a +/- 30 ms window around detected QRS peaks
#' are presumed ventricular and zeroed before back-projection.
#'
#' @param signal Numeric vector, usually upsampled, bandpass-filtered lead
#' @param frequency Sampling rate (Hz).  DEFAULT = 1000
#' @param embedding_dim Number of lags for the delay-embedding (>=3). This is
#'   equivalent to the number of components for ICA. DEFAULT = 5
#' @param qrs_loc Optional integer vector of QRS indices.  If NULL (DEFAULT),
#'   the internal `detect_QRS()` is called.
#' @param threshold Energy-ratio threshold for classifying a component as
#'   QRST-dominant. DEFAULT = 3.
#' @param post_blanking Duration of time in ms that is used to help provide some
#'   blanking and spline interpolation through residual QRS complexes. As the
#'   number increases the interpolation increases. Lower values decrease the
#'   interpolation. When 0, no blanking will occur. DEFAULT = 40 ms
#'
#' @return Numeric vector, same length as `signal`, with ventricular activity
#'   suppressed.
#'
#' @noRd
remove_qrs_with_ica <- function(signal,
																frequency = 1000,
																embedding_dim = 5,
																qrs_loc = NULL,
																threshold = 3,
																post_blanking = 40) {

	# Argument management and checks ----
	signal <- as.numeric(signal)
	N <- length(signal)

	if (!requireNamespace("fastICA", quietly = TRUE)) {
		warning("fastICA not installed, falling back to adaptive SVD.")
		return(remove_qrs_with_adaptive_svd(signal, frequency = frequency))
	}

	if (is.null(qrs_loc)) qrs_loc <- detect_QRS(signal, frequency)
	if (length(qrs_loc) < 3L) {
		warning("Too few QRS complexes for ICA, returning original signal.")
		return(signal)
	}

	embedding_dim <- max(3, min(embedding_dim, length(qrs_loc) - 1))

	# Build the embedding matrix with lags
	X <- stats::embed(signal, embedding_dim) # (N - L + 1) * L
	X <- scale(X, center = TRUE, scale = FALSE) # Scale and center

	# ICA ----

	# Try multiple ICA algorithms to increase chances of success
	ica_result <- NULL

	# List of algorithm combinations to try
	methods <- list(
		list(alg = "parallel", fun = "logcosh"),
		list(alg = "deflation", fun = "logcosh"),
		list(alg = "parallel", fun = "exp"),
		list(alg = "deflation", fun = "exp")
	)

	for (m in methods) {
		result <- tryCatch({
			fastICA::fastICA(
				X,
				n.comp = embedding_dim,
				alg.typ = m$alg,
				fun = m$fun,
				verbose = FALSE
			)
		}, error = function(e) {
			NULL
		})

		if (!is.null(result)) {
			ica_result <- result
			break
		}
	}

	# Check if ICA failed completely
	if (is.null(ica_result)) {
		warning("ICA decomposition failed with all methods. Falling back to adaptive SVD method.")
		return(remove_qrs_with_adaptive_svd(signal, frequency = frequency))
	}

	# These are the relevant ICA components
	# S are the sources
	# A are the mixing matrix
	S <- ica_result$S
	A <- ica_result$A

	# QRS sources ----

	# Setup filtering
	ny  <- frequency / 2
	bf  <- signal::butter(3, c(20, 50) / ny, type = "pass")
	win <- round(0.03 * frequency) # Small window to identify QRS

	# Compare energy levels - internal function
	comp_energy <- function(comp) {
		mean(comp^2)
	}

	# Get the high energy areas - internal function
	hf_energy   <- function(comp, idx) {
		comp_hf <- signal::filtfilt(bf, comp)
		mean(comp_hf[idx]^2)
	}

	# Adjust peak locations for shorter S (lost first L-1 samples)
	# Need to document this area better
	# Not even sure if this is helping
	shift <- embedding_dim - 1L
	keep  <- qrs_loc[qrs_loc > win & qrs_loc <= N - win] - shift
	idx_qrs <- unlist(lapply(keep, function(p) (p - win):(p + win)))

	total_energy <- apply(S, 2, comp_energy)
	qrs_energy   <- apply(S, 2, hf_energy, idx = idx_qrs)

	qrs_comps <-
		which(qrs_energy / pmax(total_energy, .Machine$double.eps) > threshold)

	# Signal reconstruction ----

	# Adjust for QRS comparisons
	# The matrix multiplication is essentially "(N - L + 1) x L"
	if (length(qrs_comps)) S[, qrs_comps] <- 0
	X_clean <- S %*% t(A)
	X_mean <- rowMeans(X_clean)

	# Pad to original length
	padded_signal <- rep(X_mean[1L], embedding_dim - 1L)
	cleaned_signal <- c(padded_signal, X_mean)
	len_diff <- N - length(cleaned_signal)
	if (len_diff > 0) cleaned_signal <-
		c(cleaned_signal, rep(cleaned_signal[length(cleaned_signal)], len_diff))

	# Post-blanking splines ----

	# This is helpful to remove the residual QRS complexes
	if (post_blanking > 0 && length(qrs_loc)) {
		win <- round((post_blanking / 1000) * frequency)
		for (p in qrs_loc) {
			i1 <- max(2, p - win)          # keep one point for interpolation
			i2 <- min(N - 1, p + win)
			cleaned_signal[i1:i2] <- stats::approx(
				x = c(i1 - 1, i2 + 1),
				y = cleaned_signal[c(i1 - 1, i2 + 1)],
				xout = i1:i2,
				method = "linear",
				rule = 2
			)$y
		}
	}

	# Return finally cleaned signal
	atrial_signal <- cleaned_signal[seq_len(N)]
	atrial_signal

}


#' Ventricular signal removal using Independent Component Analysis
#'
#' @description Removes QRST complexes using Independent Component Analysis (ICA).
#' The method creates a pseudo-multichannel signal using time-delay embedding,
#' then decomposes it with FastICA. Components with high-frequency energy around
#' QRS complexes are identified as ventricular activity and removed.
#'
#' @param signal Numeric vector of the preprocessed (upsampled and filtered) signal
#' @param frequency Sampling rate in Hz (default is 1000)
#' @param qrs_loc Optional integer vector of QRS indices. If NULL, will be detected
#' @param thresh Energy ratio threshold for classifying ventricular components (default 3)
#' @param post_blanking Time in ms to apply additional smoothing after ICA (default 40ms)
#'
#' @return Numeric vector with ventricular activity suppressed
#' @noRd
ica_removal <- function(signal,
												frequency = 1000,
												qrs_loc = NULL,
												threshold = 3,
												post_blanking = 40) {

	# Basic validation
	signal <- as.numeric(signal)
	N <- length(signal)

	# Fixed embedding dimension to avoid errors
	# Seems to be an issue with fastICA when embedding dimensions are high
	embedding_dim <- 5

	# Check for required package
	if (!requireNamespace("fastICA", quietly = TRUE)) {
		warning("Package 'fastICA' not available. Falling back to adaptive SVD method.")
		return(remove_qrs_with_adaptive_svd(signal, frequency = frequency))
	}

	# Detect QRS complexes if not provided
	if (is.null(qrs_loc)) {
		qrs_loc <- detect_QRS(signal, frequency)
	}

	# Insufficient QRS complexes for ICA
	if (length(qrs_loc) < 3) {
		warning("Too few QRS complexes detected for ICA. Returning original signal.")
		return(signal)
	}

	# Build delay-embedding matrix for pseudo-multichannel signal
	X <- stats::embed(signal, embedding_dim)  # Creates (N-embedding-dim+1) * embedding_dim matrix
	X <- scale(X, center = TRUE, scale = FALSE)  # Center but don't scale

	# Try multiple ICA algorithms to increase chances of success
	ica_result <- NULL
	# List of algorithm combinations to try
	methods <- list(
		list(alg = "parallel", fun = "logcosh"),
		list(alg = "deflation", fun = "logcosh"),
		list(alg = "parallel", fun = "exp"),
		list(alg = "deflation", fun = "exp")
	)

	for (method in methods) {
		result <- tryCatch({
			fastICA::fastICA(
				X,
				n.comp = embedding_dim,
				alg.typ = method$alg,
				fun = method$fun,
				verbose = FALSE
			)
		}, error = function(e) {
			NULL
		})

		if (!is.null(result)) {
			ica_result <- result
			break
		}
	}

	# Check if ICA failed completely
	if (is.null(ica_result)) {
		warning("ICA decomposition failed with all methods. Falling back to adaptive SVD method.")
		return(remove_qrs_with_adaptive_svd(signal, frequency = frequency))
	}

	# Extract components and mixing matrix
	S <- ica_result$S  # Sources (rows = samples, cols = components)
	A <- ica_result$A  # Mixing matrix

	# Identify QRST-dominated components based on high-frequency energy
	# Setup bandpass filter (20-50 Hz) to highlight QRS components
	nyquist <- frequency / 2
	bandpass_filter <- signal::butter(3, c(20, 50) / nyquist, type = "pass")
	window_size <- round(0.03 * frequency)  # +/- 30 ms around QRS

	# Helper functions for energy calculations
	component_energy <- function(comp) mean(comp^2)
	hf_energy <- function(comp, idx) {
		comp_hf <- signal::filtfilt(bandpass_filter, comp)
		mean(comp_hf[idx]^2)
	}

	# Adjust peak locations for the shorter matrix (lost first embedding_dim-1 samples)
	shift <- embedding_dim - 1
	valid_peaks <- qrs_loc[qrs_loc > window_size & qrs_loc <= N - window_size] - shift

	# Create window indices around QRS peaks
	qrs_indices <- unlist(lapply(valid_peaks, function(p) {
		(p - window_size):(p + window_size)
	}))

	# Calculate energy ratios and identify ventricular components
	total_energy <- apply(S, 2, component_energy)
	qrs_energy <- apply(S, 2, hf_energy, idx = qrs_indices)
	qrs_components <- which(qrs_energy / pmax(total_energy, .Machine$double.eps) > threshold)

	# Remove ventricular components and reconstruct signal
	if (length(qrs_components) > 0) {
		S[, qrs_components] <- 0
	}

	# Reconstruct cleaned signal
	X_clean <- S %*% t(A)
	clean_signal <- rowMeans(X_clean)

	# Pad to original length
	padded_signal <- c(rep(clean_signal[1], embedding_dim - 1), clean_signal)
	len_diff <- N - length(padded_signal)
	if (len_diff > 0) {
		padded_signal <- c(padded_signal, rep(padded_signal[length(padded_signal)], len_diff))
	}

	# Apply post-processing to smooth transitions around QRS complexes
	if (post_blanking > 0 && length(qrs_loc) > 0) {
		blanking_window <- round((post_blanking / 1000) * frequency)

		for (peak in qrs_loc) {
			# Determine interpolation range (keeping one point for interpolation)
			start_idx <- max(2, peak - blanking_window)
			end_idx <- min(N - 1, peak + blanking_window)

			# Linear interpolation across the blanked region
			padded_signal[start_idx:end_idx] <- stats::approx(
				x = c(start_idx - 1, end_idx + 1),
				y = padded_signal[c(start_idx - 1, end_idx + 1)],
				xout = start_idx:end_idx,
				method = "linear",
				rule = 2
			)$y
		}
	}

	# Return final atrial signal
	padded_signal[seq_len(N)]
}

# Atrial signal analysis ----

#' Analyze F waves in atrial fibrillation ECG
#'
#' @param atrial_signal Numeric vector of the atrial signal
#' @param frequency Sampling frequency of the signal
#' @param characteristics Vector of characteristics to analyze
#' @param ... Additional parameters for specific analyses
#'
#' @return A list containing the results of the requested analyses
#' @export
analyze_atrial_signal <- function(atrial_signal,
                                  frequency,
                                  characteristics = c(
                                    "amplitude",
                                    "approximate_entropy",
                                    "dominant_frequency"
                                  ),
                                  ...) {
  if (!is.numeric(atrial_signal) || !is.numeric(frequency)) {
    stop("atrial_signal must be numeric and frequency must be a number")
  }

  results <- list()

  if ("amplitude" %in% characteristics) {
    results$amplitude <- analyze_amplitude(atrial_signal, ...)
  }
  if ("approximate_entropy" %in% characteristics) {
    results$approximate_entropy <- analyze_approximate_entropy(atrial_signal, ...)
  }
  if ("dominant_frequency" %in% characteristics) {
    results$dominant_rate <- analyze_dominant_frequency(atrial_signal, frequency, ...)
  }

  results
}

# Helper functions for each characteristic
analyze_amplitude <- function(signal, ...) {
  sqrt(mean(signal^2))
}

analyze_approximate_entropy <- function(signal, ...) {
  calculate_approximate_entropy(signal, ...)
}

analyze_dominant_frequency <- function(signal, frequency, ...) {
  calculate_dominant_frequency(signal, frequency, ...)
}


# Utility functions ----

#' Detect QRS complexes in ECG signals
#'
#' @description `detect_QRS()` implements a modified Pan-Tompkins algorithm to
#' detect QRS complexes in ECG signals. The function applies a sequence of
#' processing steps including bandpass filtering, differentiation, squaring, and
#' moving window integration to identify R peaks in the signal.
#'
#' @details The Pan-Tompkins algorithm is a widely-used method for QRS detection
#' in ECG signals. This implementation follows these steps:
#'
#' 1. Bandpass filtering (5-15 Hz) to reduce noise and emphasize QRS complexes
#' 2. Differentiation to highlight the steep slopes of QRS complexes 3. Squaring
#' to amplify high-frequency components 4. Moving window integration to consider
#' the overall QRS morphology 5. Adaptive thresholding to identify peaks 6.
#' Application of a refractory period to prevent multiple detections of the same
#' QRS complex
#'
#' The function is designed to work with single-lead ECG signals, typically
#' sampled at 250-1000 Hz.
#'
#' @param signal Numeric vector representing the ECG signal
#' @param frequency Sampling frequency of the signal in Hz
#' @param window_size Width of the integration window in seconds, default is
#'   0.150 seconds
#'
#' @return Integer vector containing the sample indices of detected QRS
#'   complexes
#'
#' @references Pan, J., & Tompkins, W. J. (1985). A real-time QRS detection
#' algorithm. IEEE Transactions on Biomedical Engineering, (3), 230-236.
#' \doi{10.1109/TBME.1985.325532}
#'
#' @examples
#' \dontrun{
#' # Load ECG data
#' ecg_data <- read_muse(system.file("extdata", "muse-sinus.xml", package = "EGM"))
#'
#' # Extract lead II signal
#' signal <- ecg_data$signal$II
#'
#' # Get sampling frequency from header
#' freq <- attributes(ecg_data$header)$record_line$frequency
#'
#' # Detect QRS complexes
#' qrs_locations <- detect_QRS(signal, freq)
#'
#' # Plot ECG with detected QRS complexes
#' plot(signal, type = "l", xlab = "Sample", ylab = "Amplitude")
#' points(qrs_locations, signal[qrs_locations], col = "red", pch = 19)
#' }
#'
#' @export
detect_QRS <- function(signal, frequency, window_size = 0.150) {
	# Step 1: Bandpass Filtering (5-15 Hz)
	nyquist_freq <- frequency / 2
	low_cutoff <- 5 / nyquist_freq
	high_cutoff <- 15 / nyquist_freq

	bp_filter <- signal::butter(n = 4, W = c(low_cutoff, high_cutoff), type = "pass")
	filtered_signal <- signal::filtfilt(bp_filter, signal)

	# Step 2: Differentiation to highlight QRS slopes
	derivative_filter <- c(-1, -2, 0, 2, 1) * (frequency / 8)
	differentiated_signal <- signal::filter(derivative_filter, 1, filtered_signal)

	# Step 3: Squaring to amplify high-frequency components
	squared_signal <- differentiated_signal^2

	# Step 4: Moving Window Integration
	window_size <- round(window_size * frequency)
	integration_filter <- rep(1 / window_size, window_size)
	integrated_signal <- signal::filter(integration_filter, 1, squared_signal)

	# Step 5: Thresholding and Peak Detection
	threshold <- mean(integrated_signal) + 0.5 * sd(integrated_signal)
	is_peak <- (integrated_signal > threshold) &
		(c(FALSE, integrated_signal[-length(integrated_signal)] < integrated_signal[-1])) &
		(c(integrated_signal[-1] > integrated_signal[-length(integrated_signal)], FALSE))
	peak_indices <- which(is_peak)

	# Apply a refractory period of 200 ms
	refractory_period <- round(0.200 * frequency)
	final_peak_indices <- c()
	last_peak <- -Inf

	for (idx in peak_indices) {
		if ((idx - last_peak) > refractory_period) {
			final_peak_indices <- c(final_peak_indices, idx)
			last_peak <- idx
		}
	}

	final_peak_indices
}


#' Calculate Approximate Entropy (Ap_en) of a time series
#'
#' @description
#' This function computes the approximate entropy (Ap_en) of a time series using
#' the method described by Pincus (1991). Ap_en is a measure of the regularity
#' and complexity of the time series. It is calculated by comparing vectors
#' derived from the time series in an m-dimensional embedded space and in an
#' (m+1)-dimensional space. The basic steps are:
#'
#' 1. **Embedding:** The time series is embedded into vectors of length m (and
#' m+1) by taking successive elements. For a time series of length N, this
#' produces (N - m + 1) (or (N - m) for m+1) vectors.
#'
#' 2. **Distance Calculation:** For each pair of embedded vectors, the Chebyshev
#' distance (i.e., the maximum absolute difference among corresponding elements)
#' is computed. If the distance between two vectors is less than or equal to a
#' tolerance r, they are considered "similar."
#'
#' 3. **Counting and Averaging:** For each embedded vector, the function counts
#' the number of similar vectors (including itself) and takes the natural
#' logarithm of the ratio of this count to the total number of vectors. These
#' log-values are then averaged to yield a statistic phi.
#'
#' 4. **Ap_en Calculation:** The approximate entropy is the difference between
#' the phi computed for dimension m and the phi computed for dimension m+1,
#' i.e., Ap_en = phi(m) - phi(m+1).
#'
#' The tolerance r is typically chosen as a multiple of the standard deviation
#' of the time series (commonly 3.5 * sd(x)). If r is not provided (or is
#' negative), it is calculated automatically.
#'
#' @param x Numeric vector of the time series
#' @param m Embedding dimension (sample size), default is 3
#' @param r Tolerance (threshold), default is 3.5 * sd(x)
#' @param implementation Method to use for calculation, default is "C++", but can
#'   also be done in "R". The C++ implementation is faster.
#'
#' @return Approximate Entropy value
#'
#' @references Pincus, S. M. (1991). Approximate entropy as a measure of system
#'   complexity. Proceedings of the National Academy of Sciences, 88(6),
#'   2297-2301.
#'
#' @examples
#' # Example: Calculate approximate entropy for a random time series
#' set.seed(123)
#' x <- rnorm(1000)
#' calculate_approximate_entropy(x, m = 3, r = -1, implementation = "R")
#'
#' @export
calculate_approximate_entropy <- function(x, m = 3, r = NULL, implementation = "C++") {

  x <- as.double(x)

  if (implementation == "C++") {
    apen <- calculate_apen_cpp(x, m, r)
  } else if (implementation == "R") {
    apen <- calculate_apen_r(x, m, r)
  } else {
    stop("Invalid method specified. Choose 'R' or 'C++'")
  }

  # Return approx entropy
  apen
}

#' C++ implementation of approximate entropy
#' @noRd
calculate_apen_cpp <- function(x, m, r) {
  # Use -1 as a flag for C++ to compute r internally.
  # Cannot pass NULL to C++
  if (is.null(r)) r <- -1
  # This is a C++ file, documented in src/approximate_entropy.cpp
  apen <- calculate_approximate_entropy_cpp(x, m, r)
}

#' R implementation of approximate entropy
#' @noRd
calculate_apen_r <- function(x, m, r) {
  N <- length(x)
  r <- if (is.null(r)) 3.5 * sd(x) else r
  x <- as.vector(x)

  embed_matrix <- function(x, m) {
    N <- length(x)
    matrix(sapply(1:m, function(i) x[i:(N - m + i)]), ncol = m)
  }

  correlation_integral <- function(x, m, r) {
    N <- nrow(x)
    count <- sapply(1:N, function(i) {
      sum(apply(abs(x - rep(x[i, ], each = nrow(x))), 1, max) <= r)
    })
    sum(log(count / N)) / N
  }

  phi_m <- correlation_integral(embed_matrix(x, m), m, r)
  phi_m1 <- correlation_integral(embed_matrix(x, m + 1), m + 1, r)

  phi_m - phi_m1
}


#' Calculate Dominant Frequency of a time series
#'
#' @param x Numeric vector of the time series
#' @param frequency Sampling frequency of the signal
#' @param f_min Minimum frequency to consider (default 4 Hz)
#' @param f_max Maximum frequency to consider (default 9 Hz)
#'
#' @return Dominant Frequency in Hz
#'
#' @import stats
#' @export
calculate_dominant_frequency <- function(x, frequency, f_min = 4, f_max = 9) {
  x <- as.vector(x)
  psd <- spec.pgram(x, plot = FALSE)
  freq <- psd$freq * frequency
  power <- psd$spec

  idx <- which(freq >= f_min & freq <= f_max)
  freq_range <- freq[idx]
  power_range <- power[idx]

  freq_range[which.max(power_range)]
}
