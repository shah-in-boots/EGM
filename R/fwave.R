#' Extract F wave features from ECG
#'
#' This function analyzes F waves in an ECG signal, extracting various
#' characteristics.
#'
#' @param object An object of class `egm`
#'
#' @param lead Optional. A character string specifying the lead to analyze. If
#'   NULL (default), all available leads will be processed.
#'
#' @param qrs_method Method for ventricular signal removal. Default is
#'   "adaptive_svd" for adaptive singular value decomposition.
#'
#' @param f_characteristics Vector of characteristics to analyze from ECG
#'   signal. Options: "amplitude", "approximate_entropy", "dominant_frequency".
#'   Please see [calculate_approximate_entropy()] and
#'   [calculate_dominant_frequency()] for more details.
#'
#' @param ... Additional arguments passed to methods
#'
#' @references
#'
#' Park, Junbeom, Chungkeun Lee, Eran Leshem, Ira Blau, Sungsoo Kim, Jung Myung
#' Lee, Jung-A Hwang, Byung-il Choi, Moon-Hyoung Lee, and Hye Jin Hwang. “Early
#' Differentiation of Long-Standing Persistent Atrial Fibrillation Using the
#' Characteristics of Fibrillatory Waves in Surface ECG Multi-Leads.” Scientific
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

  # Determine which leads to process
  leads_to_process <- if (is.null(lead)) available_leads else lead
  if (!all(leads_to_process %in% available_leads)) {
    stop("Specified lead not found in the signal data")
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
    return(ica_removal(signal))
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


# Old version that will be tossed later
remove_qrs_with_adaptive_svd_old <- function(signal,
																				 frequency = 1000,
																				 qrs_window = 0.12,
																				 qrs_loc = NULL) {

  # Detect QRS complexes
  # Standard Pan Tompkins algorithm
  if (is.null(qrs_loc)) {
    qrs_loc <- detect_QRS(signal, frequency)
  }

  window_size <- round(0.5 * frequency)
  half_window <- floor(window_size / 2)

  # Extract QRST segments
  qrst_segments <- lapply(qrs_loc, function(idx) {
    start_idx <- max(1, idx - half_window)
    end_idx <- min(length(signal), idx + half_window - 1)
    segment <- signal[start_idx:end_idx]
    if (length(segment) < window_size) {
      segment <- c(segment, rep(0, window_size - length(segment)))
    }
    segment
  })

  qrst_matrix <- do.call(rbind, qrst_segments)

  # Perform SVD (adaptive)
  svd_result <- svd(qrst_matrix)

  # Determine number of components to keep (explaining 99% of variance)
  cumulative_variance <- cumsum(svd_result$d^2) / sum(svd_result$d^2)
  n_components <- max(which(cumulative_variance > 0.99))

  # Reconstruct QRST template
  qrst_template <-
    svd_result$u[, 1:n_components, drop = FALSE] %*%
    diag(svd_result$d[1:n_components, drop = FALSE]) %*%
    t(svd_result$v[, 1:n_components, drop = FALSE])

  # Subtract QRST template from original signal
  atrial_signal <- signal
  for (i in seq_along(qrs_loc)) {
    idx <- qrs_loc[i]
    start_idx <- max(1, idx - half_window)
    end_idx <- min(length(signal), idx + half_window - 1)
    segment_length <- end_idx - start_idx + 1
    atrial_signal[start_idx:end_idx] <- atrial_signal[start_idx:end_idx] - qrst_template[i, 1:segment_length]
  }

  atrial_signal
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

# Helper function to detect QRS complexes using Pan-Tompkins algorithm
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
