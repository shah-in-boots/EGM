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
  availableLeads <- names(object$signal)[-1]

  # Determine which leads to process
  leadsToProcess <- if (is.null(lead)) availableLeads else lead
  if (!all(leadsToProcess %in% availableLeads)) {
    stop("Specified lead not found in the signal data")
  }

  # Process each lead
  results <- lapply(leadsToProcess, function(l) {
    process_single_lead(object, l, qrs_method, f_characteristics)
  })

  # Name the results
  names(results) <- leadsToProcess
  results
}

# Signal cleaning ----

# Helper function to process a single lead
process_single_lead <- function(object, lead, qrs_method, f_characteristics) {
  # Extract signal for the specified lead
  signal <- object$signal[[lead]]
  hz <- attributes(object$header)$record_line$frequency

  # Preprocess signal and apply band pass filter
  upsampledSignal <-
    upsample_signal(signal, original_frequency = hz, new_frequency = 1000) |>
    filter_bandpass(signal = _, frequency = 1000)

  # Ventricular signal removal (QRST cancellation)
  atrialActivity <- remove_ventricular_signal(upsampledSignal, method = qrs_method)

  # Atrial signal analysis and feature extraction
  features <- analyze_atrial_signal(
    atrialActivity,
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
    originalLength <- length(signal)
    t <- seq(0, (originalLength - 1) / original_frequency, length.out = originalLength)
    tNew <- seq(0, (originalLength - 1) / original_frequency, length.out = originalLength * (new_frequency / original_frequency))
    upsampled <- stats::approx(t, signal, xout = tNew, method = "linear")$y
    frequency <- new_frequency
  }

  # Return new upsampled signal
  upsampled
}

#' Apply bandpass filter (0.5-30 Hz is the default)
#' @noRd
filter_bandpass <- function(signal, frequency, low = 0.5, high = 30) {
  nyquistFreq <- frequency / 2
  low <- low / nyquistFreq
  high <- high / nyquistFreq
  bf <- signal::butter(3, c(low, high), type = "pass")
  signal::filtfilt(bf, signal)
}


# QRS Methods ----

# Ventricular signal removal
remove_ventricular_signal <- function(signal, method = "adaptive_svd") {
  if (method == "adaptive_svd") {
    return(adaptive_svd_removal(signal))
  } else if (method == "ica") {
    return(ica_removal(signal))
  } else {
    stop("Unsupported method. Choose 'adaptive_svd' or 'ica'")
  }
}

#' Helper function to perform adaptive SVD cancellation
#' Add protections to stop matrix conformation issues, ASS @2025-02-18
#' @noRd
adaptive_svd_removal <- function(signal, frequency = 1000, qrs_window = 0.12, qrs_loc = NULL) {

  # Detect QRS complexes
  # Standard Pan Tompkins algorithm
  if (is.null(qrs_loc)) {
    qrs_loc <- detect_QRS(signal, frequency)
  }

  windowSize <- round(0.5 * frequency)
  halfWindow <- floor(windowSize / 2)

  # Extract QRST segments
  qrstSegments <- lapply(qrs_loc, function(idx) {
    startIdx <- max(1, idx - halfWindow)
    endIdx <- min(length(signal), idx + halfWindow - 1)
    segment <- signal[startIdx:endIdx]
    if (length(segment) < windowSize) {
      segment <- c(segment, rep(0, windowSize - length(segment)))
    }
    segment
  })

  qrstMatrix <- do.call(rbind, qrstSegments)

  # Perform SVD (adaptive)
  svdResult <- svd(qrstMatrix)

  # Determine number of components to keep (explaining 99% of variance)
  cumulativeVariance <- cumsum(svdResult$d^2) / sum(svdResult$d^2)
  nComponents <- max(which(cumulativeVariance > 0.99))

  # Reconstruct QRST template
  qrstTemplate <-
    svdResult$u[, 1:nComponents, drop = FALSE] %*%
    diag(svdResult$d[1:nComponents, drop = FALSE]) %*%
    t(svdResult$v[, 1:nComponents, drop = FALSE])

  # Subtract QRST template from original signal
  atrialSignal <- signal
  for (i in seq_along(qrs_loc)) {
    idx <- qrs_loc[i]
    startIdx <- max(1, idx - halfWindow)
    endIdx <- min(length(signal), idx + halfWindow - 1)
    segmentLength <- endIdx - startIdx + 1
    atrialSignal[startIdx:endIdx] <- atrialSignal[startIdx:endIdx] - qrstTemplate[i, 1:segmentLength]
  }

  atrialSignal
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
  calculate_apen(signal, ...)
}

analyze_dominant_frequency <- function(signal, frequency, ...) {
  calculate_dominant_frequency(signal, frequency, ...)
}


# Utility functions ----

# Helper function to detect QRS complexes using Pan-Tompkins algorithm
#' @export
detect_QRS <- function(signal, frequency, window_size = 0.150) {
  # Step 1: Bandpass Filtering (5-15 Hz)
  nyquistFreq <- frequency / 2
  lowCutoff <- 5 / nyquistFreq
  highCutoff <- 15 / nyquistFreq

  bpFilter <- signal::butter(n = 4, W = c(lowCutoff, highCutoff), type = "pass")
  filteredSignal <- signal::filtfilt(bpFilter, signal)

  # Step 2: Differentiation to highlight QRS slopes
  derivativeFilter <- c(-1, -2, 0, 2, 1) * (frequency / 8)
  differentiatedSignal <- signal::filter(derivativeFilter, 1, filteredSignal)

  # Step 3: Squaring to amplify high-frequency components
  squaredSignal <- differentiatedSignal^2

  # Step 4: Moving Window Integration
  windowSize <- round(window_size * frequency)
  integrationFilter <- rep(1 / windowSize, windowSize)
  integratedSignal <- signal::filter(integrationFilter, 1, squaredSignal)

  # Step 5: Thresholding and Peak Detection
  threshold <- mean(integratedSignal) + 0.5 * sd(integratedSignal)
  isPeak <- (integratedSignal > threshold) &
    (c(FALSE, integratedSignal[-length(integratedSignal)] < integratedSignal[-1])) &
    (c(integratedSignal[-1] > integratedSignal[-length(integratedSignal)], FALSE))
  peakIndices <- which(isPeak)

  # Apply a refractory period of 200 ms
  refractoryPeriod <- round(0.200 * frequency)
  finalPeakIndices <- c()
  lastPeak <- -Inf

  for (idx in peakIndices) {
    if ((idx - lastPeak) > refractoryPeriod) {
      finalPeakIndices <- c(finalPeakIndices, idx)
      lastPeak <- idx
    }
  }

  finalPeakIndices
}


#' Calculate Approximate Entropy (ApEn) of a time series
#'
#' @description
#' This function computes the approximate entropy (ApEn) of a time series using
#' the method described by Pincus (1991). ApEn is a measure of the regularity
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
#' 4. **ApEn Calculation:** The approximate entropy is the difference between
#' the phi computed for dimension m and the phi computed for dimension m+1,
#' i.e., ApEn = phi(m) - phi(m+1).
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

  embedMatrix <- function(x, m) {
    N <- length(x)
    matrix(sapply(1:m, function(i) x[i:(N - m + i)]), ncol = m)
  }

  correlationIntegral <- function(x, m, r) {
    N <- nrow(x)
    count <- sapply(1:N, function(i) {
      sum(apply(abs(x - rep(x[i, ], each = nrow(x))), 1, max) <= r)
    })
    sum(log(count / N)) / N
  }

  phi_m <- correlationIntegral(embedMatrix(x, m), m, r)
  phi_m1 <- correlationIntegral(embedMatrix(x, m + 1), m + 1, r)

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
