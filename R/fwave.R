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
#' @param f_characteristics Vector of characteristics to analyze from ECG signal.
#'   Options: "amplitude", "irregularity", "dominant_frequency"
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

  # Preprocess signal
  upsampledSignal <- upsample_signal(signal, original_frequency = hz, new_frequency = 1000)

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

# Preprocessing
upsample_signal <- function(signal, original_frequency, new_frequency) {
  # Increase sampling rate if necessary (e.g., from 500 Hz to 1000 Hz)
  if (original_frequency < new_frequency) {
    originalLength <- length(signal)
    t <- seq(0, (originalLength - 1) / original_frequency, length.out = originalLength)
    tNew <- seq(0, (originalLength - 1) / original_frequency, length.out = originalLength * (new_frequency / original_frequency))
    signal <- stats::approx(t, signal, xout = tNew, method = "linear")$y
    frequency <- new_frequency
  }

  # Apply bandpass filter (0.5-30 Hz)
  nyquistFreq <- frequency / 2
  low <- 0.5 / nyquistFreq
  high <- 30 / nyquistFreq
  bf <- signal::butter(3, c(low, high), type = "pass")
  filteredSignal <- signal::filtfilt(bf, signal)

  # Return new upsampled, and filtered signal
  filteredSignal
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

# Helper function to perform adaptive SVD cancellation
adaptive_svd_removal <- function(signal, frequency = 1000, qrs_window = 0.12, qrs_loc = NULL) {

  # Step 1: Detect QRS complexes
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

  # Step 2: Perform SVD (adaptive)
  svdResult <- svd(qrstMatrix)

  # Determine number of components to keep (explaining 99% of variance)
  cumulativeVariance <- cumsum(svdResult$d^2) / sum(svdResult$d^2)
  nComponents <- which(cumulativeVariance > 0.99)[1]

  # Reconstruct QRST template
  qrstTemplate <-
    svdResult$u[, 1:nComponents] %*%
    diag(svdResult$d[1:nComponents]) %*%
    t(svdResult$v[, 1:nComponents])

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
#' @param x Numeric vector of the time series
#' @param m Embedding dimension (sample size), default is 3
#' @param r Tolerance (threshold), default is 3.5 * sd(x)
#'
#' @return Approximate Entropy value
#'
#' @references Pincus, S. M. (1991). Approximate entropy as a measure of system
#'   complexity. Proceedings of the National Academy of Sciences, 88(6),
#'   2297-2301.
#' @export
calculate_apen <- function(x, m = 3, r = NULL) {
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
