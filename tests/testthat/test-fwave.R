# File: tests/testthat/test-fwave.R
#
# The validation tests below are deliberately written against synthetic signals
# with known ground truth, so that the algorithmic guarantees are checked
# everywhere rather than only where the bundled records can be read.

# Helpers ----

#' Synthesise a multi-lead AF-like ECG with a known fibrillatory frequency
#'
#' Beats are placed at irregular intervals, and each lead gets its own QRST
#' amplitude and polarity so that a cross-lead template has something to fit.
simulate_af <- function(
  frequency = 500,
  duration = 10,
  f_hz = 6,
  n_leads = 12,
  f_amplitude = 0.05,
  seed = 42,
  regular = FALSE,
  jitter_frac = 0.30
) {
  set.seed(seed)
  n <- as.integer(frequency * duration)
  t <- seq(0, duration, length.out = n)

  # Atrial signal: a fibrillatory wave with a little frequency wander
  atrial <- f_amplitude * sin(2 * pi * f_hz * t + 0.4 * sin(2 * pi * 0.3 * t))

  rr_mean <- 0.65 * frequency
  n_beats <- as.integer(duration * frequency / rr_mean) + 2L
  jitter <- if (regular) {
    rep(0, n_beats)
  } else {
    stats::runif(n_beats, -jitter_frac, jitter_frac) * rr_mean
  }
  loc <- cumsum(c(0.4 * frequency, rep(rr_mean, n_beats - 1) + jitter[-1]))
  loc <- as.integer(loc[loc > 0.3 * frequency & loc < n - 0.4 * frequency])

  # A QRS spike followed by a T wave, scaled per lead
  lead_gain <- seq(-1.2, 1.5, length.out = n_leads)
  qrs_shape <- function(idx, centre) exp(-((idx - centre) / (0.012 * frequency))^2)
  t_shape <- function(idx, centre) {
    0.25 * exp(-((idx - centre - 0.22 * frequency) / (0.05 * frequency))^2)
  }

  signals <- lapply(seq_len(n_leads), function(j) {
    x <- atrial + stats::rnorm(n, sd = f_amplitude * 0.15)
    for (p in loc) {
      w <- max(1L, p - as.integer(0.25 * frequency)):
        min(n, p + as.integer(0.45 * frequency))
      x[w] <- x[w] + lead_gain[j] * (qrs_shape(w, p) + t_shape(w, p))
    }
    x
  })
  names(signals) <- paste0("L", seq_len(n_leads))

  list(signals = signals, qrs_loc = loc, frequency = frequency, atrial = atrial)
}

# Core API ----

test_that("extract_f_waves returns features and diagnostics per lead", {
  skip_on_ci()

  mock_af <- read_wfdb("muse-af", system.file("extdata", package = "EGM"))
  result <- extract_f_waves(mock_af, verbose = FALSE)

  expect_s3_class(result, "f_wave_analysis")
  expect_true(all(c("features", "record") %in% names(result)))
  expect_equal(nrow(result$features), length(names(mock_af$signal)) - 1L)

  # Every spectral estimate must arrive with the means to judge it
  expect_true(all(
    c("dominant_rate", "harmonic_index", "on_harmonic", "cancellation_residual") %in%
      names(result$features)
  ))
  expect_true(all(
    c("n_beats_cancelled", "n_beats_skipped", "spatial_dispersion") %in%
      names(result$record)
  ))
})

test_that("extract_f_waves handles invalid input", {
  skip_on_ci()

  mock_af <- read_wfdb("muse-af", system.file("extdata", package = "EGM"))

  expect_error(extract_f_waves("not an egm object"), "class <EGM> or <ECG>")
  expect_error(
    extract_f_waves(mock_af, lead = "non_existent_lead"),
    "Not a surface ECG lead"
  )
  expect_error(
    extract_f_waves(mock_af, f_characteristics = "invalid_characteristic"),
    "Invalid characteristic specified"
  )
  expect_error(extract_f_waves(mock_af, band = c(10, 4)), "increasing")
  expect_error(extract_f_waves(mock_af, band = c(4, 400)), "Nyquist")
})

test_that("arguments actually reach the analysis layer", {
  # `...` used to be declared and silently dropped, so no parameter could
  # influence anything downstream
  sim <- simulate_af()
  narrow <- cancel_ventricular_signal(
    sim$signals,
    frequency = sim$frequency,
    qrs_loc = sim$qrs_loc
  )

  df_low <- calculate_dominant_frequency(
    narrow$atrial[[1]],
    sim$frequency,
    f_min = 2,
    f_max = 4
  )
  df_high <- calculate_dominant_frequency(
    narrow$atrial[[1]],
    sim$frequency,
    f_min = 5,
    f_max = 8
  )

  expect_true(df_low >= 2 && df_low <= 4)
  expect_true(df_high >= 5 && df_high <= 8)
  expect_false(isTRUE(all.equal(df_low, df_high)))
})

# Validation: the three checks the package should ship ----

test_that("VALIDATION harmonic test separates AF from regular rhythm", {
  # In a regular rhythm there is no fibrillatory wave to find, so the estimator
  # returns whatever is largest in the band -- which is ventricular residual,
  # sitting on a heart-rate harmonic. That is the known-negative case and it
  # should be flagged loudly rather than avoided.
  regular <- simulate_af(regular = TRUE, f_amplitude = 0, seed = 7)
  irregular <- simulate_af(regular = FALSE, f_amplitude = 0.05, seed = 7)

  harmonic_share <- function(sim) {
    res <- cancel_ventricular_signal(
      sim$signals,
      frequency = sim$frequency,
      qrs_loc = sim$qrs_loc
    )
    median_rr <- stats::median(diff(sim$qrs_loc))
    flags <- vapply(
      res$atrial,
      function(x) {
        df <- calculate_dominant_frequency(x, sim$frequency, f_min = 4, f_max = 10)
        hi <- df * median_rr / sim$frequency
        isTRUE(abs(hi - round(hi)) < 0.15)
      },
      logical(1)
    )
    mean(flags)
  }

  expect_gt(harmonic_share(regular), harmonic_share(irregular))
})

test_that("VALIDATION dominant frequency is not pinned at a band edge", {
  # The previous implementation returned exactly 4.000 Hz on the bundled AF
  # record: the argmax landed on the first bin of the search range, which is the
  # signature of an estimator reporting its own boundary rather than a peak.
  sim <- simulate_af(f_hz = 6)
  res <- cancel_ventricular_signal(
    sim$signals,
    frequency = sim$frequency,
    qrs_loc = sim$qrs_loc
  )

  band <- c(4, 10)
  df <- calculate_dominant_frequency(
    res$atrial[[1]],
    sim$frequency,
    f_min = band[1],
    f_max = band[2]
  )

  expect_gt(abs(df - band[1]), 0.1)
  expect_gt(abs(df - band[2]), 0.1)

  # And it should land on the frequency actually put into the signal
  expect_equal(df, 6, tolerance = 0.5)
})

test_that("VALIDATION no cancelled window is identically zero", {
  # A group small enough to reconstruct itself is subtracted to exactly zero,
  # which deletes the atrial signal in that window. So is replacing a window
  # with a straight line. Both are invisible unless asserted against.
  sim <- simulate_af()
  res <- cancel_ventricular_signal(
    sim$signals,
    frequency = sim$frequency,
    qrs_loc = sim$qrs_loc
  )

  for (x in res$atrial) {
    v <- as.numeric(x)

    # No constant run
    flat <- rle(abs(diff(v)) < 1e-12)
    max_flat <- if (any(flat$values)) max(flat$lengths[flat$values]) else 0L
    expect_lt(max_flat, 10L)

    # No perfectly linear run either, which is what interpolation leaves behind
    linear <- rle(abs(diff(v, differences = 2)) < 1e-12)
    max_linear <- if (any(linear$values)) max(linear$lengths[linear$values]) else 0L
    expect_lt(max_linear, 10L)
  }
})

test_that("VALIDATION fibrillatory rate is physiologically plausible", {
  # Sustained human AF runs at roughly 300-450 fibrillations per minute. This is
  # a smoke test on one synthetic record; asserting a tight cohort median needs
  # a cohort, not a single strip.
  sim <- simulate_af(f_hz = 6.2)
  res <- cancel_ventricular_signal(
    sim$signals,
    frequency = sim$frequency,
    qrs_loc = sim$qrs_loc
  )
  rate <- calculate_dominant_frequency(res$atrial[[1]], sim$frequency) * 60

  expect_gt(rate, 300)
  expect_lt(rate, 450)
})

# Cancellation ----

test_that("cancellation requires a sampling frequency", {
  sim <- simulate_af()
  expect_error(cancel_ventricular_signal(sim$signals), "frequency")
  expect_error(EGM:::remove_ventricular_signal(rnorm(1000)), "frequency")
})

test_that("spatiotemporal cancellation beats per-lead cancellation on residual", {
  # The point of the cross-lead terms is that a single-lead template cannot
  # absorb rotation of the electrical axis, so it leaves a larger residual.
  sim <- simulate_af()

  residual_for <- function(method) {
    res <- cancel_ventricular_signal(
      sim$signals,
      frequency = sim$frequency,
      qrs_loc = sim$qrs_loc,
      method = method
    )
    stats::median(vapply(
      names(sim$signals),
      function(l) {
        EGM:::cancellation_residual(
          res$atrial[[l]],
          sim$signals[[l]],
          sim$qrs_loc,
          sim$frequency
        )
      },
      numeric(1)
    ))
  }

  expect_lt(residual_for("spatiotemporal"), residual_for("average_beat"))
})

test_that("cancellation refuses to run on too few beats", {
  sim <- simulate_af()
  expect_warning(
    res <- cancel_ventricular_signal(
      sim$signals,
      frequency = sim$frequency,
      qrs_loc = sim$qrs_loc[1:2]
    ),
    "required"
  )
  expect_equal(res$n_beats_cancelled, 0L)
  # The originals come back untouched rather than zeroed
  expect_equal(res$atrial[[1]], sim$signals[[1]])
})

test_that("cancellation survives rank-deficient lead sets", {
  # Derived-lead exports and flat leads make the template columns linearly
  # dependent; the record should degrade to the well-conditioned columns
  sim <- simulate_af(n_leads = 4)
  sim$signals$duplicate <- sim$signals$L1
  sim$signals$flat <- rep(0, length(sim$signals$L1))

  expect_no_error(
    res <- cancel_ventricular_signal(
      sim$signals,
      frequency = sim$frequency,
      qrs_loc = sim$qrs_loc
    )
  )
  expect_true(all(vapply(res$atrial, function(x) all(is.finite(x)), logical(1))))
  expect_gt(res$n_beats_cancelled, 0L)
})

test_that("aberrancy is judged on morphology, not RR interval", {
  # In AF the RR interval is irregular by definition, so an RR-based criterion
  # fires on normally conducted beats in the exact rhythm this targets. Every
  # beat below is morphologically identical; only the timing varies.
  sim <- simulate_af(seed = 3, jitter_frac = 0.55)

  res <- cancel_ventricular_signal(
    sim$signals,
    frequency = sim$frequency,
    qrs_loc = sim$qrs_loc
  )

  rr <- diff(sim$qrs_loc)
  median_rr <- stats::median(rr)
  rr_flagged <- sum(c(FALSE, abs(rr - median_rr) > 0.4 * median_rr))

  expect_gt(rr_flagged, 0L)
  expect_lt(res$n_beats_aberrant, rr_flagged)
  expect_equal(res$n_beats_aberrant, 0L)
})

test_that("low-rank beat model never reconstructs a group exactly", {
  # A 95 percent variance rule selects both components of a two-beat group, the
  # reconstruction is exact, and the residual is identically zero
  sim <- simulate_af()
  signal <- sim$signals[[1]]

  for (k in c(3, 4, 5, 10)) {
    out <- EGM:::process_beat_group(
      signal,
      sim$qrs_loc[seq_len(min(k, length(sim$qrs_loc)))],
      half_window = as.integer(0.12 * sim$frequency),
      frequency = sim$frequency,
      smoothing = FALSE
    )
    expect_false(isTRUE(all.equal(out, signal)))
    expect_gt(stats::sd(out - signal), 0)
    # Residual must not vanish anywhere
    expect_gt(max(abs(out - signal)), .Machine$double.eps)
  }
})

# Spectral estimation ----

test_that("Welch spectrum recovers a known frequency", {
  fs <- 500
  x <- sin(2 * pi * 6 * seq(0, 10, by = 1 / fs)) + rnorm(5001, sd = 0.2)
  psd <- calculate_welch_spectrum(x, frequency = fs)

  expect_named(psd, c("freq", "spec"))
  expect_equal(psd$freq[which.max(psd$spec)], 6, tolerance = 0.2)

  # Zero-padding pins the resolution, so bin spacing must not move with the
  # length of the record
  short <- calculate_welch_spectrum(x[1:3000], frequency = fs)
  expect_equal(diff(psd$freq)[1], diff(short$freq)[1])
})

test_that("Welch averaging is less variable than a raw periodogram", {
  fs <- 500
  peaks_welch <- numeric(20)
  peaks_raw <- numeric(20)
  for (i in seq_len(20)) {
    set.seed(100 + i)
    x <- sin(2 * pi * 6 * seq(0, 10, by = 1 / fs)) + rnorm(5001, sd = 3)
    psd <- calculate_welch_spectrum(x, frequency = fs)
    idx <- psd$freq >= 4 & psd$freq <= 10
    peaks_welch[i] <- psd$freq[idx][which.max(psd$spec[idx])]

    raw <- stats::spec.pgram(x, plot = FALSE)
    f <- raw$freq * fs
    j <- f >= 4 & f <= 10
    peaks_raw[i] <- f[j][which.max(raw$spec[j])]
  }
  expect_lt(stats::sd(peaks_welch), stats::sd(peaks_raw))
})

test_that("calculate_dominant_frequency respects the band", {
  fs <- 500
  x <- sin(2 * pi * 6 * seq(0, 10, by = 1 / fs)) + rnorm(5001, sd = 0.1)

  expect_equal(calculate_dominant_frequency(x, fs), 6, tolerance = 0.3)

  # Slow flutter falls below the AF default and must be reachable
  y <- sin(2 * pi * 3.2 * seq(0, 10, by = 1 / fs)) + rnorm(5001, sd = 0.1)
  expect_equal(
    calculate_dominant_frequency(y, fs, f_min = 2.5, f_max = 6),
    3.2,
    tolerance = 0.3
  )
})

test_that("organization index separates organised from disorganised signals", {
  fs <- 500
  t <- seq(0, 10, by = 1 / fs)
  organised <- sin(2 * pi * 6 * t)
  disorganised <- rnorm(length(t))

  oi_org <- calculate_organization_index(organised, fs)
  oi_dis <- calculate_organization_index(disorganised, fs)

  expect_gt(oi_org, oi_dis)
  expect_true(oi_org >= 0 && oi_org <= 1)
  expect_true(oi_dis >= 0 && oi_dis <= 1)
})

# Amplitude ----

test_that("TQ amplitude excludes the QRS and reports its coverage", {
  sim <- simulate_af()
  res <- cancel_ventricular_signal(
    sim$signals,
    frequency = sim$frequency,
    qrs_loc = sim$qrs_loc
  )

  amp <- EGM:::amplitude_features(
    res$atrial[[1]],
    original_signal = sim$signals[[1]],
    frequency = sim$frequency,
    qrs_loc = sim$qrs_loc,
    annotation = NULL,
    window = "tq"
  )
  all_amp <- EGM:::amplitude_features(
    res$atrial[[1]],
    original_signal = sim$signals[[1]],
    frequency = sim$frequency,
    qrs_loc = sim$qrs_loc,
    annotation = NULL,
    window = "all"
  )

  expect_true(amp$tq_fraction > 0 && amp$tq_fraction < 1)
  expect_equal(all_amp$tq_fraction, 1)

  # Whatever cancellation fails to remove sits at the QRS, so a whole-record
  # measurement reads larger than a TQ-restricted one
  expect_lt(amp$f_amplitude_p2p, all_amp$f_amplitude_p2p)

  expect_true(is.finite(amp$qrs_amplitude))
  expect_equal(amp$f_ratio, amp$f_amplitude_p2p / amp$qrs_amplitude)
})

test_that("TQ segments are read from a wave-typed annotation", {
  skip_on_ci()

  af <- read_wfdb(
    "muse-af",
    system.file("extdata", package = "EGM"),
    annotator = "ecgpuwave"
  )
  ann <- EGM:::resolve_annotation(af$annotation)
  expect_true(is.data.frame(ann))

  segs <- EGM:::tq_segments(5000, 500, qrs_loc = NULL, annotation = af$annotation)
  expect_gt(length(segs), 5)

  # Each segment must run from a T offset to the next QRS onset, and none may
  # straddle a QRS
  qrs_on <- ann$sample[ann$type == "(" & ann$number == 1]
  for (s in segs) {
    expect_false(any(qrs_on > s[1] & qrs_on < s[2]))
  }
})

test_that("annotation-derived QRS positions improve cancellation", {
  skip_on_ci()

  d <- system.file("extdata", package = "EGM")
  annotated <- extract_f_waves(
    read_wfdb("muse-af", d, annotator = "ecgpuwave"),
    verbose = FALSE
  )
  detected <- extract_f_waves(read_wfdb("muse-af", d), verbose = FALSE)

  expect_lt(
    stats::median(annotated$features$cancellation_residual),
    stats::median(detected$features$cancellation_residual)
  )
})

# Rhythm gating ----

test_that("a regular rhythm is flagged and warned about", {
  skip_on_ci()

  sinus <- read_wfdb("muse-sinus", system.file("extdata", package = "EGM"))
  expect_warning(
    res <- suppressMessages(extract_f_waves(sinus, verbose = TRUE)),
    "does not look like atrial fibrillation"
  )
  expect_false(res$record$af_like)
})

test_that("rhythm_summary distinguishes regular from irregular", {
  fs <- 500
  regular <- as.integer(seq(100, 4900, by = 325))
  irregular <- as.integer(cumsum(c(100, stats::runif(14, 200, 600))))

  expect_false(EGM:::rhythm_summary(regular, fs)$af_like)
  expect_true(EGM:::rhythm_summary(irregular, fs)$af_like)

  # An explicit rhythm overrides inference
  expect_true(EGM:::rhythm_summary(regular, fs, rhythm = "af")$af_like)
})

# Entropy ----

test_that("sample entropy orders noise above a periodic signal", {
  set.seed(123)
  noise <- calculate_sample_entropy(rnorm(500))
  periodic <- calculate_sample_entropy(sin(2 * pi * 6 * seq(0, 10, length.out = 500)))

  expect_type(noise, "double")
  expect_gt(noise, periodic)
  expect_gt(noise, 1)
})

test_that("entropy tolerance defaults to a value that does not collapse it", {
  # A tolerance of 3.5 SD admits nearly every pair of vectors as a match, so the
  # statistic goes to ~0 regardless of the input
  set.seed(123)
  x <- rnorm(500)

  expect_gt(calculate_approximate_entropy(x), 0.5)
  expect_gt(calculate_sample_entropy(x), 0.5)

  # And the old default still behaves the way it did, when asked for explicitly
  expect_lt(calculate_approximate_entropy(x, r = 3.5 * stats::sd(x)), 0.05)
})

test_that("approximate entropy agrees between the R and C++ paths", {
  set.seed(123)
  x <- rnorm(300)
  expect_equal(
    calculate_approximate_entropy(x, implementation = "R"),
    calculate_approximate_entropy(x, implementation = "C++"),
    tolerance = 1e-8
  )
})

test_that("entropy is decimated before it is computed", {
  # O(n^2) at the raw rate is both far slower and mostly a measure of the
  # smoothness of the interpolation between neighbouring samples
  fs <- 500
  x <- sin(2 * pi * 6 * seq(0, 10, by = 1 / fs)) + rnorm(5001, sd = 0.3)

  decimated <- EGM:::decimate_for_entropy(x, fs, 50)
  expect_lt(length(decimated), length(x) / 5)

  # Above the native rate it is a no-op rather than an upsample
  expect_length(EGM:::decimate_for_entropy(x, fs, 5000), length(x))
})

# Utilities ----

test_that("upsampling a bare lead works correctly", {
  signal <- sin(seq(0, 10, length.out = 100))
  upsampled <- change_frequency(signal, from = 10, to = 100)

  # Endpoints are anchored, so (n - 1) source steps become (n - 1) * 10
  expect_length(upsampled, 991)
  expect_true(all(abs(diff(upsampled)) < 0.1))
})

test_that("detect_QRS finds peaks", {
  signal <- sin(seq(0, 10, length.out = 1000)) + rnorm(1000, sd = 0.1)
  peaks <- detect_QRS(signal, frequency = 100)

  expect_type(peaks, "integer")
  expect_true(length(peaks) > 0)
  expect_true(all(diff(peaks) > 10))
})

test_that("QRS positions are refined onto the local energy maximum", {
  fs <- 500
  n <- 5000
  x <- numeric(n)
  true_peaks <- c(500L, 1200L, 2100L, 3000L, 4000L)
  for (p in true_peaks) {
    w <- (p - 40):(p + 40)
    x[w] <- exp(-((w - p) / 6)^2)
  }

  # Detection that lags by a fixed offset, as Pan-Tompkins does
  lagged <- true_peaks + 25L
  refined <- EGM:::refine_qrs_positions(lagged, x, fs)

  expect_equal(refined, true_peaks)
})
