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

#' Synthesise a multi-lead flutter ECG conducting at a fixed ratio
#'
#' Unlike [simulate_af()] the beats are exactly evenly spaced, which is the
#' point: `rr_ms` sets how many atrial cycles fall in each RR interval, and a
#' whole number of them is what makes the flutter wave phase-locked to the QRS.
#' The atrial wave is returned per lead so that what survives cancellation can be
#' regressed on what went in.
simulate_flutter <- function(
  rr_ms = 400,
  f_hz = 5,
  frequency = 500,
  duration = 10,
  n_leads = 12,
  f_amplitude = 0.15,
  seed = 42
) {
  set.seed(seed)
  n <- as.integer(frequency * duration)
  t <- seq_len(n) / frequency

  # Sawtooth with a harmonic, which is what makes flutter look like flutter
  saw <- 2 * ((t * f_hz) %% 1) - 1
  wave <- -saw - 0.35 * sin(2 * pi * 2 * f_hz * t)

  rr <- as.integer(rr_ms / 1000 * frequency)
  loc <- seq.int(
    as.integer(0.6 * frequency),
    n - as.integer(0.6 * frequency),
    by = rr
  )

  width <- as.integer(0.5 * frequency)
  s <- (seq_len(width) - 1) / frequency
  qrst <- exp(-((s - 0.10)^2) / (2 * 0.010^2)) +
    0.25 * exp(-((s - 0.30)^2) / (2 * 0.045^2))

  # Both the ventricular and the atrial contribution vary by lead, so the
  # cross-lead fit has real spatial structure to work with
  v_gain <- seq(-1.0, 1.4, length.out = n_leads)
  a_gain <- f_amplitude * seq(0.3, 1.3, length.out = n_leads) * c(1, -1)

  atrial <- lapply(seq_len(n_leads), function(j) a_gain[j] * wave)
  signals <- lapply(seq_len(n_leads), function(j) {
    x <- atrial[[j]] + stats::rnorm(n, sd = 0.01)
    for (p in loc) {
      idx <- p - as.integer(0.10 * frequency) + seq_len(width) - 1L
      keep <- idx >= 1 & idx <= n
      x[idx[keep]] <- x[idx[keep]] + v_gain[j] * qrst[keep]
    }
    x
  })
  names(signals) <- names(atrial) <- paste0("L", seq_len(n_leads))

  list(
    signals = signals,
    atrial = atrial,
    qrs_loc = loc,
    frequency = frequency,
    cycles_per_rr = rr_ms / 1000 * f_hz
  )
}

#' Wrap a simulated multi-lead signal as an `ECG`, so the exported entry points
#' can be exercised without a bundled record and therefore without `skip_on_ci()`
as_simulated_ecg <- function(
  sim,
  record_name = "sim",
  leads = c(
    "I", "II", "III", "AVR", "AVL", "AVF",
    "V1", "V2", "V3", "V4", "V5", "V6"
  )
) {
  n <- length(sim$signals[[1]])

  EGM(
    signal = do.call(
      signal_table,
      c(
        list(sample = seq_len(n) - 1L),
        stats::setNames(sim$signals[seq_along(leads)], leads)
      )
    ),
    header = header_table(
      record_name = record_name,
      number_of_channels = length(leads),
      frequency = sim$frequency,
      samples = n,
      label = leads
    )
  )
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
    c("dominant_rate", "harmonic_overlap", "on_harmonic", "cancellation_residual") %in%
      names(result$features)
  ))
  expect_true(all(
    c("n_beats_cancelled", "n_beats_skipped", "n_beats_aberrant") %in%
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
        isTRUE(EGM:::harmonic_flag(df * median_rr / sim$frequency))
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

test_that("each lead is fitted against its own and adjacent templates only", {
  # Stridh and Sornmo's model. Every lead's template would give the fit enough
  # freedom to absorb the atrial signal lying in the window.
  twelve <- c(
    "I", "II", "III", "AVR", "AVL", "AVF",
    "V1", "V2", "V3", "V4", "V5", "V6"
  )
  adj <- EGM:::adjacent_leads(twelve)

  # Precordials chain V1 to V6, and the chain ends are not wrapped
  expect_equal(twelve[adj$V1], c("V1", "V2"))
  expect_equal(twelve[adj$V3], c("V2", "V3", "V4"))
  expect_equal(twelve[adj$V6], c("V5", "V6"))

  # Frontal leads chain in Cabrera order: aVL, I, -aVR, II, aVF, III
  expect_equal(twelve[adj$II], c("AVR", "II", "AVF"))
  expect_equal(twelve[adj$AVL], c("AVL", "I"))
  expect_equal(twelve[adj$III], c("AVF", "III"))

  # The two chains are not joined
  expect_false("V1" %in% twelve[adj$III])

  # A missing lead is stepped over, and a lead that is not a surface lead
  # takes its neighbours in the order given
  some <- c("V1", "V3", "lead3", "CS 1-2")
  adj <- EGM:::adjacent_leads(some)
  expect_equal(some[adj$V1], c("V1", "V3"))
  expect_equal(some[adj$lead3], c("V3", "lead3", "CS 1-2"))
  expect_equal(some[adj[["CS 1-2"]]], c("lead3", "CS 1-2"))
})

test_that("VALIDATION cancellation leaves the atrial signal inside the beat windows", {
  skip_on_ci()

  # Ground truth by construction: a real sinus record, so the QRST morphology
  # and its beat-to-beat variation are real, with a fibrillatory wave of known
  # amplitude added. At 4.25 cycles per RR the wave is not phase-locked to the
  # beats, so what the cancellation removes is over-fitting and nothing else.
  sinus <- read_wfdb("muse-sinus", system.file("extdata", package = "EGM"))
  fs <- frequency(sinus)
  leads <- names(sinus$signal)[-1]
  filtered <- lapply(leads, function(l) {
    EGM:::filter_bandpass(as.numeric(sinus$signal[[l]]), frequency = fs)
  })
  names(filtered) <- leads
  n <- length(filtered[[1]])
  t <- seq_len(n) / fs

  qrs <- EGM:::shared_qrs_positions(NULL, filtered, fs)
  rr <- stats::median(diff(qrs))
  f_hz <- 4.25 * fs / rr
  wave <- 50 * sin(2 * pi * f_hz * t + 0.4 * sin(2 * pi * 0.3 * t))
  signals <- lapply(filtered, function(x) x + wave)

  pre <- round(0.2 * fs)
  post <- round(min(0.5 * fs, max(0.25 * fs, 0.65 * rr)))
  inside <- logical(n)
  for (p in qrs) {
    inside[max(1, p - pre):min(n, p + post)] <- TRUE
  }

  surviving <- function(method) {
    out <- cancel_ventricular_signal(
      signals,
      frequency = fs,
      qrs_loc = qrs,
      method = method
    )
    stats::median(vapply(
      leads,
      function(l) {
        sum(wave[inside] * out$atrial[[l]][inside]) / sum(wave[inside]^2)
      },
      numeric(1)
    ))
  }

  # The all-lead fit this replaced kept 43-62% on this record
  expect_gt(surviving("spatiotemporal"), 0.75)
  expect_gt(surviving("average_beat"), 0.75)
})

test_that("subtraction is tapered at the edge of a singly-covered window", {
  # Dividing the accumulated estimate by the accumulated weight cancels the
  # taper exactly wherever one window covers a sample, so subtraction stepped
  # from the full fitted value to zero at the outer edge of every covered
  # region. The step recurs at the beat period, which is energy on heart-rate
  # harmonics -- inside the band this whole file is trying to measure.
  sim <- simulate_af()
  res <- cancel_ventricular_signal(
    sim$signals,
    frequency = sim$frequency,
    qrs_loc = sim$qrs_loc
  )

  for (l in names(sim$signals)) {
    ventricular <- sim$signals[[l]] - res$atrial[[l]]
    covered <- which(ventricular != 0)

    # Nothing is subtracted outside the covered region, so the estimate has to
    # arrive and leave at zero rather than switching on at its fitted value
    expect_lt(abs(ventricular[min(covered)]), 0.05 * max(abs(ventricular)))
    expect_lt(abs(ventricular[max(covered)]), 0.05 * max(abs(ventricular)))
  }
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

test_that("cancellation absorbs flutter conducting at a fixed ratio", {

  # The one case where the method's own assumption works against it: a template
  # built by stacking beats keeps whatever repeats at a fixed phase to the QRS,
  # and in fixed-ratio flutter the atrial wave does. Ground truth is known here,
  # so how much of it survives can be measured rather than argued about.
  surviving <- function(rr_ms, f_hz = 5, frequency = 500, duration = 10) {
    sim <- simulate_flutter(
      rr_ms = rr_ms,
      f_hz = f_hz,
      frequency = frequency,
      duration = duration
    )
    out <- cancel_ventricular_signal(
      sim$signals,
      frequency = frequency,
      qrs_loc = sim$qrs_loc
    )
    # Regress each recovered lead on the atrial wave that went in: 1 is fully
    # preserved, 0 fully removed
    stats::median(vapply(
      names(sim$signals),
      function(l) {
        truth <- sim$atrial[[l]]
        sum(truth * out$atrial[[l]]) / sum(truth^2)
      },
      numeric(1)
    ))
  }

  # 2:1 and 4:1 put a whole number of atrial cycles in every RR interval, so the
  # flutter wave sits at the same phase in every beat and joins the template
  expect_lt(surviving(rr_ms = 400), 0.25) # 2 cycles per RR
  expect_lt(surviving(rr_ms = 800), 0.35) # 4 cycles per RR

  # Half a cycle out, consecutive beats are in antiphase and the median template
  # holds no atrial signal to subtract
  expect_gt(surviving(rr_ms = 500), 0.6) # 2.5 cycles per RR
})

test_that("an absorbed flutter wave still reports a small cancellation residual", {

  # The trap worth a test of its own. The fit is *better* for having taken the
  # atrial wave with it, so nothing in the residual says the signal is gone; the
  # regularity of the ventricular response is what says it.
  locked <- simulate_flutter(rr_ms = 400)
  free <- simulate_flutter(rr_ms = 500)

  residual <- function(sim) {
    out <- cancel_ventricular_signal(
      sim$signals,
      frequency = 500,
      qrs_loc = sim$qrs_loc
    )
    stats::median(vapply(
      names(sim$signals),
      function(l) {
        stats::sd(out$atrial[[l]]) / stats::sd(sim$signals[[l]])
      },
      numeric(1)
    ))
  }

  expect_lt(residual(locked), residual(free))
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

test_that("the Welch spectrum refuses a signal with holes in it", {
  # Dropping non-finite samples closes the gap they leave and shifts everything
  # after it in time, which moves the spectrum rather than losing a little of it
  x <- sin(2 * pi * 6 * seq(0, 10, by = 1 / 500))
  x[c(100, 2000)] <- NA_real_

  expect_error(calculate_welch_spectrum(x, frequency = 500), "non-finite")
})

test_that("the organisation index does not depend on the rate it is measuring", {
  # The harmonic window is centred on twice the dominant frequency, so a band
  # that stops short of it drops the harmonic for the fastest rates the
  # estimator searches. The index then moves with the rate rather than with the
  # morphology, and fast AF reads as disorganised for an arithmetic reason.
  fs <- 500
  t <- seq(0, 10, by = 1 / fs)
  set.seed(4)
  tone <- function(f) {
    sin(2 * pi * f * t) + sin(2 * pi * 2 * f * t) + stats::rnorm(length(t), sd = 4)
  }
  fast <- tone(9)
  slow <- tone(5)

  oi <- function(x, df, band) {
    calculate_organization_index(
      x, fs, dominant_frequency = df, n_harmonics = 1, band = band
    )
  }

  # Identical construction at both rates, so the index has to agree
  expect_equal(oi(fast, 9, c(2.5, 21)), oi(slow, 5, c(2.5, 21)), tolerance = 0.02)

  # The band that stops at 15 Hz does not, because 2 * 9 Hz falls outside it
  expect_gt(abs(oi(fast, 9, c(2.5, 15)) - oi(slow, 5, c(2.5, 15))), 0.1)
})

test_that("the organisation index is summed over the peaks the literature uses", {
  # Everett takes the dominant peak with its first four harmonics, and An's
  # surface implementation does the same over a 0-50 Hz denominator. Summing
  # fewer terms returns a systematically smaller number that no published
  # threshold applies to.
  expect_equal(eval(formals(calculate_organization_index)$n_harmonics), 4L)

  band <- eval(formals(calculate_organization_index)$band)
  half_width <- eval(formals(calculate_organization_index)$half_width)
  expect_gte(band[2], 50)

  # And the band must at minimum hold the first harmonic across the 4-10 Hz
  # range `extract_f_waves()` searches
  expect_gte(band[2], 2 * 10 + 1.5 * half_width)

  # More harmonic windows can only add power to the numerator
  fs <- 500
  x <- sin(2 * pi * 6 * seq(0, 10, by = 1 / fs)) + stats::rnorm(5001, sd = 1)
  one <- calculate_organization_index(x, fs, dominant_frequency = 6, n_harmonics = 1)
  four <- calculate_organization_index(x, fs, dominant_frequency = 6, n_harmonics = 4)
  expect_gte(four, one)
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
    raw_signal = sim$signals[[1]],
    frequency = sim$frequency,
    qrs_loc = sim$qrs_loc,
    annotation = NULL,
    window = "tq"
  )
  all_amp <- EGM:::amplitude_features(
    res$atrial[[1]],
    raw_signal = sim$signals[[1]],
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
  # The ratio is the Alcaraz-Rieta group's normalised amplitude: RMS over the
  # QRS, not the length-confounded peak-to-peak
  expect_equal(amp$f_ratio, amp$f_amplitude_rms / amp$qrs_amplitude)
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
  labelled <- EGM:::label_waves(ann)
  qrs_on <- labelled$sample[labelled$type == "(" & labelled$wave %in% "QRS"]
  for (s in segs) {
    expect_false(any(qrs_on > s[1] & qrs_on < s[2]))
  }
})

test_that("f_amplitude is the RMS, and one bad segment does not carry it", {
  # Peak-to-peak is a maximum over its segment, so it grows with the segment's
  # length, and TQ segment length is set by the RR interval. Both amplitudes are
  # still returned; the default is the one without that confound.
  sim <- simulate_af()
  object <- as_simulated_ecg(sim)
  res <- suppressMessages(suppressWarnings(
    extract_f_waves(object, qrs_loc = sim$qrs_loc, verbose = FALSE)
  ))

  expect_equal(res$features$f_amplitude, res$features$f_amplitude_rms)
  expect_false(isTRUE(all.equal(
    res$features$f_amplitude, res$features$f_amplitude_p2p
  )))

  # `normalize = "qrs"` still points it at the ratio
  ratio <- suppressMessages(suppressWarnings(
    extract_f_waves(object, qrs_loc = sim$qrs_loc, normalize = "qrs", verbose = FALSE)
  ))
  expect_equal(ratio$features$f_amplitude, ratio$features$f_ratio)

})

test_that("both amplitudes are reduced by the median across segments", {
  # Pooling the root-mean-square over every TQ sample at once would let one
  # noisy segment carry the lead, and the segments are not exchangeable: they
  # vary in length with the RR interval. Nine quiet segments and one loud one.
  set.seed(21)
  fs <- 500
  n <- 11000
  qrs <- as.integer(seq(1, n, length.out = 11))
  segs <- EGM:::tq_segments(n, fs, qrs, NULL)
  expect_length(segs, 10)

  x <- stats::rnorm(n, sd = 1)
  loud <- segs[[5]]
  x[loud[1]:loud[2]] <- stats::rnorm(loud[2] - loud[1] + 1L, sd = 30)

  amp <- EGM:::amplitude_features(
    x,
    raw_signal = x,
    frequency = fs,
    qrs_loc = qrs,
    annotation = NULL,
    window = "tq"
  )

  # One segment thirty times louder than the other nine moves a pooled figure by
  # about tenfold and a median across segments hardly at all
  pooled <- sqrt(mean(x[unlist(lapply(segs, function(s) s[1]:s[2]))]^2))
  expect_gt(pooled, 8)
  expect_lt(amp$f_amplitude_rms, 1.2)
  expect_lt(amp$f_amplitude_p2p, 10)
})

test_that("several usable annotators are refused rather than chosen between", {
  # The `channel` column catches one per-lead convention. The other -- a file
  # per lead, as LUDB writes -- leaves `chan` at 0 in every file, so the tables
  # are indistinguishable by channel and taking the first silently takes a lead.
  # Which one changes the beat positions, the TQ boundaries and the amplitudes.
  one <- get_annotation(read_wfdb("ecg-sinus", test_path(), "ann"))
  expect_s3_class(EGM:::resolve_annotation(list(ann = one)), "data.frame")

  expect_error(
    EGM:::resolve_annotation(list(i = one, ii = one)),
    "2 usable annotators"
  )
})

test_that("channel 0 rides along as the global channel", {
  # Every table counts signals from 1 - a file that counted from 0 is renumbered
  # as it is read - so 0 is never a lead, and keeping it beside the requested
  # one cannot pool two leads' fiducials.
  ann <- get_annotation(read_wfdb("ecg-sinus", test_path(), "ann"))
  ann$channel[seq_len(20)] <- 0L

  record <- EGM(
    signal = signal_table(sample = 0:9, I = rnorm(10)),
    header = header_table(record_name = "x", number_of_channels = 1, frequency = 500),
    annotation = list(ann = ann)
  )
  kept <- EGM:::resolve_fwave_annotation(record, channel = 2)
  expect_setequal(unique(kept$channel), c(0L, 2L))
})

test_that("TQ segments are found when the annotator leaves `number` at zero", {
  # Wave identity is the peak symbol each bracket pair encloses, never the WFDB
  # `number` column, which most annotators never populate. Reading `number` sent
  # every such record silently to the fixed exclusion window while still
  # reporting `amplitude_window = "tq"`. The bundled `ecg-sinus.ann` is one.
  ann <- get_annotation(read_wfdb("ecg-sinus", test_path(), "ann"))
  expect_true(all(ann$number == 0L))

  one_lead <- ann[ann$channel %in% c(1L, 0L), ]
  segs <- EGM:::tq_segments(5000, 500, qrs_loc = NULL, annotation = one_lead)

  expect_gt(length(segs), 5)
  expect_true(all(vapply(segs, function(s) s[2] > s[1], logical(1))))
})

test_that("TQ segments are resolved to one lead", {
  # Pooled across a per-lead annotator every segment appears once per lead, so
  # the segments overlap and their total length exceeds the record. It shows up
  # as a `tq_fraction` greater than one, which is impossible by construction.
  object <- as_ECG(read_wfdb("ecg-sinus", test_path(), "ann"))
  ann <- get_annotation(object)
  expect_gt(length(unique(ann$channel)), 1L)

  pooled <- EGM:::tq_segments(5000, 500, qrs_loc = NULL, annotation = ann)
  resolved <- EGM:::tq_segments(
    5000,
    500,
    qrs_loc = NULL,
    annotation = EGM:::resolve_fwave_annotation(object, channel = 2)
  )

  coverage <- function(segs) sum(vapply(segs, function(s) s[2] - s[1] + 1, numeric(1)))
  expect_gt(coverage(pooled), 5000)
  expect_lte(coverage(resolved), 5000)

  # And the same guarantee through the exported entry point
  result <- suppressWarnings(extract_f_waves(object, channel = 2, verbose = FALSE))
  expect_true(all(result$features$tq_fraction <= 1))
})

test_that("the QRS excursion is measured before the bandpass", {
  # The passband stops at 30 Hz and the QRS carries energy above it, so a
  # filtered excursion is small by a factor that depends on the QRS width. That
  # factor would land in `f_ratio`, whose whole purpose is comparability
  # between patients.
  sim <- simulate_af()
  filtered <- EGM:::filter_bandpass(sim$signals[[1]], sim$frequency)

  raw_amp <- EGM:::qrs_excursion(sim$signals[[1]], sim$qrs_loc, sim$frequency)
  filtered_amp <- EGM:::qrs_excursion(filtered, sim$qrs_loc, sim$frequency)

  expect_gt(raw_amp, filtered_amp)

  amp <- EGM:::amplitude_features(
    sim$signals[[1]] - filtered,
    raw_signal = sim$signals[[1]],
    frequency = sim$frequency,
    qrs_loc = sim$qrs_loc,
    annotation = NULL,
    window = "tq"
  )
  expect_equal(amp$qrs_amplitude, raw_amp)
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

  # Sinus trips both gates: nothing fibrillatory to measure, and regular enough
  # that cancellation would have taken it anyway
  expect_warning(
    expect_warning(
      res <- suppressMessages(extract_f_waves(sinus, verbose = TRUE)),
      "does not look like atrial fibrillation"
    ),
    "ventricular response is regular"
  )
  expect_false(res$record$af_like)
  expect_true(res$record$rr_regular)
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

test_that("rr_regular flags the rhythm the canceller cannot be trusted on", {
  fs <- 500
  regular <- as.integer(seq(100, 4900, by = 325))
  irregular <- as.integer(cumsum(c(100, stats::runif(14, 200, 600))))

  expect_true(EGM:::rhythm_summary(regular, fs)$rr_regular)
  expect_false(EGM:::rhythm_summary(irregular, fs)$rr_regular)

  # `af_like` answers whether there is fibrillatory activity to measure;
  # `rr_regular` answers whether cancellation will leave it there. Labelling a
  # record must not silence the second - fixed-ratio flutter is exactly the case
  # it exists for, and it is exactly the case someone labels "flutter".
  labelled <- EGM:::rhythm_summary(regular, fs, rhythm = "flutter")
  expect_true(labelled$af_like)
  expect_true(labelled$rr_regular)
})

test_that("a regular ventricular response warns about what cancellation takes", {
  sim <- simulate_flutter(rr_ms = 400)
  object <- as_simulated_ecg(sim, "flutter")

  # `rhythm = "flutter"` settles the other warning, so this one stands alone -
  # which is the behaviour under test: a label must not silence it
  expect_warning(
    res <- suppressMessages(
      extract_f_waves(object, qrs_loc = sim$qrs_loc, rhythm = "flutter")
    ),
    "ventricular response is regular"
  )
  expect_true(res$record$rr_regular)
})

test_that("the rhythm warnings are not silenced by verbose = FALSE", {
  # `verbose` reports progress. A batch run turns it off, and a batch run is
  # where an unnoticed flutter or sinus record does the most damage, so the
  # warnings have to survive it. They used to be raised inside `if (verbose)`.
  sim <- simulate_flutter(rr_ms = 400)
  object <- as_simulated_ecg(sim, "flutter")

  raised <- character()
  withCallingHandlers(
    suppressMessages(
      extract_f_waves(object, qrs_loc = sim$qrs_loc, verbose = FALSE)
    ),
    warning = function(w) {
      raised <<- c(raised, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_true(any(grepl("does not look like atrial fibrillation", raised)))
  # The regular-response warning is the one trap 5 rests on
  expect_true(any(grepl("ventricular response is regular", raised)))
})

test_that("asking for organisation returns organisation", {
  # `organization_index` used to be assigned only inside the
  # `"dominant_frequency"` branch, so asking for it alone returned a table with
  # no such column and no complaint -- in a function that validates
  # `f_characteristics` against a whitelist a few lines earlier.
  sim <- simulate_af()
  object <- as_simulated_ecg(sim)

  res <- suppressMessages(suppressWarnings(extract_f_waves(
    object,
    qrs_loc = sim$qrs_loc,
    f_characteristics = "organization",
    verbose = FALSE
  )))

  expect_true("organization_index" %in% names(res$features))
  expect_true(all(is.finite(res$features$organization_index)))
})

test_that("the dominant frequency is read from V1 and shared by every lead", {
  sim <- simulate_af()
  object <- as_simulated_ecg(sim)

  v1 <- suppressMessages(suppressWarnings(
    extract_f_waves(object, qrs_loc = sim$qrs_loc, verbose = FALSE)
  ))
  expect_length(unique(v1$features$dominant_rate), 1L)

  # And it is V1's own estimate, not a pooled one
  own <- suppressMessages(suppressWarnings(extract_f_waves(
    object,
    qrs_loc = sim$qrs_loc,
    spectrum = "lead",
    keep_signal = TRUE,
    verbose = FALSE
  )))
  expect_equal(
    v1$features$dominant_rate[1],
    own$features$dominant_rate[own$features$lead == "V1"]
  )
  expect_equal(
    own$features$dominant_rate,
    vapply(
      own$features$lead,
      function(l) 60 * calculate_dominant_frequency(own$signal[[l]], sim$frequency),
      numeric(1)
    ),
    ignore_attr = TRUE
  )
})

test_that("a record without V1 is refused only where the frequency is needed", {
  sim <- simulate_af()
  limb <- as_simulated_ecg(sim, leads = c("I", "II", "III", "AVR", "AVL", "AVF"))

  expect_error(
    suppressMessages(suppressWarnings(
      extract_f_waves(limb, qrs_loc = sim$qrs_loc, verbose = FALSE)
    )),
    "does not carry"
  )

  # Amplitude alone consults no spectrum, so no V1 is needed for it
  amp <- suppressMessages(suppressWarnings(extract_f_waves(
    limb,
    qrs_loc = sim$qrs_loc,
    f_characteristics = "amplitude",
    amplitude_window = "all",
    verbose = FALSE
  )))
  expect_true(all(is.finite(amp$features$f_amplitude)))

  # And the other spectra do not need it either
  pooled <- suppressMessages(suppressWarnings(extract_f_waves(
    limb,
    qrs_loc = sim$qrs_loc,
    spectrum = "pooled",
    verbose = FALSE
  )))
  expect_length(unique(pooled$features$dominant_rate), 1L)
})

test_that("pooled_spectrum still works, and says so once", {
  sim <- simulate_af()
  object <- as_simulated_ecg(sim)
  suppressWarnings(
    rm("extract_f_waves.pooled_spectrum", envir = EGM:::deprecation_state)
  )

  # Every warning but the one under test is muffled; the rhythm warnings are
  # tested elsewhere
  only_superseded <- function(expr) {
    withCallingHandlers(
      expr,
      warning = function(w) {
        if (!grepl("superseded", conditionMessage(w))) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }

  old <- NULL
  only_superseded(expect_warning(
    old <- suppressMessages(extract_f_waves(
      object,
      qrs_loc = sim$qrs_loc,
      pooled_spectrum = FALSE,
      keep_signal = TRUE,
      verbose = FALSE
    )),
    "superseded"
  ))
  # `FALSE` maps to `"lead"`: each lead its own estimate
  expect_equal(
    old$features$dominant_rate,
    vapply(
      old$features$lead,
      function(l) 60 * calculate_dominant_frequency(old$signal[[l]], sim$frequency),
      numeric(1)
    ),
    ignore_attr = TRUE
  )

  # Second use in the session says nothing
  expect_no_warning(only_superseded(suppressMessages(extract_f_waves(
    object,
    qrs_loc = sim$qrs_loc,
    pooled_spectrum = TRUE,
    verbose = FALSE
  ))))

  expect_error(
    extract_f_waves(object, spectrum = "lead", pooled_spectrum = TRUE),
    "not both"
  )
})

test_that("an annotation is demanded only where something will read it", {
  # `qrs_loc` supplied and amplitude measured over the whole record means
  # nothing consults the annotations, so requiring a `channel` for them would be
  # a guard on nothing
  object <- as_ECG(read_wfdb("ecg-sinus", test_path(), "ann"))
  qrs <- EGM:::locate_features(get_annotation(object), "N", 2L)

  expect_error(
    extract_f_waves(object, qrs_loc = qrs, verbose = FALSE),
    "channel"
  )
  expect_no_error(suppressWarnings(extract_f_waves(
    object,
    qrs_loc = qrs,
    amplitude_window = "all",
    f_characteristics = "sample_entropy",
    verbose = FALSE
  )))
})

test_that("printing a record too short to summarise does not error", {
  # `rhythm_summary()` returns `af_like = NA` below three beats, and the print
  # method branched on it directly
  sim <- simulate_af()
  object <- as_simulated_ecg(sim)
  res <- suppressMessages(suppressWarnings(
    extract_f_waves(object, qrs_loc = sim$qrs_loc[1:2], verbose = FALSE)
  ))

  expect_true(is.na(res$record$af_like))
  expect_output(print(res), "NOT AF-like")
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

test_that("approximate entropy matches a direct implementation of Pincus", {
  # Transcribed straight from the definition, with no shared code, so that the
  # C++ is checked against the paper rather than against itself
  apen_reference <- function(x, m, r) {
    N <- length(x)
    embed_matrix <- function(k) {
      matrix(vapply(seq_len(k), function(i) x[i:(N - k + i)], numeric(N - k + 1)),
        ncol = k
      )
    }
    correlation_integral <- function(mat) {
      rows <- nrow(mat)
      count <- vapply(
        seq_len(rows),
        function(i) {
          sum(apply(abs(mat - rep(mat[i, ], each = rows)), 1, max) <= r)
        },
        numeric(1)
      )
      sum(log(count / rows)) / rows
    }
    correlation_integral(embed_matrix(m)) - correlation_integral(embed_matrix(m + 1))
  }

  set.seed(123)
  x <- rnorm(300)
  expect_equal(
    calculate_approximate_entropy(x, m = 2, r = 0.2 * stats::sd(x)),
    apen_reference(x, m = 2, r = 0.2 * stats::sd(x)),
    tolerance = 1e-8
  )
})

test_that("a series with holes in it is refused, spectrum and entropy alike", {
  # Dropping the non-finite samples joins two stretches that were not adjacent.
  # For a spectrum that moves the whole time axis; for an entropy it is worse,
  # since the statistic is the relationship between neighbouring samples and the
  # spliced pairs get compared as though they were contiguous.
  x <- sin(2 * pi * 6 * seq(0, 10, by = 1 / 500))
  x[c(100, 2000)] <- NA_real_

  expect_error(calculate_welch_spectrum(x, frequency = 500), "non-finite")
  expect_error(calculate_sample_entropy(x), "non-finite")
  expect_error(calculate_approximate_entropy(x), "non-finite")
})

test_that("entropy decimates to the rate its parameters were tuned at", {
  # Alcaraz et al. tuned m, r and the rate for AF organisation specifically and
  # found classification degraded below 256 Hz. The default used to be 50, which
  # was chosen from the fibrillatory bandwidth and the O(n^2) cost instead.
  expect_equal(eval(formals(extract_f_waves)$entropy_rate), 256)
  expect_equal(eval(formals(analyze_atrial_signal)$entropy_rate), 256)

  fs <- 500
  x <- sin(2 * pi * 6 * seq(0, 10, by = 1 / fs)) + rnorm(5001, sd = 0.3)
  expect_equal(length(EGM:::decimate_for_entropy(x, fs, 256)), 2561)
})

test_that("the main atrial wave is the band around the dominant frequency", {
  # Alcaraz and Rieta's parameters were tuned on this, not on a broadband
  # signal. Narrowing a noisy 6 Hz wave to the band around 6 Hz leaves
  # something far more regular, so the entropy falls.
  set.seed(7)
  fs <- 500
  x <- sin(2 * pi * 6 * seq(0, 10, by = 1 / fs)) + rnorm(5001, sd = 0.5)
  maw <- EGM:::main_atrial_wave(x, fs, dominant_frequency = 6, bandwidth = 3)
  expect_length(maw, length(x))

  psd <- calculate_welch_spectrum(maw, fs)
  outside <- psd$freq < 4 | psd$freq > 8
  expect_lt(sum(psd$spec[outside]), 0.05 * sum(psd$spec))

  decimated <- function(v) EGM:::decimate_for_entropy(v, fs, 256)
  expect_lt(
    calculate_sample_entropy(decimated(maw)),
    calculate_sample_entropy(decimated(x))
  )

  # No dominant frequency, no main atrial wave; and a band that reaches below
  # 0 Hz is refused rather than clipped into a low-pass
  expect_null(EGM:::main_atrial_wave(x, fs, dominant_frequency = NA_real_))
  expect_error(
    EGM:::main_atrial_wave(x, fs, dominant_frequency = 1, bandwidth = 3),
    "outside"
  )
})

test_that("entropy_input reaches the entropy, and is NA without a frequency", {
  sim <- simulate_af()
  broad <- analyze_atrial_signal(
    sim$atrial,
    sim$frequency,
    characteristics = "sample_entropy"
  )
  narrow <- analyze_atrial_signal(
    sim$atrial,
    sim$frequency,
    characteristics = "sample_entropy",
    entropy_input = "main_atrial_wave"
  )
  expect_false(isTRUE(all.equal(broad$sample_entropy, narrow$sample_entropy)))

  none <- analyze_atrial_signal(
    sim$atrial,
    sim$frequency,
    characteristics = "sample_entropy",
    entropy_input = "main_atrial_wave",
    dominant_frequency = NA_real_
  )
  expect_true(is.na(none$sample_entropy))

  object <- as_simulated_ecg(sim)
  res <- suppressMessages(suppressWarnings(extract_f_waves(
    object,
    qrs_loc = sim$qrs_loc,
    entropy_input = "main_atrial_wave",
    verbose = FALSE
  )))
  expect_true(all(is.finite(res$features$sample_entropy)))
  expect_error(extract_f_waves(object, entropy_bandwidth = -1), "positive")

  # The tolerance reaches the entropy too: Alcaraz and Rieta selected 0.25 on
  # the main atrial wave, and the default is 0.2
  loose <- analyze_atrial_signal(
    sim$atrial,
    sim$frequency,
    characteristics = "sample_entropy",
    entropy_tolerance = 0.25
  )
  expect_false(isTRUE(all.equal(broad$sample_entropy, loose$sample_entropy)))
  expect_error(extract_f_waves(object, entropy_tolerance = 0), "positive")
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

test_that("VALIDATION detected positions land on the beats that are there", {
  # The local-maximum test compared the sample against its neighbour twice in
  # the same direction, so it flagged every sample on a rising limb rather than
  # the peak. With the refractory loop taking the first of each cluster, what
  # came back was a threshold crossing, offset from the beat by an amount that
  # moves with the signal. Nothing asserted otherwise: the old tests checked
  # only that some peaks came back and that they were spaced apart.
  fs <- 500
  n <- fs * 10
  x <- numeric(n)
  true_peaks <- seq(fs, n - fs, by = as.integer(0.8 * fs))
  for (p in true_peaks) {
    w <- (p - 25):(p + 25)
    x[w] <- x[w] + exp(-((w - p) / 8)^2)
  }
  set.seed(11)
  x <- x + stats::rnorm(n, sd = 0.01)

  detected <- detect_QRS(x, fs)
  expect_length(detected, length(true_peaks))

  # The integration window is causal, so a detection lags its beat and can never
  # precede it. Reporting a rising-edge crossing is what made it precede: on
  # this record the old test came back 8 samples early on every beat.
  expect_true(all(detected >= true_peaks))
  expect_true(all(detected - true_peaks <= as.integer(0.150 * fs)))

  # And refinement has to bring them back onto the beat, which needs a search
  # window wider than the integration lag it is undoing
  refined <- EGM:::refine_qrs_positions(detected, x, fs)
  expect_true(all(abs(refined - true_peaks) <= as.integer(0.02 * fs)))
})

test_that("the refinement window is wider than the lag it has to undo", {
  # detect_QRS() reports the peak of a 150 ms integration window, so it lags the
  # beat by about 75 ms. A search window narrower than that cannot reach the
  # beat and latches onto whatever else is in range; it used to be 60 ms.
  search_ms <- eval(formals(EGM:::refine_qrs_positions)$search_ms)
  integration_ms <- 1000 * eval(formals(detect_QRS)$window_size)

  expect_gt(search_ms, integration_ms / 2)
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

test_that("beat annotations are not pooled across a per-lead annotator", {
  object <- as_ECG(read_wfdb("ecg-sinus", test_path(), "ann"))

  # Twelve leads' worth of QRS annotations would be counted as twelve times as
  # many beats, and every rhythm measure derived from them follows
  expect_error(
    extract_f_waves(object, verbose = FALSE),
    "needs a guiding `channel`"
  )

  guided <- suppressWarnings(
    extract_f_waves(object, channel = 2, verbose = FALSE)
  )
  qrs <- EGM:::locate_features(get_annotation(object), "N", 2L)
  expect_equal(guided$record$n_beats, length(qrs))
  expect_gt(guided$record$heart_rate, 20)
  expect_lt(guided$record$heart_rate, 300)
})

test_that("an implausible heart rate is reported rather than returned", {
  # Beats one sample apart at 500 Hz is 30,000 bpm: a counting error, most often
  # annotations pooled across leads, and invisible in every feature downstream
  expect_warning(
    EGM:::rhythm_summary(c(100L, 101L, 102L, 103L), 500),
    "Implausible heart rate"
  )
  expect_silent(EGM:::rhythm_summary(c(0L, 500L, 1000L, 1500L), 500))
})
