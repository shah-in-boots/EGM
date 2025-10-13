# File: tests/testthat/test-fwave.R

test_that("extract_f_waves works with default parameters", {

	skip_on_cran()
	skip_on_ci()

	# Object for mock testing
	mock_af <- read_wfdb("muse-af", system.file("extdata", package = "EGM"))

	result <- extract_f_waves(mock_af)

	expect_type(result, "list")
	expect_named(result, names(mock_af$signal)[-1])
	expect_type(result$I, "list")
	expect_named(result$I, "amplitude")

	# Check on alternative characteristics
	result <- extract_f_waves(mock_af, lead = "II", f_characteristics = "approximate_entropy")

})

test_that("extract_f_waves handles invalid input", {
	skip_on_cran()
	skip_on_ci()

	# Object for mock testing
	mock_af <- read_wfdb("muse-af", system.file("extdata", package = "EGM"))

	expect_error(extract_f_waves("not an egm object"), "Input must be of class 'egm'")

	class(mock_af) <- "egm"

	expect_error(
		extract_f_waves(mock_af, lead = "non_existent_lead"),
		"Specified lead not found in the signal data"
	)

	expect_error(
		extract_f_waves(mock_af, f_characteristics = "invalid_characteristic"),
		"Invalid characteristic specified"
	)
})

test_that("upsample_signal works correctly", {
	signal <- sin(seq(0, 10, length.out = 100))
	upsampled <- upsample_signal(signal, original_frequency = 10, new_frequency = 100)
	expect_length(upsampled, 1000)

	# The last beat may be off due to being stationary
	# Checks for "smoothness" of the signal
	expect_true(all(abs(diff(upsampled[1:length(upsampled) - 1])) < 0.1))
})

test_that("detect_QRS finds peaks", {
	signal <- sin(seq(0, 10, length.out = 1000)) + rnorm(1000, sd = 0.1)
	peaks <- detect_QRS(signal, frequency = 100)

	expect_type(peaks, "integer")
	expect_true(length(peaks) > 0)
	expect_true(all(diff(peaks) > 10))  # Check for minimum distance between peaks
})

test_that("QRS removal functions work appropriately", {

	# Skip on WFDB datasets
	skip_on_cran()
	skip_on_ci()

	mock_af <- read_wfdb("muse-af", system.file("extdata", package = "EGM"))

	expect_message({
		out_svd <-
				extract_f_waves(mock_af, lead = "V1", qrs_method = "adaptive_svd")
	})
	expect_type(unlist(out_svd), "double")

	expect_message({
		out_ica <-
				extract_f_waves(mock_af, lead = "V1", qrs_method = "ica")
	})
	expect_type(unlist(out_ica), "double")

})

test_that("calculate_approximate_entropy returns expected range", {
	signal <- rnorm(1000)

	# Trial the R approach
	apen <- calculate_approximate_entropy(signal, implementation = "R")
	expect_type(apen, "double")
	expect_true(apen >= 0 && apen <= 2)  # ApEn is typically between 0 and 2

	# Can also use C++ for speedup as the default
	apen <- calculate_approximate_entropy(signal, implementation = "C++")
	expect_type(apen, "double")
})

test_that("calculate_dominant_frequency returns expected range", {
	signal <- sin(2 * pi * 6 * seq(0, 1, length.out = 1000)) + rnorm(1000, sd = 0.1)
	dom_freq <- calculate_dominant_frequency(signal, frequency = 1000)

	expect_type(dom_freq, "double")
	expect_true(dom_freq >= 4 && dom_freq <= 9)  # Check if within expected range
	expect_equal(dom_freq, 6, tolerance = 0.5)  # Should be close to 6 Hz
})

