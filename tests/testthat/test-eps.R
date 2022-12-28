test_that("eps/signal class definition works", {

	# Class definition
	x <- new_eps()
	expect_length(x, 0)
	expect_true(is_eps(x))
	expect_equal(new_eps(), eps())

	# Random signal with peaks and troughs, cosine pattern
	x <- cos(2 * pi * (1:1000) * (1:100)/1e+5)

	# Components of vector
	label <- "V1"
	label <- .labels[.labels == "V1"]
	for (i in names(.leads)) {
		if (label %in% .leads[[i]]) {
			source <- i
		}
	}
	color <- "#0000000"
	voltage <- "mV"
	frequency <- as.integer(1000)

	s <- new_eps(
		x = x,
		label = label,
		source = source,
		color = color,
		frequency = frequency,
		voltage = voltage
	)
	expect_s3_class(s, "eps")

	s <- eps(
		x = x,
		label = label,
		source = source,
		color = color,
		frequency = frequency,
		voltage = voltage
	)
	expect_s3_class(s, "eps")

	# Vector-like behaviour should be inherited from {vctrs}
	s1 <- s[1:100]
	s2 <- s[101:200]
	s3 <- c(s1, s2)
	expect_s3_class(s3, "eps")

	# S and X are the same sequence of numbers
	y1 <- c(s[1:100], x[1:100])
	expect_type(y1, "double")
	expect_equal(class(y1), "numeric")
	expect_length(y1, 200)
	y2 <- c(x[1:100], s[1:100])
	expect_equal(y1, y2)

	# Basic output data
	expect_output(print(s), "electrical_signal")

	if (isTRUE(requireNamespace("tibble", quietly = TRUE))) {
		tibble::tibble(s) |>
			print() |>
			expect_output("<eps>")
	}

})

test_that("can get attributes from EPS data", {

	# Random signal with peaks and troughs, cosine pattern
	s <- eps(
		x = cos(2 * pi * (1:1000) * (1:100)/1e+5),
		label = .labels[.labels == "II"],
		source = identify_channel_source("II"),
		color = "#000000",
		frequency = 1000,
		voltage = "mV"
	)
	expect_s3_class(s, "eps")
	expect_equal(get_frequency(s), 1000)
	expect_equal(get_voltage(s), "mV")
	expect_true(get_source(s) %in% .source)
	expect_equal(as.character(get_label(s)), "II")
	expect_equal(get_color(s), "#000000")

	# Vector-like behaviour should be inherited from {vctrs}
	s1 <- s[1:100]
	s2 <- s[900:1000]
	s3 <- c(s1, s2)
	expect_s3_class(s3, "eps")
	expect_equal(get_frequency(s1), get_frequency(s2))
	expect_equal(get_voltage(s1), get_voltage(s2))
	expect_equal(get_source(s1), get_source(s2))
	expect_equal(get_label(s1), get_label(s2))
	expect_equal(get_color(s1), get_color(s2))


})
