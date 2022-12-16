test_that("eps/signal class definition works", {

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

	# Basic output data
	expect_output(print(s), "electrical_signal")

	if (isTRUE(requireNamespace("tibble", quietly = TRUE))) {
		tibble::tibble(s) |>
			print() |>
			expect_output("<eps>")
	}

})
