test_that("signal class definition works", {

	# Random signal with peaks and troughs, cosine pattern
	x <- cos(2 * pi * (1:1000) * (1:100)/1e+5)

	# Components of vector
	label <- .labels[.labels == "V1"]
	for (i in names(.leads)) {
		if (label %in% .leads[[i]]) {
			source <- i
		}
	}
	color <- "#0000000"
	voltage <- "mV"
	frequency <- as.integer(1000)

	s <- new_signal(
		x = x,
		label = label,
		source = source,
		color = color,
		frequency = frequency,
		voltage = voltage
	)
	expect_s3_class(s, "signal")

	s <- signal(
		x = x,
		label = label,
		source = source,
		color = color,
		frequency = frequency,
		voltage = voltage
	)
	expect_s3_class(s, "signal")

})
