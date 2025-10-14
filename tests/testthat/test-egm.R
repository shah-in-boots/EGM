test_that("egm class can be made", {

	df <- read_bard(file = test_path("bard-egm.txt"))

	file <- test_path("bard-egm.txt")
	sig <- read_bard_signal(file)
	hea <- read_bard_header(file)

	x <- new_egm(signal = sig, header = hea)
	expect_s3_class(x, "egm")
	expect_s3_class(x$signal, 'signal_table')
	expect_s3_class(x$signal, "data.table")
	expect_s3_class(x$signal, "data.frame")
	expect_s3_class(x$header, 'header_table')

})

test_that("egm/signal class definition works", {

	# Class definition
	x <- new_egm()
	expect_length(x, 3)
	expect_true(is_egm(x))
	expect_equal(new_egm(), egm())

	# Random signal with peaks and troughs, cosine pattern
	x <- cos(2 * pi * (1:1000) * (1:100)/1e+5)

	# Components of header
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

	sig <- signal_table(V1 = x)
	hea <- header_table(
		label = label,
		color = color,
		ADC_units = voltage,
		frequency = frequency
	)

	s1 <- new_egm(signal = sig, header = hea)
	expect_s3_class(s1, "egm")
	s2 <- egm(sig, hea)
	expect_equal(s1, s2)

	# Basic output data
	expect_output(print(s1), "[Electrical Signal]")

})


test_that('signal can be removed from egm object', {

	skip_on_cran()
	skip_on_ci()

	object <- read_wfdb('ecg', test_path())
	expect_s3_class(object, 'egm')

	# Default = data.frame
	raw <- extract_signal(object)
	expect_s3_class(raw, 'data.frame')
	expect_length(raw, 13)

	# Matrix
	raw <- extract_signal(object, data_format = 'matrix')
	expect_type(raw, 'integer')
	expect_equal(class(raw)[1], 'matrix')
	expect_equal(dim(raw)[1], 5000)
	expect_equal(dim(raw)[2], 12)

})
