test_that("egm class can be made", {

	df <- read_lspro(file = test_path("egm.txt"))

	file <- test_path("egm.txt")
	sig <- read_lspro_signal(file)
	hea <- read_lspro_header(file)

	x <- new_egm(signal = sig, header = hea)
	expect_s3_class(x, "egm")
	expect_s3_class(x, "data.table")
	expect_s3_class(x, "data.frame")
	expect_s3_class(vec_data(x), "data.frame")

})

test_that("egm/signal class definition works", {

	# Class definition
	x <- new_egm()
	expect_length(x, 0)
	expect_true(is_egm(x))
	expect_equal(new_egm(), egm(), ignore_attr = TRUE)

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

	sig <- signal_table(x)
	hea <- list(LABEL = label,
							COLOR = color,
							VOLTAGE = voltage,
							FREQUENCY = frequency)

	s1 <- new_egm(signal = sig, header = hea)
	expect_s3_class(s1, "egm")
	s2 <- egm(sig, hea)
	expect_equal(s1, s2)

	# Basic output data
	expect_output(print(s1), "<Electrical Signal>")

})

