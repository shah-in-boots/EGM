test_that("plots can be generated easily", {

	dat <- read_lspro(test_path('sample-egm.txt'))
	header <- dat$header
	signal <- dat$signal


	data <- melt_egm(header = header, signal = signal, format = "lspro")
	expect_s3_class(data, "data.table")

	channels <- c("I", "CS", "HIS d", "HIS m", "RV")
	frequency <- 1000
	time_frame <- c(.1, 3)

	gg_egm(
		data = data,
		frequency = frequency,
		channels = channels,
		time_frame = time_frame
	)

})
