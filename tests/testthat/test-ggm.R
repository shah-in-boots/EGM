test_that("plots can be generated easily", {

	dat <- read_lspro(test_path('sample-egm.txt'))
	header <- dat$header
	signal <- dat$signal


	data <- melt_egm(header = header, signal = signal, format = "lspro")
	expect_s3_class(data, "data.table")

	channels <- c("I", "CS", "HIS d", "HIS m", "RV")
	time_frame <- c(.1, 3)

	ggm(
		data = data,
		channels = channels,
		time_frame = time_frame,
		annotation_channel = "I",
		intervals = TRUE
	)

})
