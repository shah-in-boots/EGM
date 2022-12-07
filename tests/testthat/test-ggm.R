test_that("plots can be generated easily", {

	data <- read_lspro(test_path('sample-egm.txt'))
	expect_s3_class(data, "egm")

	channels <- c("I", "CS", "HIS D", "HIS M", "RV")
	time_frame <- c(.1, 3)

	ggm(
		data = data,
		channels = channels,
		time_frame = time_frame
	)

})
