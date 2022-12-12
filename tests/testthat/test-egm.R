test_that("egm class can be made", {

	#file <- test_path("sample-egm.txt")
	dt <- read_lspro(file = test_path("sample-egm.txt"))
	header <- .pull_header(dt)
	signal <- .pull_signal(dt)
	channels <- .pull_channels(dt)

	x <- new_egm(header = header, channels = channels, signal = signal)
	expect_s3_class(x, "egm")
	expect_s3_class(.pull_signal(x), "data.table")

	y <- egm(header = header, channels = channels, signal = signal)
	expect_s3_class(y, "egm")
	expect_s3_class(.pull_signal(y), "data.table")

	expect_length(x, 3)
	# Should actually be just "1" EGM recording TBH
	expect_output(print(x), "<electrogram\\[3\\]>")

})
