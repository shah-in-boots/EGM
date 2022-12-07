test_that("egm class can be made", {

	file <- test_path("sample-egm.txt")
	dt <- read_lspro(file)
	header <- dt$header
	signal <- dt$signal

	x <- new_egm(header = header, signal = signal)
	expect_s3_class(x, "egm")
	expect_s3_class(x$signal, "data.table")

	x <- egm(header = header, signal = signal)
	expect_s3_class(x, "egm")
	expect_s3_class(x$signal, "data.table")

})
