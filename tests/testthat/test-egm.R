test_that("egm class can be made", {

	file <- test_path("sample-egm.txt")
	dt <- read_lspro(file)
	header <- .pull_header(dt)
	signal <- .pull_signal(dt)

	x <- new_egm(header = header, signal = signal)
	expect_s3_class(x, "egm")
	expect_s3_class(.pull_signal(x), "data.table")

	y <- egm(header = header, signal = signal)
	expect_s3_class(y, "egm")
	expect_s3_class(.pull_signal(y), "data.table")

	expect_length(x, 2)
	expect_output(print(x), "<electrogram>")

})
