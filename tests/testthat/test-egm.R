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
