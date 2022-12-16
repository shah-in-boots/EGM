test_that("egm class can be made", {

	#file <- test_path("sample-egm.txt")
	df <- read_lspro(file = test_path("sample-egm.txt"))

	x <- new_egm(header = get_header(df), signal = get_signal(df))
	expect_s3_class(x, "egm")

	y <- egm(header = get_header(df), signal = get_signal(df))
	expect_s3_class(y, "egm")
	expect_s3_class(y, "data.table")
	expect_s3_class(vec_data(y), "data.frame")


})
