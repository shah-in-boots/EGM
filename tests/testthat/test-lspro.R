test_that("lspro data can be read in", {

	file <- test_path("sample-egm.txt")
	x <- read_lspro(file)
	expect_s3_class(x, "egm")
	expect_s3_class(x[[1]], "eps")

})
