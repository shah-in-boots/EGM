test_that("lspro data can be read in", {

	file <- test_path("egm.txt")
	x <- read_lspro(file)
	expect_s3_class(x, "egm")

	hea <- read_lspro_header(file)
	sig <- read_lspro_signal(file)

	expect_type(hea, "list")
	expect_s3_class(sig, "data.table")

})
