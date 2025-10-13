test_that("bard data can be read in", {

	file <- test_path("bard-egm.txt")
	x <- read_bard(file)
	expect_s3_class(x, "egm")

	hea <- read_bard_header(file)
	sig <- read_bard_signal(file)

	expect_type(hea, "list")
	expect_s3_class(sig, "data.table")

})
