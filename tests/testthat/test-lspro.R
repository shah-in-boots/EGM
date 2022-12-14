test_that("lspro data can be read in", {

	file <- test_path("sample-egm.txt")
	dt <- read_lspro(file)
	expect_type(dt, "list")
	expect_length(dt, 3)
	expect_s3_class(dt$signal, "data.table")
	expect_s3_class(dt$header, "data.table")

})
