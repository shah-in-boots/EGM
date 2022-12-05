test_that("lspro data can be read in", {

	dt <- read_lspro(test_path("sample-egm.txt"))
	expect_type(dt, "list")
	expect_length(dt, 2)
	expect_s3_class(dt$signal, "data.table")
	expect_s3_class(dt$header, "data.table")

})
