# Writing WFDB records ----

test_that("wrsamp works", {

	# Convert a LSPRO text file into a WFDB compatible format
	wd <- getwd()
	expect_silent(
		rewrite_wfdb(
			file = "sample-egm.txt",
			type = "lspro",
			record = "sample",
			record_dir = test_path(),
			wfdb_path = "/usr/local/bin"
		)
	)

	expect_true(file.exists(file.path(test_path(), "sample.hea")))
	expect_true(file.exists(file.path(test_path(), "sample.dat")))
	expect_equal(wd, getwd())

})

test_that("rdsamp works", {

	# Reads in sample data
	x <- read_wfdb(
		record = "sample",
		record_dir = test_path(),
		wfdb_path = "/usr/local/bin",
		begin = 0,
		units = "physical"
	)

	expect_s3_class(x, "data.frame")

})

