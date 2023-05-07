# Writing WFDB records ----

test_that("can convert lspro to wfdb with wrsamp", {

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

test_that("R data objects can be converted or written to WFDB format", {

	file <- test_path("sample-egm.txt")
	sig <- read_lspro_signal(file)
	hea <- read_lspro_header(file)

	write_wfdb(
		data = sig,
		type = "lspro",
		record = "sample",
		record_dir = test_path(),
		wfdb_path = "/usr/local/bin",
		header = hea
	)

	headerFile <- readLines(test_path("sample.hea"))
	expect_length(headerFile, 19)
	expect_output(print(headerFile[1]), "sample 14")

})

test_that("rdsamp can read in WFDB formatted files", {

	# Reads in sample data
	x <- read_wfdb(
		record = "sample",
		record_dir = test_path(),
		wfdb_path = "/usr/local/bin",
		begin = 0,
		units = "digital"
	)

	expect_s3_class(x, "data.frame")

})

