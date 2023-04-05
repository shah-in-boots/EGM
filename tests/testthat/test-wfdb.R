# Writing WFDB records ----

test_that("wrsamp works", {

	wd <- getwd()
	# Should not change directory either
	expect_silent(
		write_wfdb(
			file = "sample-egm.txt",
			type = "lspro",
			record = "sample",
			wfdb_path = "/usr/local/bin",
			working_dir = test_path()
		)
	)

	expect_true(file.exists(file.path(test_path(), "sample.hea")))
	expect_equal(wd, getwd())

})

test_that("rdsamp works", {

	record = 300
	wfdb_path = "/usr/local/bin"
	record_dir = test_path()

})

# Write annotation files ----

test_that("ecgpuwave creates annotation file", {

	wd <- getwd()
	# Should not change directory either
	expect_silent(
		detect_beats(
			record = "sample",
			detector = "ecgpuwave",
			wfdb_path = "/usr/local/bin",
			working_dir = test_path()
		)
	)

	expect_true(file.exists(file.path(test_path(), "sample.qrs")))
	expect_equal(wd, getwd())

})
