test_that("ecgpuwave creates annotation file", {

	wd <- getwd()
	# Should not change directory either
	expect_silent(
		detect_surface_beats(
			record = "sample",
			detector = "ecgpuwave",
			record_dir = test_path(),
			wfdb_path = "/usr/local/bin"
		)
	)

	expect_true(file.exists(file.path(test_path(), "sample.qrs")))
	expect_equal(wd, getwd())

})
