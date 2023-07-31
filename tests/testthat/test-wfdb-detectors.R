test_that("ecgpuwave creates annotation file", {

	wd <- getwd()
	# Should not change directory either
	expect_silent(
		detect_surface_beats(
			record = "300",
			detector = "ecgpuwave",
			record_dir = test_path()
		)
	)

	expect_true(file.exists(file.path(test_path(), "300.ecgpuwave")))
	expect_equal(wd, getwd())

})
