test_that("ECG can be segmented", {

	skip_on_cran()
	skip_on_ci()

	rec <- "ecg"
	dir <- test_path()
	object <- read_wfdb(rec, dir, "ecgpuwave")

	# Should create a number of high likelihood sinus beats
	beats <- window_signal(
		object,
		by = "rhythm",
		rhythm_type = "sinus",
		onset_criteria = list(type = "(", number = 0),
		offset_criteria = list(type = ")", number = 2),
		reference_criteria = list(type = "N")
	)
	expect_length(beats, 13)
	expect_equal(nrow(beats[[1]]$signal), 264) # Checked the size of the 1st beat

})
