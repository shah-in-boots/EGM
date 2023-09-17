test_that("ECG segmentation works", {

	rec <- 'ecg'
	dir <- test_path()
	object <- read_wfdb(rec, dir, 'ecgpuwave')

	# Visualize
	g <-
		ggm(object) |>
		draw_boundary_mask()

	# Should create a number of high likelihood sinus beats
	beats <- segment_sinus_beats(object)
	expect_length(beats, 11)
	expect_equal(nrow(beats[[1]]$signal), 263) # Checked the size of the 1st beat

})
