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


test_that('Padding works', {

	rec <- 'ecg'
	dir <- test_path()
	object <- read_wfdb(rec, dir, 'ecgpuwave')
	beats <- segmentation(object, by = 'sinus', pad = TRUE)

	expect_equal(nrow(beats[[1]]$signal), nrow(beats[[2]]$signal))

})

test_that('Sinus segmentation will not work, appropriately', {

	rec <- 'muse-af'
	rec_dir <- system.file('extdata', package = 'shiva')
	ecg <- read_wfdb(rec, rec_dir, 'ecgpuwave')


})
