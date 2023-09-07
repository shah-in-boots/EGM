test_that("ECG segmentation works", {

	rec <- 'ecg'
	dir <- test_path()
	object <- read_wfdb(rec, dir, 'ecgpuwave')

	# Visualize
	g <-
		ggm(object) |>
		draw_boundary_mask()


})
