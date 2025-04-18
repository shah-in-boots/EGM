test_that("ECG can be segmented", {

	skip_on_cran()
	skip_on_ci()

	rec <- "ecg"
	dir <- test_path()
	object <- read_wfdb(rec, dir, "ecgpuwave")

	# Visualize
	g <- ggm(object)

	# Should create a number of high likelihood sinus beats
	beats <- segmentation(object, by = "sinus")
	expect_length(beats, 11)
	expect_equal(nrow(beats[[1]]$signal), 263) # Checked the size of the 1st beat

})

test_that("MUSE ECG can be segmented without error", {

	skip()
	rec <- "MUSE_20230626_141828_25000"
	rec_dir <- "/Users/asshah4/data/aflubber/ecg_data/wes_wfdb"
	object <- read_wfdb(rec, rec_dir, "ecgpuwave")

	segmentation(object, by = "sinus")
})

test_that("ECG can be padded", {

	skip_on_cran()
	skip_on_ci()

	rec <- "ecg"
	dir <- test_path()
	object <- read_wfdb(rec, dir, "ecgpuwave")

	# Truncating before
	expect_warning(
		paddedOutput <- pad_sequence(object, pad = "before", pad_length = 500)
	)
	expect_lt(nrow(paddedOutput$signal), nrow(object$signal))
	expect_equal(nrow(paddedOutput$signal), 500)

	# Truncating after
	expect_warning(
		paddedOutput <- pad_sequence(object, pad = "after", pad_length = 500)
	)
	expect_lt(nrow(paddedOutput$signal), nrow(object$signal))
	expect_equal(nrow(paddedOutput$signal), 500)

	# Truncating both
	expect_warning(
		paddedOutput <- pad_sequence(object, pad = "both", pad_length = 1000)
	)
	expect_lt(nrow(paddedOutput$signal), nrow(object$signal))
	expect_equal(nrow(paddedOutput$signal), 1000)

	# Padding of a subset beat
	beat <- segment_by_sinus(object)[[1]]

	# Padding before
	paddedOutput <- pad_sequence(beat, pad = "before", pad_length = 500)
	expect_gt(nrow(paddedOutput$signal), nrow(beat$signal))
	expect_equal(nrow(paddedOutput$signal), 500)

	# Padding after
	paddedOutput <- pad_sequence(beat, pad = "after", pad_length = 500)
	expect_gt(nrow(paddedOutput$signal), nrow(beat$signal))
	expect_equal(nrow(paddedOutput$signal), 500)

	# Padding both
	paddedOutput <- pad_sequence(beat, pad = "both", pad_length = 500)
	expect_gt(nrow(paddedOutput$signal), nrow(beat$signal))
	expect_equal(nrow(paddedOutput$signal), 500)

})

test_that("waveforms can be centered", {

	skip_on_cran()
	skip_on_ci()

	# Generally this ECG has 250 ms beats
	# Padding up to 500 should allow room to center
	rec <- "ecg"
	dir <- test_path()
	object <- read_wfdb(rec, dir, "ecgpuwave")
	beats <- segment_by_sinus(object)
	beat <- beats[[1]]

	x <- center_sequence(beat, center = "N", pad_length = 500)
	x <- center_sequence(beat, center = "N", pad_length = 500)
	expect_equal(nrow(x$signal), 500)


})

test_that("Sinus segmentation will not work, appropriately", {

	skip_on_cran()
	skip_on_ci()

	rec <- "muse-af"
	rec_dir <- system.file("extdata", package = "EGM")
	ecg <- read_wfdb(rec, rec_dir, "ecgpuwave")

	expect_length(ecg$signal, 13)

})
