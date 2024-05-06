test_that("plots can be generated easily", {

	# EPS data
	data <- read_lspro(test_path('egm.txt'))
	expect_s3_class(data, "egm")

	channels <- c("I", "CS", "HIS D", "HIS M", "RV")
	time_frame <- c(.1, 3)

	object <- ggm(
		data = data,
		channels = channels,
		time_frame = time_frame
	)

	expect_s3_class(object, "ggm")
	expect_s3_class(object, "ggplot")

	# ECG data
	data <- read_muse(test_path('ecg.xml'))
	ggm(data, channels = c("I", "II", "III")) +
		theme_egm_dark()

})

test_that('header and labels work fluidly when plotting', {

	skip_on_cran()
	skip_on_ci()

	data <- read_wfdb(record = 'ludb-ecg',
										record_dir = test_path(),
										annotator = 'i')

	object <- ggm(data, channels = data$header$label)

	expect_s3_class(object, 'ggm')

})

