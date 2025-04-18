# Plotting ----

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
	g <- ggm(data, channels = c("I", "II", "III"))
	expect_s3_class(g, "ggm")
	expect_s3_class(g, "ggplot")

})

test_that('header and labels work fluidly when plotting', {

	skip_on_cran()
	skip_on_ci()

	data <- read_wfdb(record = 'ludb-ecg',
										record_dir = test_path(),
										annotator = 'i')

	object <- ggm(data, channels = data$header$label)

	expect_s3_class(object, 'ggm')
	expect_equal(attributes(object$theme$axis.ticks)$class[1], "element_blank")

})

# Colors ----

test_that("theming works", {

	# EPS data
	data <- read_lspro(test_path('egm.txt'))
	expect_s3_class(data, "egm")

	channels <- c("I", "CS", "HIS D", "HIS M", "RV")
	time_frame <- c(.1, 3)

	g <- ggm(
		data = data,
		channels = channels,
		time_frame = time_frame,
		mode = NULL
	)

	expect_equal(g$labels$x, "sample")
	expect_length(g$theme, 0)

	dark <- ggm(
		data = data,
		channels = channels,
		time_frame = time_frame
	)

	# When adding a theme, should be similar to the built-in


})

test_that("multiple channel data from different leads can be theme", {

	fp <- system.file('extdata', 'lspro-avnrt.txt', package = 'EGM')
	dat <- read_lspro(fp)

	# Similarly, can be visualized with ease
	g <-
		ggm(dat, channels = c('HIS', 'CS', 'RV'), mode = NULL) +
		theme_egm_dark()

})
