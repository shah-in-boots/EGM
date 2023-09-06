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
	ggm(data, channels = c("I", "II", "III")) + theme_egm_dark()

})

test_that('header and labels work fluidly when plotting', {

	sig <- read_wfdb(record = 'ludb-ecg', record_dir = test_path())
	hea <- read_header(record = 'ludb-ecg', record_dir = test_path())
	ann <-
		read_annotation(record = 'ludb-ecg',
										record_dir = test_path(),
										annotator = 'i')

	data <- egm(sig, hea, ann)
	object <- ggm(data, channels = hea$label)

	object

})

test_that("simple intervals can be added to surface leads", {

	object <- ggm(data = read_lspro(test_path('egm.txt')),
								channels = c("I", "CS", "HIS D", "HIS M", "RV"))

	obj1 <-
		object |>
		add_intervals(channel = "I")

	expect_s3_class(obj1, "ggm")
	expect_s3_class(obj1, "ggplot")

	obj2 <-
		object |>
		add_intervals(channel = "CS 9-10")

	obj3 <-
		object |>
		add_intervals(channel = "I") |>
		add_intervals(channel = "CS 9-10")

	expect_s3_class(obj3, "ggm")

})

# Colors/theme ----

test_that("colors can be applied to a light or dark theme", {

	data <- read_lspro(test_path('egm.txt'))
	channels <- c("I", "CS", "HIS D", "HIS M", "RV")
	time_frame <- c(.1, 3)

	# Basic signal plot of egms
	basic <- ggm(
		data = data,
		channels = channels,
		time_frame = time_frame
	)

	expect_true("#FFFFFF" %in% basic$data$color)
	expect_equal(basic$theme$plot.background$fill, NULL)

	light <-
		basic |>
		add_colors(palette = "material", mode = "light")

	expect_length(light$theme$plot.background, 0)
	expect_length(light$theme$panel.background, 0)

})

# Annotations ----

test_that("annotations can be added to ggplot", {

	record <- '300'
	record_dir = test_path()
	sig <- read_wfdb(record, record_dir)
	hea <- read_header(record, record_dir)
	ann <- read_annotation(record, record_dir, annotator = 'ecgpuwave')
	data <- egm(sig, hea, ann)
	channels <- 'ECG'

	object <- ggm(
		data,
		channels = 'ECG',
		time_frame = c(3, 6)
	)


	expect_length(unique(object$data$label), 2)

	masked <-
		object |>
		draw_boundary_mask()

	expect_equal(attributes(masked)$annotation, attributes(object)$annotation)
	expect_equal(attributes(masked)$header, attributes(object)$header)

	#

})
