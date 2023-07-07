test_that("plots can be generated easily", {

	data <- read_lspro(test_path('egm.txt'))
	expect_s3_class(data, "egm")

	channels <- c("I", "CS", "HIS D", "HIS M", "RV")
	time_frame <- c(.1, 3)

	# Basic signal plot of egms
	object <- ggm(
		data = data,
		channels = channels,
		time_frame = time_frame
	) + theme_egm_light()

	expect_s3_class(object, "ggm")
	expect_s3_class(object, "ggplot")
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

test_that("colors can be applied to a light or dark theme", {

	data <- read_lspro(test_path('egm.txt'))
	channels <- c("I", "CS", "HIS D", "HIS M", "RV")
	time_frame <- c(.1, 3)

	# Basic signal plot of egms
	dark <- ggm(
		data = data,
		channels = channels,
		time_frame = time_frame
	)

	expect_true("#FFFFFF" %in% dark$data$color)
	expect_equal(dark$theme$plot.background$fill, "black")

	light <-
		dark |>
		add_colors(palette = "material", mode = "light")

	expect_length(light$theme$plot.background, 0)
	expect_length(light$theme$panel.background, 0)

})
