test_that("plots can be generated easily", {

	data <- read_lspro(test_path('sample-egm.txt'))
	expect_s3_class(data, "egm")

	channels <- c("I", "CS", "HIS D", "HIS M", "RV")
	time_frame <- c(.1, 3)

	# Basic signal plot of egms
	object <- ggm(
		data = data,
		channels = channels,
		time_frame = time_frame
	)

	expect_s3_class(object, "ggm")
	expect_s3_class(object, "ggplot")

	# Add intervals to the surface lead
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

})
