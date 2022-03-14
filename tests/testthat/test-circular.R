test_that("circular-linear model", {

	# Linear regression
	raw <-
		cm(
			formula = el_svg ~ lab_hba1c + bmi,
			type = "linear",
			data = card::geh,
			units = "degrees"
		)
	tidy <-
		cm(
			formula = el_svg ~ lab_hba1c + bmi,
			type = "linear",
			data = card::geh,
			units = "degrees",
			tidy = TRUE
		)


})
