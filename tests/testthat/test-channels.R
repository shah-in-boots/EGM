test_that("lead-based colors can be extracted", {
	col_light <- color_channels(x = "CS 1-2", palette = "material", mode = "light")
	expect_type(col_light, "character")
	col_dark <- color_channels(x = c("CS 1-2"), palette = "material", mode = "dark")
	expect_false(col_light == col_dark)
})
