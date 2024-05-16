test_that("lead-based colors can be extracted", {
	col_light <- color_channels(x = "CS 1-2", palette = "material", mode = "light")
	expect_type(col_light, "character")
	col_dark <- color_channels(x = c("CS 1-2"), palette = "material", mode = "dark")
	expect_false(col_light == col_dark)
})

test_that("colors can be added to ggplot", {

	fp <- system.file('extdata', 'lspro-avnrt.txt', package = 'EGM')
	obj <- read_lspro(fp)

	# Similarly, can be visualized with ease
	g1 <-
		ggm(obj, channels = c('HIS', 'CS', 'RV'), mode = NULL)
	g2 <-
		ggm(obj, channels = c('HIS', 'CS', 'RV'), mode = "dark")
	g3 <-
		ggm(obj, channels = c('HIS', 'CS', 'RV'), palette = "material", mode = "dark")
	g4 <-
		ggm(obj, channels = c('HIS', 'CS', 'RV'), palette = "material")

	expect_equal(g3, g4)

})
