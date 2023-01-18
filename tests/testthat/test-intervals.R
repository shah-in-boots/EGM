test_that("can find peaks in a single channel", {
	df <- read_lspro(file = test_path("sample-egm.txt"))
	pks <- find_major_peaks(df$I)
	sig <- as.numeric(df$I)
	expect_length(pks, 2)
	expect_type(pks, "list")

	plot(sig, type = "l", col = "black")
	points(pks$location, pks$peaks, type = "p", col = "orange")

})
