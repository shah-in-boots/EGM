test_that("egm class can be made", {

	file <- test_path("sample-egm.txt")
	dt <- read_lspro(file)
	header <- dt$header
	signal <- dt$signal

	new_egm(header = dt$header, signal = dt$signal)

})
