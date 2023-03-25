skip_if_no_wfdb <- function() {
	have_wfdb <- reticulate::py_module_available("wfdb")
	if (!have_wfdb)
		skip("{wfdb} not available for testing")
}

test_that("WFDB from python can be used and test", {
	skip_if_no_wfdb()

	# Local testing
	record <- "300"
	location <- test_path(".")

	out <- describe_wfdb(record, location)
	expect_output(cat(out), "300")
})

test_that("create WFDB-compatible records for LSPro", {
	#skip_if_no_wfdb()

	expect_message(
		write_wfdb(
			file = test_path("sample-egm.txt"),
			type = "lspro",
			record = "sample",
			write_location = test_path()
		)
	)


})

test_that("wfdb files can be read in (with their annotation)", {
	skip_if_no_wfdb()

	record = "sample"
	location = test_path()
	channels = c("I", "III", "V1")

	x <- read_wfdb(record = record, location = location, channels = channels)
	expect_length(x, 2)
	expect_equal(ncol(x[[1]]), 3)
	expect_equal(x[[2]]$sig_name, c("I", "III", "V1"))



})


test_that("annotations can be read in", {

	record = "300"
	location = test_path()

	sig <- read_wfdb(record = record, location = location)
	a <- read_annotation(record = record, location = location, annotator = "atr")
	a$fs <- reticulate::r_to_py(360)

	wfdb$plot_wfdb(record = reticulate::r_to_py(sig),
								 annotation = reticulate::r_to_py(a))
})
