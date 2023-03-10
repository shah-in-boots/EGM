skip_if_no_wfdb <- function() {
	have_wfdb <- reticulate::py_module_available("wfdb")
	if (!have_wfdb)
		skip("{wfdb} not available for testing")
}

test_that("WFDB from python can be used and test", {
	skip_if_no_wfdb()

	# Local testing
	rec <- "300"
	loc <- test_path(".")

	out <- wfdbdesc(rec, loc)
	expect_output(cat(out), "300")
})
