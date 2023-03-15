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

	out <- wfdbdesc(rec, loc)
	expect_output(cat(out), "300")
})

test_that("create WFDB-compatible records for LSPro", {


	wrsamp(
		file = test_path("sample-egm.txt"),
		type = "lspro",
		record = "sample",
		write_location = test_path()
	)

	expect_fi


})
