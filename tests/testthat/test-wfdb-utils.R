test_that("paths are available", {

	skip_on_cran()
	skip_on_ci()

	expect_match(find_wfdb_software(), "/usr/local/bin")
})
