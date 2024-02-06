test_that("paths are available", {

	skip_on_cran()

	expect_match(find_wfdb_software(), "/usr/local/bin")
})
