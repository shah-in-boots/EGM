test_that("can read in annotation files", {

	x <- read_annotation(
		record = "sample",
		annotator = "qrs",
		record_dir = test_path(),
		wfdb_path = "/usr/local/bin"
	)

	expect_s3_class(x, "data.frame")
	expect_length(x, 6)

})

