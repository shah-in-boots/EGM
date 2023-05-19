test_that("convert ECG from MUSE XML format to WFDB", {

	# ECG XML file
	file <- system.file("extdata", "muse-sinus.xml", package = "shiva")

	# Get signal data
	ecg <- read_muse(file)
	expect_length(ecg, 12)
	expect_equal(nrow(ecg), 5000)
	expect_s3_class(ecg, "data.table")
	expect_s3_class(ecg, "egm")
	expect_type(attr(ecg, "header"), "list")


})
