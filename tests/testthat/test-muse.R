test_that("convert ECG from MUSE XML format to WFDB", {

	# ECG XML file
	file <- system.file("extdata", "muse-sinus.xml", package = "EGM")

	# Get signal data
	ecg <- read_muse(file)
	expect_length(ecg, 3)
	expect_length(ecg$signal, 13)
	expect_equal(nrow(ecg$signal), 5000)
	expect_s3_class(ecg, c("egm", "ecg"))
	expect_s3_class(ecg$signal, "signal_table")
	expect_s3_class(ecg$signal, "data.table")
	expect_s3_class(ecg$header, "header_table")

})
