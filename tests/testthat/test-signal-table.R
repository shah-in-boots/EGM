test_that('signal table class can be made', {

	expect_equal(signal_table(), new_signal_table())
	expect_output(print(signal_table()), "signal_table")

	# Reads in sample ECG data
	dat <- read_wfdb(
		record = "ecg",
		record_dir = test_path(),
		wfdb_path = "/usr/local/bin",
		begin = 0,
		units = "digital"
	)

	# ECG data should be 12 signal columns and 1 index column
	x <- signal_table(dat[, -1])
	expect_equal(dim(x), c(5000, 12))

})
