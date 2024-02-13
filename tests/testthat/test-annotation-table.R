test_that('annotation table class can be made', {

	skip_on_cran()
	skip_on_ci()

	expect_named(annotation_table())
	expect_output(print(annotation_table()), "annotation_table")

	# Reads in sample ECG data
	dat <- read_annotation(
		record = 'ecg',
		record_dir = test_path(),
		annotator = 'ecgpuwave'
	)

	# ECG data should be 12 annotation columns and 1 index column
	expect_s3_class(dat, 'annotation_table')
	expect_length(dat, 6)
	expect_equal(nrow(dat), 117)

})
