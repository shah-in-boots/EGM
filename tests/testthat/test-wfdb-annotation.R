test_that("can read in annotation files", {

	skip_on_cran()
	skip_on_ci()

	x <- read_annotation(
		record = "300",
		record_dir = test_path(),
		annotator = "ecgpuwave"
	)

	expect_s3_class(x, "data.frame")
	expect_length(x, 6)
	expect_output(print(x), 'ecgpuwave')

})

test_that("can write annotation files", {

	skip_on_cran()
	skip_on_ci()

	ann <- read_annotation(
		record = "300",
		record_dir = test_path(),
		annotator = "ecgpuwave"
	)
	expect_s3_class(ann, "data.table")
	expect_named(ann,
							 expected = c("time", "sample", "type", "subtype", "channel", "number"))

})

test_that('can read in faulty signal safely', {

	skip_on_cran()
	skip_on_ci()

	# Bad ECG that has no signal
	record <- 'bad-ecg'
	record_dir <- test_path()

	read_wfdb(record, record_dir, annotator = 'ecgpuwave')
	expect_s3_class(read_header(record, record_dir), 'header_table')
	expect_s3_class(read_signal(record, record_dir), 'signal_table')
	expect_message({
		ann <- read_annotation(record, record_dir, annotator = 'ecgpuwave')
	})
	expect_length(ann, 6)
	expect_equal(nrow(ann), 0)

})
