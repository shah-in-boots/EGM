test_that("can read in annotation files", {

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

	ann <- read_annotation(
		record = "300",
		record_dir = test_path(),
		annotator = "ecgpuwave"
	)
	expect_s3_class(ann, "data.table")
	expect_named(ann,
							 expected = c("time", "sample", "type", "subtype", "channel", "number"))

})
