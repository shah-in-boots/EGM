# Test read_prucka_header ------------------------------------------------

test_that("read_prucka_header reads basic metadata correctly", {

	file <- test_path("prucka.inf")
	hea <- read_prucka_header(file)

	# Test header structure
	expect_s3_class(hea, "header_table")
	expect_s3_class(hea, "data.table")

	# Test record line attributes
	expect_equal(attributes(hea)$record_line$number_of_channels, 28)
	expect_equal(attributes(hea)$record_line$samples, 4027)
	expect_equal(attributes(hea)$record_line$frequency, 977)
})

test_that("read_prucka_header parses channel labels correctly", {

	file <- test_path("prucka.inf")
	hea <- read_prucka_header(file)

	# Check channel labels
	expect_equal(length(hea$label), 28)
})

test_that("read_prucka_header handles missing file", {
	expect_error(
		read_prucka_header("nonexistent_file.inf"),
		"File not found"
	)
})

test_that("read_prucka_header parses timestamps correctly", {

	file <- test_path("prucka.inf")
	hea <- read_prucka_header(file)

	# Check that start_time is POSIXct
	expect_s3_class(attributes(hea)$record_line$start_time, "POSIXct")
})

test_that("read_prucka_header handles non-sequential channel numbers", {

	# Create test file with non-sequential channels
	temp_dir <- tempdir()
	inf_content <- c(
		"Patient = TEST",
		"Description = Test",
		"Date = 10/12/2025 2:30:00 PM",
		"Number of Channel = 5",
		"Points for Each Channel = 100",
		"Data Sampling Rate = 1000 points/second",
		"Start Time = 10/12/2025 2:30:00 PM",
		"Channel Number  Channel Label",
		"1              I",
		"5              V1",
		"10             ABL",
		"50             CS",
		"75             RV"
	)

	inf_file <- file.path(temp_dir, "test_nonseq.inf")
	writeLines(inf_content, inf_file)
	on.exit(unlink(inf_file))

	hea <- read_prucka_header(inf_file)

	# Channels should be sorted by number
	expect_equal(hea$label, c("I", "V1", "ABL", "CS", "RV"))
	expect_equal(length(hea$label), 5)
})


# Test read_prucka_signal ------------------------------------------------

test_that("read_prucka_signal reads signal data correctly", {

	file <- test_path("prucka.txt")
	sig <- read_prucka_signal(file)

	# Test signal structure
	expect_s3_class(sig, "signal_table")
	expect_s3_class(sig, "data.table")

	# Should have 4027 rows and 29 columns (28 channels + index)
	expect_equal(nrow(sig), 4027)
	expect_equal(ncol(sig), 29)
})

test_that("read_prucka_signal respects n parameter", {

	file <- test_path("prucka.txt")
	sig <- read_prucka_signal(file, n = 50)

	# Should only read 50 rows
	expect_equal(nrow(sig), 50)
})

test_that("read_prucka_signal handles missing file", {
	expect_error(
		read_prucka_signal("nonexistent_file.txt"),
		"File not found"
	)
})

# Test read_prucka -------------------------------------------------------

test_that("read_prucka combines header and signal correctly", {

	file <- test_path("prucka.txt")
	prucka_data <- read_prucka(file)

	# Test egm structure
	expect_s3_class(prucka_data, "egm")
	expect_length(prucka_data, 3)

	# Test components
	expect_s3_class(prucka_data$signal, "signal_table")
	expect_s3_class(prucka_data$header, "header_table")
	expect_s3_class(prucka_data$annotation, "annotation_table")
})

test_that("read_prucka assigns channel names correctly", {

	file <- test_path("prucka.txt")
	prucka_data <- read_prucka(file)

	# Check that signal columns are named correctly
	expected_names <- c("sample", "I", "II", "V1", "ABL")
	expect_true(all(expected_names %in% names(prucka_data$signal)))
})

test_that("read_prucka finds inf file automatically", {

	file <- test_path("prucka.txt")
	prucka_data <- read_prucka(file)

	expect_s3_class(prucka_data, "egm")
})
