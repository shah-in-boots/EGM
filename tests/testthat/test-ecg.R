test_that("ecg class can be created", {
	# Create a simple ECG object with minimal data
	lead_names <- c("I", "II", "III", "AVR", "AVL", "AVF", "V1", "V2", "V3", "V4", "V5", "V6")
	sample_data <- as.data.frame(matrix(rnorm(1200), nrow = 100))
	colnames(sample_data) <- lead_names

	sig <- signal_table(sample_data)
	hea <- header_table(
		record_name = "test_ecg",
		number_of_channels = 12,
		frequency = 500,
		samples = 100,
		label = lead_names
	)

	ecg_obj <- ecg(signal = sig, header = hea)

	# Test class inheritance
	expect_s3_class(ecg_obj, "ecg")
	expect_s3_class(ecg_obj, "egm")
	expect_s3_class(ecg_obj, "list")

	# Test structure
	expect_length(ecg_obj, 3)
	expect_true(is_ecg(ecg_obj))
	expect_true(is_egm(ecg_obj))

	# Test internal components
	expect_s3_class(ecg_obj$signal, 'signal_table')
	expect_s3_class(ecg_obj$header, 'header_table')
	expect_s3_class(ecg_obj$annotation, 'annotation_table')
})

test_that("ecg validation accepts standard lead names", {
	# Create data with standard lead names
	lead_names <- c("I", "II", "III", "AVR", "AVL", "AVF", "V1", "V2", "V3", "V4", "V5", "V6")
	sample_data <- as.data.frame(matrix(rnorm(1200), nrow = 100))
	colnames(sample_data) <- lead_names

	sig <- signal_table(sample_data)
	hea <- header_table(
		record_name = "test_ecg",
		number_of_channels = 12,
		frequency = 500,
		samples = 100,
		label = lead_names
	)

	# Should create without warnings
	expect_silent(ecg(signal = sig, header = hea))
})

test_that("ecg validation handles non-standard lead names with warnings", {
	# Create data with non-standard lead names
	lead_names <- c("Lead_I", "Lead_II", "Lead_III", "aVR", "aVL", "aVF",
									"Chest1", "Chest2", "Chest3", "Chest4", "Chest5", "Chest6")
	sample_data <- as.data.frame(matrix(rnorm(1200), nrow = 100))
	colnames(sample_data) <- lead_names

	sig <- signal_table(sample_data)
	hea <- header_table(
		record_name = "test_ecg",
		number_of_channels = 12,
		frequency = 500,
		samples = 100,
		label = lead_names
	)

	# Should create with warnings
	expect_warning(ecg(signal = sig, header = hea), "Non-standard ECG lead names")
})

test_that("ecg validation warns about incorrect lead count", {
	# Create data with wrong number of leads
	lead_names <- c("I", "II", "III", "AVR", "AVL", "AVF", "V1", "V2", "V3")
	sample_data <- as.data.frame(matrix(rnorm(900), nrow = 100))
	colnames(sample_data) <- lead_names

	sig <- signal_table(sample_data)
	hea <- header_table(
		record_name = "test_ecg",
		number_of_channels = 9,
		frequency = 500,
		samples = 100,
		label = lead_names
	)

	# Should create with warnings
	expect_warning(ecg(signal = sig, header = hea), "should contain 12 leads")
})

test_that("format and print methods work correctly", {
	# Create a simple ECG object
	lead_names <- c("I", "II", "III", "AVR", "AVL", "AVF", "V1", "V2", "V3", "V4", "V5", "V6")
	sample_data <- as.data.frame(matrix(rnorm(1200), nrow = 100))
	colnames(sample_data) <- lead_names

	sig <- signal_table(sample_data)
	hea <- header_table(
		record_name = "test_ecg",
		number_of_channels = 12,
		frequency = 500,
		samples = 100,
		label = lead_names
	)

	ecg_obj <- ecg(signal = sig, header = hea)

	# Check print output
	expect_output(print(ecg_obj), "Electrogram")
	expect_output(print(ecg_obj), "Type: Standard 12-lead ECG")
})

test_that("as_ecg conversion works", {
	# Create a simple EGM object
	lead_names <- c("I", "II", "III", "AVR", "AVL", "AVF", "V1", "V2", "V3", "V4", "V5", "V6")
	sample_data <- as.data.frame(matrix(rnorm(1200), nrow = 100))
	colnames(sample_data) <- lead_names

	sig <- signal_table(sample_data)
	hea <- header_table(
		record_name = "test_egm",
		number_of_channels = 12,
		frequency = 500,
		samples = 100,
		label = lead_names
	)

	egm_obj <- egm(signal = sig, header = hea)

	# Convert to ECG
	ecg_obj <- as_ecg(egm_obj)

	# Test class conversion
	expect_s3_class(ecg_obj, "ecg")
	expect_s3_class(ecg_obj, "egm")

	# Test object structure preservation
	expect_equal(ecg_obj$signal, egm_obj$signal)
	expect_equal(ecg_obj$header, egm_obj$header)
	expect_equal(ecg_obj$annotation, egm_obj$annotation)
})

test_that("as_ecg conversion rejects non-egm objects", {
	# Try converting something that isn't an egm
	not_egm <- list(a = 1, b = 2)

	# Should error
	expect_error(as_ecg(not_egm), "must be of class 'egm'")
})

test_that("read_muse returns ecg object", {
	# Skip if file doesn't exist
	file <- system.file("extdata", "muse-sinus.xml", package = "EGM")
	if (file == "") skip("Test MUSE file not available")

	# Read ECG from MUSE
	ecg_obj <- read_muse(file)

	# Check class
	expect_s3_class(ecg_obj, "ecg")
	expect_s3_class(ecg_obj, "egm")

	# Check structure
	expect_length(ecg_obj, 3)
	expect_s3_class(ecg_obj$signal, 'signal_table')
	expect_s3_class(ecg_obj$header, 'header_table')
})

