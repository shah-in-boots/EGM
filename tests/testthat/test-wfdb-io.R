# Writing WFDB records ----

test_that('can convert lspro to wfdb with wrsamp', {

	# Convert a LSPRO text file into a WFDB compatible format
	wd <- getwd()

	file <- test_path('egm.txt')
	lspro <- read_lspro(file)
	write_wfdb(
		data = lspro,
		record = 'egm',
		record_dir = test_path()
	)

	expect_true(file.exists(file.path(test_path(), 'egm.hea')))
	expect_true(file.exists(file.path(test_path(), 'egm.dat')))
	expect_equal(wd, getwd())

})

test_that('R data objects can be converted or written to WFDB format', {

	file <- test_path('egm.txt')
	sig <- read_lspro_signal(file)
	hea <- read_lspro_header(file)
	rec <- attributes(hea)$record_line
	data <- egm(sig, hea)

	write_wfdb(
		data = data,
		record = 'egm',
		record_dir = test_path()
	)

	headerFile <- readLines(test_path('egm.hea'))
	expect_gt(length(headerFile), 14)
	expect_output(print(headerFile[1]), 'egm 14')

	file <- system.file('extdata', 'muse-sinus.xml', package = 'shiva')
	ecg <- read_muse(file)

	write_wfdb(
		data = ecg,
		record = 'ecg',
		record_dir = test_path()
	)

})

# Reading WFDB records ----

test_that('rdsamp can read in WFDB formatted files', {

	# Reads in EGM data (which is an EP study)
	x <- read_wfdb(
		record = 'egm',
		record_dir = test_path(),
		begin = 0,
		units = 'digital'
	)

	expect_s3_class(x, 'data.frame')

	# Reads in ECG data
	y <- read_wfdb(
		record = 'ecg',
		record_dir = test_path(),
		wfdb_path = '/usr/local/bin',
		begin = 0,
		units = 'digital'
	)

	expect_s3_class(y, 'data.frame')


	# Read in a ECG file from PhysioNet
	z <- read_wfdb(
		record = '300',
		record_dir = test_path(),
		begin = 20
	)
})

test_that('internals of `read_header()` can create `header_table` from LSPro data', {

	fp <- test_path("egm.hea")

	record_line <- readLines(con = fp, n = 1)
	record_items <-
		record_line |>
		strsplit('\ ') |>
		unlist()

	record_name <- as.character(record_items[1])
	number_of_channels <- as.integer(record_items[2])
	frequency <- as.integer(record_items[3])
	samples <- as.integer(record_items[4])
	start_time <- parse_date_and_time(record_line)

	# Number of columns is important here
	sig_data <-
		data.table::fread(file = fp,
											skip = 1, # Skip head line
											nrows = number_of_channels) # Read in channel data

	# ADC gain is in multiple parts that need to be split
	# Units will come after a forward slash `/`
	# Baseline value will be within parenthesis
	adc <- sig_data[[3]]
	ADC_gain <- stringr::str_extract(adc, '\\d+([.]\\d+)?')
	ADC_baseline <- stringr::str_extract(adc, "\\((\\d+)\\)", group = 1)
	ADC_baseline <-
		ifelse(is.na(ADC_baseline),
					 formals(header_table)$ADC_zero,
					 ADC_baseline)
	ADC_units <- stringr::str_extract(adc, "/([:alpha:]+)", group = 1)
	ADC_units <-
		ifelse(is.na(ADC_units),
					 formals(header_table)$ADC_units,
					 ADC_units)

	h <- header_table(
		record_name = record_name,
		number_of_channels = number_of_channels,
		frequency = frequency,
		samples = samples,
		start_time = start_time,
		file_name = sig_data[[1]],
		storage_format = sig_data[[2]],
		ADC_gain = ADC_gain,
		ADC_baseline = ADC_baseline,
		ADC_units = ADC_units,
		ADC_resolution = sig_data[[4]],
		ADC_zero = sig_data[[5]],
		initial_value = sig_data[[6]],
		checksum = sig_data[[7]],
		blocksize = sig_data[[8]],
		label = sig_data[[9]]
	)

	expect_s3_class(h, 'header_table')
	expect_equal(nrow(h), 14)

})

test_that('can read in MUSE ECG header', {

})
