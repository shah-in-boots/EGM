# Writing WFDB records ----

test_that('can convert bard to wfdb with wrsamp', {

	skip_on_cran()
	skip_on_ci()

	# Convert a bard text file into a WFDB compatible format
	wd <- getwd()

	file <- test_path('bard-egm.txt')
	bard <- read_bard(file)
	write_wfdb(
		data = bard,
		record = 'bard-egm',
		record_dir = test_path()
	)

	expect_true(file.exists(file.path(test_path(), 'bard-egm.hea')))
	expect_true(file.exists(file.path(test_path(), 'bard-egm.dat')))
	expect_equal(wd, getwd())

})

test_that('R data objects can be converted or written to WFDB format', {

	skip_on_cran()
	skip_on_ci()

	file <- test_path('bard-egm.txt')
	sig <- read_bard_signal(file)
	hea <- read_bard_header(file)
	rec <- attributes(hea)$record_line
	data <- egm(sig, hea)

	write_wfdb(
		data = data,
		record = 'bard-egm',
		record_dir = test_path()
	)

	headerFile <- readLines(test_path('bard-egm.hea'))
	expect_gt(length(headerFile), 14)
	expect_output(print(headerFile[1]), 'egm 14')

	file <- system.file('extdata', 'muse-sinus.xml', package = 'EGM')
	ecg <- read_muse(file)

	write_wfdb(
		data = ecg,
		record = 'ecg',
		record_dir = test_path()
	)

})

# Reading WFDB records ----

test_that('rdsamp can read in WFDB formatted files for signal data', {

	skip_on_cran()
	skip_on_ci()

	# Reads in EGM data (which is an EP study)
	x <- read_signal(
		record = 'egm',
		record_dir = test_path(),
		begin = 0L,
		units = 'digital'
	)

	expect_s3_class(x, 'data.frame')

	# Reads in ECG data
	y <- read_signal(
		record = 'ecg',
		record_dir = test_path(),
		begin = 0,
		units = 'digital'
	)

	expect_s3_class(y, 'data.frame')

	# Read in a ECG file from PhysioNet
	z <- read_signal(
		record = '300',
		record_dir = test_path(),
		begin = 20
	)

	expect_s3_class(z, 'signal_table')

})

test_that('internals of `read_header()` can create `header_table` from bard data', {

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

test_that('can read in WFDB file into `egm` directly', {

	skip_on_cran()
	skip_on_ci()

	# Basics
	record = 'ecg'
	record_dir = test_path()
	annotator = 'ecgpuwave'
	wfdb_path = getOption("wfdb_path")
	begin = 0
	end = NA_integer_
	interval = NA_integer_
	units = "digital"
	channels = character()

	x <- read_wfdb(
		record = record,
		record_dir = record_dir,
		wfdb_path = wfdb_path,
		annotator = annotator,
		begin = begin,
		end = end,
		interval = interval,
		units = units,
		channels = channels
	)

	expect_s3_class(x, 'egm')

	# From the stored package data

	rec <- 'muse-sinus'
	dir <- system.file('extdata', 'muse-sinus.dat', package = 'EGM')
	ecg <- read_wfdb(rec, fs::path_dir(dir))


})

test_that('can read in MUSE ECG header', {

	skip_on_cran()
	skip_on_ci()

	# Simple header
	hea <- read_header("ecg", record_dir = test_path())
	expect_equal(unique(hea$file_name), "ecg.dat")

	# Complex header
	fp <- system.file("extdata", "muse-sinus.hea", package = "EGM")
	hea <- read_header(
		record =
			fs::path_file(fp) |>
			fs::path_ext_remove(),
		record_dir = fs::path_dir(fp)
	)

	header <- readLines(fp)
	expect_equal(hea$color, unlist(strsplit(header[16], " "))[-c(1:2)])

})

