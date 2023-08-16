# Writing WFDB records ----

test_that('can convert lspro to wfdb with wrsamp', {

	# Convert a LSPRO text file into a WFDB compatible format
	wd <- getwd()

	file <- test_path('egm.txt')
	lspro <- read_lspro(file)
	write_wfdb(
		data = lspro,
		type = 'lspro',
		header = attributes(lspro)$header,
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

	write_wfdb(
		data = sig,
		type = 'lspro',
		record = 'egm',
		record_dir = test_path(),
		header = hea
	)

	headerFile <- readLines(test_path('egm.hea'))
	expect_length(headerFile, 19)
	expect_output(print(headerFile[1]), 'egm 14')

	file <- system.file('extdata', 'muse-sinus.xml', package = 'shiva')
	ecg <- read_muse(file)
	sig <- vec_data(ecg)
	hea <- attr(ecg, 'header')

	write_wfdb(
		data = sig,
		type = 'muse',
		record = 'ecg',
		record_dir = test_path(),
		header = hea
	)

})

# Reading WFDB records ----

test_that('rdsamp can read in WFDB formatted files', {

	# Reads in egm data (which is an EP study)
	x <- read_wfdb(
		record = 'egm',
		record_dir = test_path(),
		begin = 0,
		units = 'digital'
	)

	expect_s3_class(x, 'data.frame')

	# Reads in egm data
	y <- read_wfdb(
		record = 'ecg',
		record_dir = test_path(),
		wfdb_path = '/usr/local/bin',
		begin = 0,
		units = 'digital'
	)

	expect_s3_class(y, 'data.frame')

})

test_that('can read in LSPro header', {

	fp <- test_path("egm.hea")

	record_line <- readLines(con = fp, n = 1)
	record_items <-
		record_line |>
		strsplit('\ ') |>
		unlist()

	record_name <- record_items[1]
	number_of_channels <- record_items[2]
	frequency <- record_items[3]
	samples <- record_items[4]

	start_time <- parse_date_and_time(record_line)


	h <- header_table(
		file_name = hea$file_name,
		number_of_channels = hea$number_of_channels,
		samples = hea$samples,
		start_time = hea$start_time,
		end_time = hea$end_time,
		frequency = hea$frequency,
		ADC_saturation = hea$ADC_saturation,
		label = channels$label,
		digital_gain = channels$gain,
		low_pass = channels$low,
		high_pass = channels$high,
		color = channels$color
	)

})

test_that('can read in MUSE ECG header', {

})
