# Writing WFDB records ----

test_that('can convert lspro to wfdb with wrsamp', {

	# Convert a LSPRO text file into a WFDB compatible format
	wd <- getwd()
	expect_silent(
		rewrite_wfdb(
			file = 'egm.txt',
			type = 'lspro',
			record = 'egm',
			record_dir = test_path()
		)
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
		wfdb_path = '/usr/local/bin',
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

test_that('headers can be read in', {

	record <- 'ecgm'
	record_dir <- test_path()

	header_table(
		file_name = hea$file_name,
		number_of_channels = hea$number_of_channels,
		samples = hea$samples,
		start_time = hea$start_time,
		end_time = hea$end_time,
		frequency = hea$frequency,
		ADC_saturation = hea$ADC_saturation,
		label = channels$label,
		gain = channels$gain,
		low_pass = channels$low,
		high_pass = channels$high,
		color = channels$color
	)

})
