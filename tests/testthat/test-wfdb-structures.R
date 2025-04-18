test_that("signal table class can be made", {

	skip_on_cran()
	skip_on_ci()

	expect_equal(signal_table(), new_signal_table())
	expect_output(print(signal_table()), "signal_table")

	# Reads in sample ECG data
	dat <- read_signal(
		record = "ecg",
		record_dir = test_path(),
		begin = 0,
		units = "digital"
	)

	# ECG data should be 12 signal columns and 1 index column
	x <- dat[, -1]
	expect_equal(dim(x), c(5000, 12))

})

test_that("header_table can be made using LSPro data", {

	# Initial values
	record_name = character() # Record line information
	number_of_channels = integer()
	frequency = 250.0
	samples = integer()
	start_time = Sys.time()
	ADC_saturation = integer()
	file_name = character() # Signal specific information
	storage_format = 16L
	ADC_gain = 200L
	ADC_units = "mV"
	ADC_resolution = 12L
	ADC_zero = 0L
	ADC_baseline = ADC_zero
	initial_value = ADC_zero
	checksum = 0L
	blocksize = 0L
	label = character()
	info_strings = list() # Secondary information
	additional_gain = 1.0
	low_pass = integer()
	high_pass = integer()
	color = character()
	scale = integer()

	file <- test_path("egm.txt")
	record_name <- deparse1(substitute(file))
	file_name <- paste0(deparse1(substitute(file)), ".dat")
	hea <-
		readLines(file, n = 13) |>
		tstrsplit(split = ":\ ", fill = NA) |>
		{\(.x)
			data.table(description = .x[[1]], value = .x[[2]])
		}() |>
		{\(.y)
			list(
				record_name = record_name,
				number_of_channels = as.numeric(.y$value[.y$description == "Channels exported"]),
				samples = {
					s <- .y$value[.y$description == "Samples per channel"]
					if (grepl(":", s)) {
						substr(s, start = 1, stop = nchar(s) - 8) |>
							as.numeric()
					} else {
						as.numeric(s)
					}
				},
				start_time = as.POSIXct(strptime(.y$value[.y$description == "Start time"], format = "%H:%M:%S")),
				end_time = as.POSIXct(strptime(.y$value[.y$description == "End time"], format = "%H:%M:%S")),
				frequency = {
					f <- .y$value[.y$description == "Sample Rate"]
					if (grepl("Hz", f)) {
						gsub("Hz", "", f) |>
							as.numeric()
					} else {
						as.numeric(f)
					}
				}
			)
		}()

	hea$ADC_saturation <- 32768

	ch_list <- list()
	for (i in 1:hea$number_of_channels) {
		ch_list[[i]] <-
			fread(
				file,
				skip = 13 + (i - 1) * 8,
				nrows = 8,
				sep = NULL,
				header = FALSE
			) |>
			unlist() |>
			tstrsplit(split = ":\ ") |>
			{
				\(.x)
				data.table(description = .x[[1]], value = .x[[2]])
			}() |>
			{
				\(.y)
				list(
					number = as.numeric(.y[1, 2]),
					label = as.character(.y[2, 2]),
					gain = as.numeric(gsub("mv", "", .y[3, 2])),
					low = as.numeric(gsub("Hz", "", .y[4, 2])),
					high = as.numeric(gsub("Hz", "", .y[5, 2])),
					frequency = as.numeric(gsub("Hz", "", .y[6, 2])),
					color = paste0("#", .y[7, 2]),
					scale = as.numeric(.y[8, 2])
				)
			}()
	}

	channels <- rbindlist(ch_list)

	h <- header_table(
		record_name = hea$record_name,
		number_of_channels = hea$number_of_channels,
		frequency = hea$frequency,
		samples = hea$samples,
		start_time = hea$start_time,
		ADC_saturation = hea$ADC_saturation,
		file_name = file_name,
		label = channels$label,
		additional_gain = channels$gain,
		low_pass = channels$low,
		high_pass = channels$high,
		color = channels$color
	)

	expect_length(h, 19)
	expect_true(length(unique(h$color)) > 1)
	expect_true(length(unique(h$low_pass)) == 2)
	expect_true(length(unique(h$high_pass)) == 2)
	rec <- attributes(h)$record_line
	info <- attributes(h)$info_string
	expect_type(rec, "list")
	expect_type(info, "list")
	expect_named(rec, c("record_name", "number_of_channels", "samples", "start_time", "frequency", "ADC_saturation"), ignore.order = TRUE)

})

test_that("minimal header_table can be made that can then be written to WFDB", {

	x <-
		header_table(record_name = "temp",
								 frequency = 500)

	expect_equal(attributes(x)$record_line$record_name, "temp")


})

test_that("an annotation table can be created", {

	fp <- system.file('extdata', 'muse-sinus.xml', package = 'EGM')
	ecg <- read_muse(fp)
	x <-
		ecg$signal$I |>
		abs()
	sample <- which(diff(sign(diff(x))) == -2) + 1
	sample <- sample[x[sample] > (mean(x, na.rm = TRUE) + 2 * sd(x, na.rm = TRUE))]


	ann <- annotation_table(
	  annotator = "test_peaks",
	  sample = sample,
	  type = "R",
	  frequency = attributes(ecg$header)$record_line$frequency,
	  channel = "I"
	)

	expect_s3_class(ann, "annotation_table")

})
