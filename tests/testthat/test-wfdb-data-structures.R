test_that('signal table class can be made', {

	expect_equal(signal_table(), new_signal_table())
	expect_output(print(signal_table()), "signal_table")

	# Reads in sample ECG data
	dat <- read_wfdb(
		record = "ecg",
		record_dir = test_path(),
		begin = 0,
		units = "digital"
	)

	# ECG data should be 12 signal columns and 1 index column
	x <- signal_table(dat[, -1])
	expect_equal(dim(x), c(5000, 12))

})

test_that('header_table can be made using LSPro data', {

	file <- test_path("egm.txt")
	file_nm <- deparse1(substitute(file))
	hea <-
		readLines(file, n = 13) |>
		tstrsplit(split = ":\ ", fill = NA) |>
		{\(.x)
			data.table(description = .x[[1]], value = .x[[2]])
		}() |>
		{\(.y)
			list(
				file_name = file_nm,
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

	expect_length(h, 10)
	rec <- attributes(h)$record_line
	info <- attributes(h)$info_string
	expect_type(rec, 'list')
	expect_type(info, 'list')
	expect_named(rec, c('file_name', 'number_of_channels', 'samples', 'start_time', 'end_time', 'frequency', 'ADC_saturation'), ignore.order = TRUE)

})
