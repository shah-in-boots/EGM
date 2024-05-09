#' Read in ECG and EGM data from LabSystem Pro
#'
#' @description This function allows for reading in LS Pro data based on their
#' text export of signals. Signals can be exported directly from the LS Pro
#' system.
#'
#' The [LabSystem
#' Pro](https://www.bostonscientific.com/en-EU/medical-specialties/electrophysiology/arrhythmias/cardiac-mapping-system/electrophysiology-recording-system.html)
#' was acquired by Boston Scientific from the original company
#' [Bard](https://news.bostonscientific.com/2013-06-28-Boston-Scientific-To-Acquire-C-R-Bard-Electrophysiology-Business).
#' They are a common electrophysiology signal processing device for
#' visualization and measurement of intracardiac signals.
#'
#' @details
#' # Data Export
#'
#' The steps to data export are as follows.
#'
#' 1. Start LabSystem PRO
#'
#' 1. Open a patient record
#'
#' 1. Display a waveform recording in a Review Window
#'
#' 1. Scroll to a point of interest in a waveform recording
#'
#' 1. Right click on the review window to the left of the region of interest
#'
#' 1. Select an Export option, either a default time range or the entire visible
#' page (which depends on the sweep speed).
#'
#' # Data Format
#'
#' ```
#' [Header] Recording info – contains (example):
#'
#' [Header]<CR><LF>
#' 	 File Type: 1<CR><LF>
#' 	 Version: 1<CR><LF>
#' 	 Channels exported: 22<CR><LF>
#' 	 Samples per channel: 5000<CR><LF>
#' 	 Start time:  6:55:24<CR><LF>
#' 	 End time:  6:55:29<CR><LF>
#' 	 Ch. Info. Pointer: 320<CR><LF>
#' 	 Stamp Data: T<CR><LF>
#' 	 Mux format: 0<CR><LF>
#' 	 Mux Block Size: <CR><LF>
#' 	 Data Format 1<CR><LF>
#' 	 Sample Rate: 1000Hz<CR><LF>
#'
#' [Header] Channel info (per channel example):
#'
#'   Channel #: 1<CR><LF>
#'   Label: III<CR><LF>
#' 	 Range: 5mv <CR><LF>
#' 	 Low: 1Hz<CR><LF>
#' 	 High: 100Hz<CR><LF>
#' 	 Sample rate: 1000Hz<CR><LF>
#' 	 Color: 0000FF<CR><LF>
#' 	 Scale: -7<CR><LF>
#'
#' [Data] As described below:
#'
#' 	-256,-1056,576,-256,320,-736,144,576,-592,176,608,240,176,-560,496,-
#' 	144,0,0,-32,-48,-32,-80<CR><LF>
#'
#' ```
#'
#' Channel Data is interleaved in the example above (sample indexed at 1):
#'
#' |  1 |  2 |  3 |  ... | 22 |
#' | --- | --- | --- | --- | --- |
#' | Ch1:1 | Ch2:1 | Ch3:1 | ...  | Ch22:1 |
#' | Ch1:2 | Ch2:2 | Ch3:2 | ...  | Ch22:2 |
#' | Ch1:3 | Ch2:3 | Ch3:3 | ...  | Ch22:3 |
#' | ... | ... | ... | ... | ... |
#' | Ch1:5000 | Ch2:5000 | Ch3:5000 | ...  | Ch22:5000 |
#'
#' @param file The path to the file where the data is located. It must be a
#'   __*.txt__ file. See details below about its format.
#'
#' @param n Number of signal values to return (this will be the same for each
#'   channel of data). Defaults to all values.
#'
#' @return An `egm` class object that is a list of `eps` signals the format of a
#'   `data.table`, with an attached __header__ attribute that contains
#'   additional recording data.
#' @name lspro
NULL

#' @rdname lspro
#' @export
read_lspro <- function(file, n = Inf) {

	# Read in the LSPro data and package it into a simple "EGM" table
	# This is to ensure class safety when plotting
	sig <- read_lspro_signal(file = file, n = n)
	hea <- read_lspro_header(file = file)

	# Get the correct naems from the header file
	names(sig) <- c('sample', as.character(hea$label))

	egm(signal = sig, header = hea)

}

#' @rdname lspro
#' @export
read_lspro_header <- function(file) {

	record_name <- fs::path_ext_remove(fs::path_file(file))
	file_name <- paste0(record_name, '.dat')

	hea <-
		readLines(file, n = 13) |>
		tstrsplit(split = ":\ ", fill = NA) |>
		{\(.x)
			data.table(description = .x[[1]], value = .x[[2]])
		}() |>
		{\(.y)
			list(
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

	# Data is given in 16-bit integer format
	# Convert to milivolts from ADC units
	# [mV] = [ADC value] * [Range or gain in mV] / 32768
	hea$ADC_saturation <- 32768

	# Individual channel data, 8 elements each
	# Written after header/channel info (13 + 8 * n + 2) ... Blank + [Data] Line
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

	# Return
	header_table(
		record_name = record_name,
		file_name = file_name,
		number_of_channels = hea$number_of_channels,
		samples = hea$samples,
		start_time = hea$start_time,
		frequency = hea$frequency,
		ADC_saturation = hea$ADC_saturation,
		label = channels$label,
		additional_gain = channels$gain,
		low_pass = channels$low,
		high_pass = channels$high,
		color = channels$color
	)

}

#' @rdname lspro
#' @export
read_lspro_signal <- function(file, n = Inf) {

	# Read in the CSV-styled signal data quickly
	sig <-
		fread(
			file,
			skip = "[Data]",
			header = FALSE,
			nrows = n
		)

	signal_table(sig)

}
