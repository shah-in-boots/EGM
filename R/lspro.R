#' Read in ECG and EGM data from LabSystem Pro
#'
#' This function allows for reading in LS Pro data based on their text export of
#' signals. Signals can be exported directly from the LS Pro system.
#'
#' The [LabSystem
#' Pro](https://www.bostonscientific.com/en-US/products/capital-equipment--diagnotic/labsystem-pro.html)
#' was acquired by Boston Scientific from the original company
#' [Bard](https://news.bostonscientific.com/2013-06-28-Boston-Scientific-To-Acquire-C-R-Bard-Electrophysiology-Business).
#' They are a common electrophysiology signal processing device for
#' visualization and measurement of intracardiac signals.
#'
#' ## Data Export
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
#' ## Data Format
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
#' @return A `list` of length = 2 that has a 1) header table and 2) signal
#'   table.
#' @export
read_lspro <- function(file, n = Inf) {

	# Overall header
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
				freq = {
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
					freq = as.numeric(gsub("Hz", "", .y[6, 2])),
					color = paste0("#", .y[7, 2]),
					scale = as.numeric(.y[8, 2])
				)
			}()
	}

	# Channels should be organized appropriately
	# Top to bottom should be from high to low, and then from left to right
	# Catheters/leads are specifically included
	surface <-
		c("I", "II", "III", "AVF", "AVL", "AVR", paste0("V", 1:6))
	lead_pairs <-
		paste0(seq(from = 1, to = 19, by = 2), "-", seq(from = 2, to = 20, by = 2))
	lead_loc <- c("D", "DIST", "DISTAL",
								"M", "MID", "MIDDLE",
								"P", "PROX", "PROXIMAL")
	hra <- rev(paste("RA", lead_pairs[1:2]))
	his <- rev(paste("HIS", c(lead_pairs[1:3], lead_loc)))
	cs <- rev(paste("CS", lead_pairs[1:5]))
	dd <- rev(paste("DD", lead_pairs[1:10]))
	rv <- rev(paste("RV", lead_pairs[1:2]))
	abl <- rev(paste("ABL", c(lead_pairs[1:2], lead_loc[c(1:3, 7:9)])))

	# Order patterns
	lead_order <- c(surface, rev(c(lead_pairs, lead_loc)))
	label_order <- c(surface, hra, his, cs, dd, rv, abl)
	source_order <- c("ECG", "HRA", "RA", "HIS", "CS", "DD", "RV", "ABL")

	# Table of channel information
	# Clean up names if possible
	# All are made upper character
	channels <-
		ch_list |>
		rbindlist() |>
		dplyr::mutate(label = toupper(label)) |>
		tidyr::separate(
			label,
			into = c("source", "lead"),
			sep = "(?<=[a-zA-Z])\\s*(?=[0-9])",
			remove = FALSE,
			fill = "right"
		) |>
		dplyr::mutate(lead = dplyr::case_when(
			label %in% surface ~ label,
			TRUE ~ lead
		)) |>
		dplyr::mutate(source = dplyr::case_when(
			label %in% surface ~ "ECG",
			TRUE ~ source
		)) |>
		tidyr::separate(
			source,
			into = c("source", "extra"),
			sep = "\ ",
			fill = "right"
		) |>
		dplyr::mutate(lead = dplyr::if_else(is.na(lead), extra, lead)) |>
		dplyr::select(-extra) |>
		dplyr::mutate(label = sub("ECG\ ", "", paste(source, lead)))

	# Now reorder the levels by factor
	channels$label <-
		factor(channels$label, levels = intersect(label_order, channels$label))
	channels$source <-
		factor(channels$source, levels = intersect(source_order, channels$source))
	channels$lead <-
		factor(channels$lead, levels = intersect(lead_order, channels$lead))

	### Colors ###

	# Surface leads (blues)
	channels$color[channels$source == "ECG"] <- "#0C46A0"

	# Ablation catheter (usually 2 leads) (purple-red)
	channels$color[channels$source == "ABL"] <- "#870D4E"

	# CS catheter, usually decapolar, 5 leads (teals)
	channels$color[channels$source == "CS"] <-
		c("#004C3F", "#00685B", "#00796B", "#00887A", "#009687")

	# Duodecapolar catheter, 10 leads (teals)
	channels$color[channels$source == "DD"] <-
		rep(c("#004C3F", "#00685B", "#00796B", "#00887A", "#009687"), each = 2)

	# His catheter tends to be 2-3 leads (gold)
	channels$color[channels$source == "HIS"] <- "#F8A72B"

	# RV and RA catheters are usually 2 leads (pink-reds)
	channels$color[channels$source == "RV"] <- "#AC135F"
	channels$color[channels$source == "RA"] <- "#C1185A"

	# Read in the CSV-styled signal data quickly
	sig <-
		fread(
			file,
			skip = "[Data]",
			header = FALSE,
			col.names = as.character(channels$label),
			nrows = n
		)

	# Convert to milivolts from ADC units
	# [mV] = [ADC value] * [Range or gain in mV] / 32768
	ADC_saturation <- 32768
	for (i in 1:hea$number_of_channels) {
		sig[[i]] <-
			sig[[i]] * channels$gain[channels$number == i] / ADC_saturation
	}

	# Return a new list_of class
	egm(header = as.data.table(hea),
			channels = channels,
			signal = sig)

}

