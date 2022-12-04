#' ECG and EGM Graphical Interface
#'
#' @export
geom_ecg <- function() {

}

#' Read in ECG and EGM data from LabSystem Pro (BARD)
#'
#' @export
read_lspro <- function() {
	# Setup
	library(tidyverse)

	# Location of data
	data_folder <- function() {
		x <- sessionInfo()$running
		if (grepl("mac", x)) {
			file.path("/Users",
								"asshah4",
								"OneDrive - University of Illinois at Chicago",
								"data")
		} else if (grepl("Windows", x)) {
			file.path("C:/Users",
								"asshah4",
								"OneDrive - University of Illinois at Chicago",
								"data")
		}
	}

	sample <- "m3916_avnrt.txt"

	data_loc <- file.path(data_folder(), "signals", "lspro", sample)

	# Read in overall header
	rec <-
		tibble(raw = read_lines(data_loc, n_max = 13)) |>
		separate(col = raw, into = c("description", "value"), sep = ":\ ", extra = "merge")

	header <- list(
		number_of_channels = as.numeric(rec$value[rec$description == "Channels exported"]),
		samples = {
			s <- rec$value[rec$description == "Samples per channel"]
			if (grepl(":", s)) {
				substr(s, start = 1, stop = nchar(s) - 8) |>
					as.numeric()
			} else {
				as.numeric(s)
			}
		},
		start_time = lubridate::hms(rec$value[rec$description == "Start time"]),
		end_time = lubridate::hms(rec$value[rec$description == "End time"]),
		freq = {
			f <- rec$value[rec$description == "Sample Rate"]
			if (grepl("Hz", f)) {
				gsub("Hz", "", f) |>
					as.numeric()
			} else {
				as.numeric(f)
			}
		}
	)

	# Now get all the channel info read in
	# All should be 8 elements each
	ch_list <- list()
	for (i in 1:header$number_of_channels) {
		x <-
			tibble(raw = read_lines(data_loc, skip = 13 + (i - 1) * 8, n_max = 8)) |>
			separate(raw, into = c("description", "value"), sep = ":\ ", extra = "merge")

		ch <- list(
			number = as.numeric(x[1, 2]),
			label = as.character(x[2, 2]),
			gain = as.numeric(gsub("mv", "", x[3, 2])),
			low = as.numeric(gsub("Hz", "", x[4, 2])),
			high = as.numeric(gsub("Hz", "", x[5, 2])),
			freq = as.numeric(gsub("Hz", "", x[6, 2])),
			color = paste0("#", x[7, 2]),
			scale = as.numeric(x[8, 2])
		)

		ch_list[[i]] <- ch

	}

	channels <-
		lapply(ch_list, FUN = as_tibble) |>
		bind_rows() |>
		separate(
			label,
			into = c("source", "lead"),
			sep = "\ ",
			fill = "left",
			remove = FALSE
		) |>
		mutate(source = if_else(is.na(source), "ECG", source)) |>
		mutate(lead = toupper(lead))

	# Data will actually be in CSV style
	# Written after header/channel info (13 + 8 * n + 2) ... Blank + [Data] Line
	head_space <- 13 + 8 * header$number_of_channels + 2
	signal <-
		read_csv(
			data_loc,
			col_names = channels$label,
			col_types = paste0(rep("n", header$number_of_channels), collapse = ""),
			skip = head_space + 300000,
			n_max = 10000
		)


	# Convert to milivolts from ADC units
	# [mV] = [ADC value] * [Range or gain in mV] / 32768
	ADC_saturation <- 32768
	for (i in seq_along(channels)) {
		signal[[i]] <-
			signal[[i]] * channels$gain[channels$number == i] / ADC_saturation
	}

	signal$index <- 1:nrow(signal)

	ggplot(signal, aes(x = index, y = I)) +
		geom_line()


	# Attempting grouped data
	wide_signal <-
		signal |>
		pivot_longer(-index, names_to = "label", values_to = "mV") |>
		pivot_wider(id_cols = label, names_from = index, values_from = mV) |>
		mutate(label = channels$label)

	channel_signal <-
		left_join(channels, wide_signal, by = "label") |>
		pivot_longer(cols = 11:last_col(), names_to = "index", values_to = "mV") |>
		mutate(index = as.numeric(index)/header$freq)

}


dat <- filter(channel_signal, str_detect(source, "ECG|CS"))

ggplot(dat, aes(x = index, y = mV, color = source)) +
	geom_line() +
	facet_wrap(~label, ncol = 1, scales = "free_y", strip.position = "left") +
	theme_lspro() +
	scale_x_continuous(breaks = seq(0, 10, .5), minor_breaks = seq(0, 10, .1), labels = NULL)

# Custom theme
theme_lspro <- function() {
	font <- "Arial"
	theme_minimal() %+replace%
		theme(

			# Panels
			panel.grid.major.y = element_blank(),
			panel.grid.minor.y = element_blank(),
			panel.grid.major.x = element_blank(),
			panel.grid.minor.x = element_blank(),

			# Axes
			axis.ticks.y = element_blank(),
			axis.title.y = element_blank(),
			axis.text.y = element_blank(),
			axis.title.x = element_blank(),
			axis.ticks.x = element_line(),

			# Facets
			panel.spacing = unit(0, units = "npc"),
			panel.background = element_blank(),
			strip.text.y.left = element_text(angle = 0, hjust = 1),

			# Legend
			legend.position = "none"
		)
}

