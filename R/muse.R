#' Read in ECG data from MUSE
#'
#' @description
#' This function serves to read/convert XML based files from the MUSE system to
#' digital signal. This can subsequently be written into other formats. The MUSE
#' system is somewhat proprietary, and each version may or may not allow export
#' options into XML.
#'
#' @details
#' GE Healthcare MUSE v9 is currently the model that is being used. These
#' functions have not been tested in older versions.
#'
#' @return An `egm` class object that is a list of `eps` signals the format of a
#'   `data.table`, with an attached __header__ attribute that contains
#'   additional recording data.
#'
#' @param file An ECG file from MUSE in XML format
#'
#' @name muse
NULL

#' @rdname muse
#' @export
read_muse <- function(file) {
	# Signal data ----

	# Read in XML signal data
	doc <- xml2::read_xml(file)

	# Rhythm is the second Wavefrom (Median first)
	rhythmData <- xml2::xml_contents(xml2::xml_child(doc, "Waveform[2]"))

	# Index of lead data
	rhythmNames <- xml2::xml_name(rhythmData)
	leadPositions <- which(rhythmNames == "LeadData")
	leadCount <- length(leadPositions)

	# If its is 8 leads, we can generate the other 4 mathematically
	if (leadCount == 8) {
		leadCount <- 12
	}

	# Get sample count
	sampleCount <-
		xml2::xml_child(rhythmData, "LeadSampleCountTotal")[leadPositions[1]] |>
		xml2::xml_integer()

	# Matrix to hold results (filled with NA)
	leadMatrix <- matrix(nrow = sampleCount, ncol = leadCount)

	# Lead names
	leadNames <-
		rhythmData[leadPositions] |>
		xml2::xml_child("LeadID") |>
		xml2::xml_text()

	if (leadCount == 12) {
		leadNames <- as.character(.leads$ECG) # Built in data on lead names
	}

	colnames(leadMatrix) <- leadNames

	# Each lead must have data extracted
	for (l in leadPositions) {
		lead <- xml2::as_list(rhythmData[l][[1]])
		id <- lead$LeadID[[1]]
		ampPerByte <- as.numeric(lead$LeadAmplitudeUnitsPerBit[[1]])
		waveform <- lead$WaveFormData[[1]]
		bin <- base64enc::base64decode(waveform)
		sigData <- readBin(bin, integer(), sampleCount, size = 2) * ampPerByte
		leadMatrix[, id] <- sigData
	}

	# The augmented/avergaed leads can be recreated post-hock
	# 	III = II - I
	# 	AVR = -(I + II)/2
	# 	AVL = I - II/2
	# 	AVF = II - I/2
	leadMatrix[, "III"] <- leadMatrix[, "II"] - leadMatrix[, "I"]
	leadMatrix[, "AVR"] <- -(leadMatrix[, "I"] + leadMatrix[, "II"]) / 2
	leadMatrix[, "AVL"] <- leadMatrix[, "I"] - leadMatrix[, "II"] / 2
	leadMatrix[, "AVF"] <- leadMatrix[, "II"] - leadMatrix[, "I"] / 2

	# Return lead matrix as a data.table
	sig <-
		leadMatrix |>
		as.data.table() |>
		signal_table()

	# Header ----

	# File name
	file_nm <- deparse1(substitute(file))

	# Lead names and number in correct order
	leadNames <- colnames(leadMatrix)
	leadNumber <- seq_along(leadNames)

	# Sample frequency
	hz <-
		rhythmData[which(rhythmNames == "SampleBase")] |>
		xml2::xml_integer()

	# The acquisition time stamp
	time <-
		xml2::xml_child(doc, "TestDemographics") |>
		xml2::xml_child("AcquisitionTime") |>
		xml2::xml_text()

	date <-
		xml2::xml_child(doc, "TestDemographics") |>
		xml2::xml_child("AcquisitionDate") |>
		xml2::xml_text()

	timeStamp <-
		paste(date, time) |>
		as.POSIXct(format = "%m-%d-%Y %H:%M:%S")

	# Additional information
	# 	Demographic information
	# 	MRN
	# 	Age
	# 	Sex
	# 	Race
	demoNode <- xml2::xml_contents(xml2::xml_child(doc, "PatientDemographics"))
	nms <- xml2::xml_name(demoNode)
	demo <- xml2::xml_text(demoNode)
	names(demo) <- nms

	# MRN
	if ("PatientID" %in% nms) {
		mrn <- demo[["PatientID"]]
	} else {
		mrn <- NA
	}

	# Age
	if ("PatientAge" %in% nms) {
		age <- demo[["PatientAge"]]
	} else {
		age <- NA
	}


	# Sex
	if ("Gender" %in% nms) {
		sex <- demo[["Gender"]]
	} else {
		sex <- NA
	}

	# Race
	if ("Race" %in% nms) {
		race <- demo[["Race"]]
	} else {
		race <- NA
	}

	# Diagnosis information
	dx <-
		xml2::xml_child(doc, "Diagnosis") |>
		xml2::xml_contents() |>
		sapply(xml2::xml_text) |>
		{
			\(.x) gsub("RESTING", "", .x)
		}() |>
		{
			\(.x) gsub("ENDSLINE", "", .x)
		}() |>
		paste(collapse = ", ") |>
		{
			\(.x) gsub(", ,", ",", .x)
		}() |>
		{
			\(.x) gsub(",,", ",", .x)
		}() |>
		{
			\(.x) gsub("^, ", "", .x)
		}() |>
		trimws()

	hea <- header_table(
		record_name = file_nm,
		number_of_channels = leadCount,
		samples = sampleCount,
		frequency = hz,
		start_time = timeStamp,
		label = leadNames,
		info_strings = list(
			mrn = mrn,
			age = age,
			sex = sex,
			race = race,
			diagnosis = dx
		)
	)

	# Return EGM/ECG data
	# Note that this subclasses `egm` into `ecg` class type
	ecg(signal = sig, header = hea)

}
