#' Read in ECG data from MUSE
#' @return List of 2
#' @param file An ECG file from MUSE in XML format
#' @name muse
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
		leadNames <- as.character(.ecg) # Built in data on lead names
	}

	colnames(leadMatrix) <- .ecg

	# Each lead must have data extracted
	for (l in leadPositions) {
		lead <- xml2::as_list(rhythmData[l][[1]])
		id <- lead$LeadID[[1]]
		ampPerByte <- as.numeric(lead$LeadAmplitudeUnitsPerBit[[1]])
		waveform <- lead$WaveFormData[[1]]
		bin <- base64enc::base64decode(waveform)
		sig <- readBin(bin, integer(), n, size = 2) * ampPerByte
		leadMatrix[, id] <- sig
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
		unname() |>
		data.table::data.table()

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
	demo <- xml2::xml_contents(xml2::xml_child(doc, "PatientDemographics"))
	nms <- xml2::xml_name(demo)
	demo <- xml2::xml_text(demo)
	names(demo) <- nms
	age <- demo[["PatientAge"]]
	mrn <- demo[["PatientID"]]
	sex <- demo[["Gender"]]
	race <- demo[["Race"]]

	# Diagnosis information
	dx <-
		xml2::xml_child(doc, "Diagnosis") |>
		xml2::xml_contents() |>
		sapply(xml2::xml_text) |>
		{\(.x) gsub("RESTING", "", .x)}() |>
		{\(.x) gsub("ENDSLINE", "", .x)}() |>
		paste(collapse = ", ") |>
		{\(.x) gsub(", ,", ",", .x)}() |>
		{\(.x) gsub(",,", ",", .x)}() |>
		{\(.x) gsub("^, ", "", .x)}() |>
		trimws()

	hea <- list(
		file_name = file_nm,
		number_of_channels = leadCount,
		samples = sampleCount,
		start_time = timeStamp,
		frequency = hz,
		adc_gain = rep(1, leadCount),
		number = leadNumber,
		label = leadNames,
		mrn = mrn,
		age = age,
		sex = sex,
		race = race,
		diagnosis = dx
	)

	# Return EGM data
	egm(signal = sig, header = hea)

}

