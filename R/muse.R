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
#' # Units and gain
#'
#' MUSE stores each sample as an ADC count together with the scale that converts
#' it, `LeadAmplitudeUnitsPerBit`, and the units that scale is in,
#' `LeadAmplitudeUnits`. The scale is applied while reading, so the returned
#' signal holds those units — microvolts on every export seen — and the header
#' gets the `ADC_gain` that carries them back to millivolts, 1000 for a
#' microvolt payload. A record whose units are unrecognised, or that names
#' different units on different leads, is refused rather than given a gain that
#' would put its amplitudes on an unknown scale.
#'
#' The samples are deliberately left on the microvolt scale rather than returned
#' to raw counts. The augmented limb leads are derived here as halves of the
#' recorded ones, so they fall between counts, and rounding them back onto the
#' count grid would throw that precision away.
#'
#' @return An `EGM` class object that is a list of surface ECG signals the
#'   format of a `data.table`, with an attached __header__ attribute that
#'   contains additional recording data.
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
  # MUSE names the units its per-bit scale is in alongside the scale itself, and
  # both are collected here because the scale is applied to the samples below.
  ampUnits <- character()

  for (l in leadPositions) {
    lead <- xml2::as_list(rhythmData[l][[1]])
    id <- lead$LeadID[[1]]
    ampPerByte <- as.numeric(lead$LeadAmplitudeUnitsPerBit[[1]])
    ampUnits <- c(
      ampUnits,
      toupper(as.character(unlist(lead$LeadAmplitudeUnits)))
    )
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

  # `LeadAmplitudeUnitsPerBit` has already been applied to every sample, so the
  # matrix holds the units MUSE named rather than raw ADC counts.  The WFDB gain
  # is what carries them back to millivolts, and the WFDB default of 200 reads a
  # microvolt payload as though it were counts - a five-fold inflation of every
  # amplitude that nothing downstream can catch, because the values and their
  # "digital" label agree with each other and only the header disagrees.
  # Do not rescale the payload back to counts instead: the limb leads derived
  # above are halves, and land between counts.
  ADC_gain <- c(MICROVOLTS = 1000, MILLIVOLTS = 1)[unique(ampUnits)]
  if (length(ADC_gain) != 1 || is.na(ADC_gain)) {
    stop(
      "`LeadAmplitudeUnits` was read as ",
      if (length(ampUnits) == 0) {
        "absent"
      } else {
        paste(unique(ampUnits), collapse = ", ")
      },
      ", and only a single MICROVOLTS or MILLIVOLTS record can be given an ",
      "ADC gain. Reading it anyway would put every amplitude in this record ",
      "on an unknown scale.",
      call. = FALSE
    )
  }

  hea <- header_table(
    record_name = file_nm,
    number_of_channels = leadCount,
    samples = sampleCount,
    frequency = hz,
    start_time = timeStamp,
    ADC_gain = unname(ADC_gain),
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
  # Note that this subclasses `EGM` into `ecg` class type
  ECG(signal = sig, header = hea)
}
