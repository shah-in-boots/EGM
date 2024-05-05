#' Segmentation of electrical signal by wave specifications
#'
#' @details Requires a 12-lead ECG that has been digitized, and input as an
#' `egm` object. This object must have an annotation file associated with it
#' that contains demarcation annotations. Please see below for approaches based
#' on the annotation type. Current, the following are supported:
#'
#' - sinus = supports using *ecgpuwave* as the annotator
#'
#' # Sinus beat segmentation
#'
#' Identify individual sinus beats on surface ECG and extract as individual
#' beats, returning a list of sinus beats in the form of the `egm` class. a
#' consistent __P__, __R__, and __T__ wave amongst all channels. If a channel
#' does not have, for example, a visible __T__ wave, it will still label it as
#' information gained from other channels. This is based off of the algorithm
#' from the annotation tool named `ecgpuwave`. Please see [read_annotation()]
#' for further details.
#'
#' @return Returns a list of `egm` objects. Each item is a segmentation of an
#'   `egm`, using the selected channels (if available). It will attempt to
#'   optimize and pick the best annotations to help create consistencies between
#'   the signal channels as possible.
#'
#' @param object Object of the `egm` class, which includes header, signal
#'   information, and annotation information.
#'
#' @param by A `character` string naming waveform type to segment by. Options
#'   include the following:
#'
#'   * sinus = Will call [segment_by_sinus()] on `egm` object
#'
#' @param pad `character` String to specify which side of sequence to pad (or
#'   both). Options include `c("before", "after", "both")`.
#'
#'   Default is *before*. If *center* is being used, then the this argument is
#'   ignored.
#'
#' @param pad_length Offers padding of the segmented beats to a maximum length,
#'   as an `integer`. The default is *0L*, which means no padding will be
#'   applied. If `pad > 0` then will add the baseline value (specified within
#'   the header of the signal) to either before or after the signal. You can
#'   also choose to `center` the sequence, which will also only occur if `pad >
#'   0`. I.e., if `pad = 500` then each segmented object will be increased TO a
#'   max length of `500`. If the maximum size is larger than the padding size,
#'   then a warning will be issued and the sequence will be truncated.
#'
#' @param center A single Roman alphabetic letter `character` that utilizes the
#'   annotations given in the `egm` object to center the sequence. This is found
#'   under the __type__ variable in the annotation table.
#'
#'   For example, if sinus waveforms were annotated as `c("P", "R", "T")` at
#'   their peak, then could center around *R*. This will only occur if `pad >
#'   0L`. This is case-insensitive. The amount of padding will be determined by
#'   the __pad_length__ argument
#'
#' @name segmentation
#' @export
segmentation <- function(object,
												 by = "sinus",
												 pad = "before",
												 pad_length = 0L,
												 center = NULL) {

	stopifnot('Requires object of <egm> class for evaluation'
						= inherits(object, 'egm'))

	# Choose based on waveform segmentation request
	switch(by,
				 sinus = {
				 	segments <- segment_by_sinus(object)
				 })

	# Padding without signal rearrangement
	if (pad_length > 0 & is.null(center)) {
		# Checks happen internally
		segments <- lapply(segments, function(.x) {
			pad_sequence(.x, pad = pad, pad_length = pad_length)
		})


	}

	# Center the waveform of interest if needed
	if (!is.null(center) & pad_length > 0) {
		segments <- lapply(segments, function(.x) {
			center_sequence(.x, center = center, pad_length = pad_length)
		})
	}

	# Return
	segments

}

#' @rdname segmentation
#' @export
segment_by_sinus <- function(object) {

	lifecycle::signal_stage("experimental", "segment_by_sinus()")

	# Global variables to be nulled
	. <- number <- beat <- NULL

	# Eventually want to return ECG into a 12 x n matrix
	# Will need that digital signal for ML purposes
	# First need to identify the okay beats (ignoring first and last beats)

	# Get individual data
	sig <- copy(object$signal)
	hea <- copy(object$header)
	ann <- copy(object$annotation)
	type <- attributes(ann)$annotator
	stopifnot('Currently only supports `ecgpuwave` as the annotator' =
							type == 'ecgpuwave')


	# Find all RR intervals for the normal beats
	n <- as.data.table(sig[, .(sample)])
	rp <- ann[type == 'N', ]$sample

	# If there is only one beat, its unlikely to be sinus
	# Would consider it a PVC or AF
	# Will error here if there is only one beat?
	rr <- findInterval(n$sample, rp)
	n$rr <- rr

	# Trim n to the least it can be (removing first and last intervals)
	n <- n[rr > 0 & rr < max(rr), ]
	n$beat <- 0

	# The strategy to label beats will be
	# 	P waves must exist in previous RR_(n-1)
	# 		Each P must exist within an RR interval
	# 	T waves must exist in next RR_(n+1)
	# 	First QRS and last QRS may not be fully represented, thus must be cut

	# P waves (onset)
	pp <- ann[type == '(' & number == 0, ]$sample
	pp <- pp[pp %in% n$sample]

	# T waves (offset)
	tp <- ann[type == ')' & number == 2, ]$sample
	tp <- tp[tp %in% n$sample]

	# For each interval, do the rules apply?
	for (i in 1:(max(n$rr) - 1)) {

		# P wave exists in preceding RR interval
		# Get latest P wave, closest to QRS
		s1 <- n[rr == i, ]$sample
		if (any(pp %in% s1)) {
			p <- max(pp[pp %in% s1], na.rm = TRUE)
		} else {
			p <- -1
		}

		# T wave must exist subsequently to the QRS complex
		s2 <- n[rr == i + 1, ]$sample
		if (any(tp %in% s2)) {
			t <- min(tp[tp %in% s2], na.rm = TRUE)
		} else {
			t <- -1
		}

		# Find R that exists appropriately within this window
		# If it does exist, and this is a valid window, as we would expect
		# Then we can make this as a beat
		# But first, have to have an appropriate window
		if (p != -1 & t != -1) {
			s3 <- p:t
			if (any(rp %in% s3)) {
				n[, beat := fifelse(sample %in% s3, i, beat)]
			}
		}

	}

	# We now have a series of beats that we can turn back into a list
	# Must convert back to simple EGM using original object data
	# The sample numbers should be pulled from the original signal table
	beats <- unique(n[beat > 0, beat])
	beatList <- list()

	for (i in beats) {

		# Get range
		start <- min(n[beat == i, sample], na.rm = TRUE)
		stop <- max(n[beat == i, sample], na.rm = TRUE)

		# Signal data filtered down
		beatSignal <- sig[sample >= start & sample <= stop, ]

		# Header data simplified
		beatHeader <- header_table(
			record_name = attributes(hea)$record_line$record_name,
			number_of_channels = attributes(hea)$record_line$number_of_channels,
			frequency = attributes(hea)$record_line$frequency,
			samples = nrow(beatSignal),
			ADC_gain = hea$gain,
			label = hea$label,
			info_strings = attributes(hea)$info_strings
		)

		# Annotation data to just this beat
		beatAnnotation <- ann[sample >= start & sample <= stop, ]

		# New beat!
		beatList[[i]] <-
			egm(signal = beatSignal,
					header = beatHeader,
					annotation = beatAnnotation)

	}

	# Return a list of beats
	beatList

}

#' @rdname segmentation
#' @export
pad_sequence <- function(object, pad, pad_length) {

	stopifnot('Requires object of `<egm>` class for evaluation'
						= inherits(object, 'egm'))

	# Needs correct side argument
	stopifnot(
		"The `pad` argument is not valid"
		= pad %in% c("before", "after", "both")
	)

	# Get max sample length from object
	hea <- object$header
	ann <- object$annotation
	sig <- object$signal
	maxLength <- attributes(hea)$record_line$samples

	# Get properties for padding
	delta <- pad_length - maxLength
	# Check if truncation will occur
	if (delta < 0) {
		warning("Truncation will occur as there are ", maxLength, " samples with `pad` set to ", pad_length, ".")
		truncatingLength <- maxLength - pad_length
		if (pad == "before") {
			padSignal <- sig[(truncatingLength + 1):maxLength, ]
		} else if (pad == "after") {
			padSignal <- sig[1:pad_length, ]
		} else if (pad == "both") {
			truncatingLeft <- ceiling(truncatingLength / 2)
			truncatingRight <- maxLength - floor(truncatingLength / 2)
			padSignal <- sig[(truncatingLeft + 1):truncatingRight, ]
		}


	} else if (delta >= 0) {

		if (pad == "before") {
			left <- delta
			right <- 0
		} else if (pad == "after") {
			left <- 0
			right <- delta
		} else if (pad == "both") {
			left <- ceiling(delta / 2)
			right <- floor(delta / 2)
		}

		nm <- names(sig)

		before <- stats::setNames(data.table(matrix(nrow = left, ncol = length(nm))), nm)
		before[is.na(before), ] <- 0
		before$sample <- (min(sig$sample) - left):(min(sig$sample) - 1)

		after <- stats::setNames(data.table(matrix(nrow = right, ncol = length(nm))), nm)
		after[is.na(after), ] <- 0
		after$sample <- (max(sig$sample) + 1):(max(sig$sample) + right)

		if (pad %in% c("before", "both")) {
			before[left,
						 names(before[, -1]) :=
						 	ceiling(sig[sample == min(sample), .SD, .SDcols = nm[-1]] / 2)]
		} else if (pad %in% c("both", "after")) {
			after[1,
						names(after[, -1]) :=
							floor(sig[sample == max(sample), .SD, .SDcols = nm[-1]] / 2)]
		}

		# Remake signal now after padding
		padSignal <- rbindlist(list(before, sig, after))
	}

	# Update header with new data count
	padHeader <- header_table(
		record_name = attributes(hea)$record_line$record_name,
		number_of_channels = attributes(hea)$record_line$number_of_channels,
		frequency = attributes(hea)$record_line$frequency,
		samples = nrow(padSignal),
		ADC_gain = hea$gain,
		label = hea$label,
		info_strings = attributes(hea)$info_strings
	)

	# Update annotation file if padded or truncation occurred
	padAnn <- copy(ann)
	padAnn[sample >= min(padSignal$sample) & sample <= max(padSignal$sample), ]

	# Return the new EGM subclass
	egm(signal = signal_table(padSignal), header = padHeader, annotation = padAnn)

}

#' @export
#' @rdname segmentation
center_sequence <- function(object, center, pad_length) {

	stopifnot('Requires object of <egm> class for evaluation'
						= inherits(object, 'egm'))

	# Global variables
	type <- NULL

	# Center the waveform of interest if needed
	# If centering, waveform type of segmentation is needed
	annTypes <- unique(object$annotation$type)
	waveTypes <- annTypes[toupper(annTypes) %in% LETTERS]
	stopifnot(
		"The `center` variable was not available in the annotation set."
		= (toupper(center) %in% toupper(waveTypes))
		)
	centerWave <- toupper(center)[toupper(center) %in% toupper(waveTypes)]

	# Attributes of object
	hea <- object$header
	sig <- object$signal
	ann <- object$annotation

	# Only ONE annotation should be present in the signal for centering
	stopifnot(
		"Centering fails if the waveform of interest is not unique in sample."
		= nrow(ann[type == toupper(centerWave) | type == tolower(centerWave)]) == 1
	)

	# Now, can center by placing annotation in middle and surrounding by padding
	nm <- names(sig)
	cenInd <- ann[type == centerWave, sample]
	left <- ceiling(pad_length / 2)
	right <- floor(pad_length / 2)

	# Must do the left side, only adding hte needed number of rows before
	# Inclusive of the center index
	leftSig <- sig[sample > cenInd - left & sample <= cenInd, ]
	before <- stats::setNames(data.table(matrix(
		nrow = left - nrow(leftSig), ncol = length(nm)
	)), nm)
	before[is.na(before), ] <- 0
	before$sample <- (cenInd - left + 1):(min(leftSig$sample) - 1)

	# Now the right sided, only adding needed rows afterwards
	rightSig <- sig[sample > cenInd & sample <= cenInd + right, ]
	after <- stats::setNames(data.table(matrix(
		nrow = right - nrow(rightSig), ncol = length(nm)
	)), nm)
	after[is.na(after), ] <- 0
	after$sample <- (max(rightSig$sample) + 1):(cenInd + right)

	# Combine all the sides
	centerSignal <- rbindlist(list(before, leftSig, rightSig, after))

	# Update header with new data count
	centerHeader <- header_table(
		record_name = attributes(hea)$record_line$record_name,
		number_of_channels = attributes(hea)$record_line$number_of_channels,
		frequency = attributes(hea)$record_line$frequency,
		samples = nrow(centerSignal),
		ADC_gain = hea$gain,
		label = hea$label,
		info_strings = attributes(hea)$info_strings
	)

	# Update annotation file if padded or truncation occurred
	# Likely to not change annotations, but to be consistent
	centerAnn <- copy(ann)
	centerAnn[sample >= min(centerSignal$sample) & sample <= max(centerSignal$sample), ]

	# Return EGM object
	egm(
		signal = signal_table(centerSignal),
		header = centerHeader,
		annotation = centerAnn
	)

}
