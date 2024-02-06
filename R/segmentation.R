#' Segmentation of electrical signal by wave specifications
#'
#' @details
#' Requires a 12-lead ECG that has been digitized, and input as an `egm` object.
#' This object must have an annotation file associated with it that contains
#' demarcation of **P** waves, **R** waves, and **T** waves.
#'
#' @return
#' Returns a list of `<egm>` objects. Each item is a segmentation of an `<egm>`,
#' using the selected channels (if available). It will attempt to optimize and
#' pick the best annotations to help create consistencies between the signal
#' channels as possible.
#'
#' @param object Object of the `<egm>` class, which includes header, signal
#'   information, and annotation information.
#'
#' @param by <character> string naming waveform type to segment by. Options
#'   include the following:
#'
#'   * sinus = Will call [segment_sinus_beats()] on `egm` object
#'
#' @param pad <integer> Offer padding of the segmented beats to a maximum
#'   length. The default is `0L`, which means no padding will be applied. If
#'   `pad > 0` then will add the baseline value (specified within the header of
#'   the signal) to a `side` of the beat. You can also choose to `center` the
#'   sequence, which will also only occur if `pad > 0`. For example, if `pad =
#'   500`, then each segmented object will be of `500` length, even if it
#'   requires truncation (a warning message will be given).
#'
#' @param side <character> String to specify which side of sequence to pad (or
#'   both). Options include `c("right", "left", "both")`. Will only apply if
#'   `pad` is greater than `0L`.
#'
#'   Default is `right`. If `center` is being used, then the this argument is
#'   ignored.
#'
#' @param center <character> String that utilizes the annotations given in the
#'   `<egm>` object to center the sequence. This is found under the **type**
#'   variable in the annotation table. For example, if sinus waveforms were
#'   annotated as `c("P", "R", "T")` at their peak, then could center around
#'   `"R"`. This will only occur if `pad > 0L`. This is case-insensitive.
#'
#' @name segmentation
#' @export
segmentation <- function(object, by = "sinus", pad = 0L, side = "right", center = NULL) {

	stopifnot('Requires object of `egm` class for evaluation'
						= inherits(object, 'egm'))

	# Choose based on waveform segmentation request
	switch(by,
				 sinus = {
				 	segments <- segment_sinus_beats(object)
				 })

	# Padding and centering
	if (pad > 0) {
		# Needs correct side argument
		stopifnot(
			"The `side` argument is not valid"
			= side %in% c("right", "left", "both")
		)

		# Get max samples
		segLengths <- sapply(segments, function(.x) {
			.h <- .x$header
			attributes(.h)$record_line$samples
		})
		maxLength <- ceiling(max(segLengths))

		# Check if truncation will occur
		if (maxLength > pad) {
			warning("Truncation will occur with padding. Consider increasing `pad`.")
		}

		if (!is.null(center)) {
			# If centering, waveform type of segmentation is needed
			annTypes <- unique(object$annotation$type)
			waveTypes <- annTypes[toupper(annTypes) %in% LETTERS]
			stopifnot(
				"The `center` variable was not available in the annotation set."
				= (toupper(center) %in% toupper(waveTypes)
				)
			centerWave <- toupper(center)[toupper(center) %in% toupper(waveTypes)]


		}


	}

	# If padding...
	if (pad) {

		# Get max samples
		beatLengths <- sapply(segments, function(.x) {
			.h <- .x$header
			attributes(.h)$record_line$samples
		})
		maxLength <- ceiling(max(beatLengths) * 1.05)

		paddedBeats <- lapply(beats, function(.x) {

			.h <- .x$header
			.s <- .x$signal
			.a <- .x$annotation

			delta <- maxLength - attributes(.h)$record_line$samples
			left <- ceiling(delta / 2)
			right <- floor(delta / 2)

			# Need to create a before and after data table to eventually merge
			nm <- names(.s)

			before <- setNames(data.table(matrix(nrow = left, ncol = length(nm))), nm)
			before[is.na(before), ] <- 0
			before$sample <- (min(.s$sample) - left):(min(.s$sample) - 1)

			after <- setNames(data.table(matrix(nrow = right, ncol = length(nm))), nm)
			after[is.na(after), ] <- 0
			after$sample <- (max(.s$sample) + 1):(max(.s$sample) + right)

			# Average out the term to help be less sharp..
			before[left,
						 names(before[, -1]) :=
						 	ceiling(.s[sample == min(sample), .SD, .SDcols = nm[-1]] / 2)]
			after[1,
						names(after[, -1]) :=
							floor(.s[sample == max(sample), .SD, .SDcols = nm[-1]] / 2)]

			# Remake signal now after padding
			padSignal <- rbindlist(list(before, .s, after))

			# Update header with new data count
			padHeader <- header_table(
				record_name = attributes(.h)$record_line$record_name,
				number_of_channels = attributes(.h)$record_line$number_of_channels,
				frequency = attributes(.h)$record_line$frequency,
				samples = nrow(padSignal),
				ADC_gain = .h$gain,
				label = .h$label,
				info_strings = attributes(.h)$info_strings
			)

			egm(signal = padSignal, header = padHeader, annotation = .a)

		})

		beats <- paddedBeats
	}

	# Return
	beats

}

#' @describeIn segmentation Identify individual sinus beats on surface ECG and
#'   extract as individual beats, returning a list of sinus beats in the form of
#'   the `egm` class. a consistent **P**, **R**, and **T** wave amongst all
#'   channels. If a channel does not have, for example, a visible **T** wave, it
#'   will still label it as information gained from other channels.
#' @export
segment_sinus_beats <- function(object) {

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



