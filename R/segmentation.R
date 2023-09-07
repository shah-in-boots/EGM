#' Segmentation of electrical signal by beat specifications
#'
#' @details
#' Requires a 12-lead ECG that has been digitized, and input as an `egm` object.
#' This object must have an annotation file associated with it that contains
#' demarcation of **P** waves, **R** waves, and **T** waves.
#'
#' @return
#' Returns a `list` of `egm` objects. Each item is a segmentation of an `egm`
#' object, using the selected channels (if available). It will attempt to
#' optimize and pick the best annotations to help create consistencies between
#' the signal channels as possible.
#'
#' @param object Object of the `egm` class, which includes header (meta), signal
#'   information, and annotation information.
#'
#' @name segmentation
NULL

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
		}

		# T wave must exist subsequently to the QRS complex
		s2 <- n[rr == i + 1, ]$sample
		if (any(tp %in% s2)) {
			t <- min(tp[tp %in% s2], na.rm = TRUE)
		}

		# Find R that exists appropriately within this window
		# If it does exist, and this is a valid window, as we would expect
		# Then we can make this as a beat
		s3 <- p:t
		if (any(rp %in% s3)) {
			n[, beat := fifelse(sample %in% s3, i, beat)]
		}

	}

	# We now have a series of beats that we can turn back into a list
	# Must convert back to simple EGM


}



