#' Finds peaks that represent near-field signal
#' @param hz the frequency that the signal was sampled on. Default is 1000/sec.
#' @name intervals
#' @export
find_major_peaks <- function(x, hz = 1000L, sensitivity = .2) {

	y <- as.numeric(x)

	# Major peaks physiologically are not usually faster than 200 ms
	# For margin of safety, will minimize this to 100 ms
	min_dist <- 0.1 * hz

	# The distance between signals should be relatively small, like 20-30 ms
	min_width <- 0.02 * hz

	# Major peak height should not vary too much for EGMs
	# Sensitivity should be set by the user
	min_ht <- sensitivity * max(abs(y), na.rm = TRUE)

	# Get all the individual peak locations and return them as index of position
	# Position as part of initial vector
	pks <-
		gsignal::findpeaks(
			data = y,
			MinPeakDistance = min_dist,
			MinPeakWidth = min_width,
			MinPeakHeight = min_ht,
			DoubleSided = TRUE
		)

	# Return list
	list(peaks = pks$pks, location = pks$loc)

}

#' Find minor peaks that may be present as near-field signal
#' @rdname intervals
#' @export
find_minor_peaks <- function(x, hz = 1000L, sensitivity = 0.2, specificity = 0.8) {

	# Major peaks physiologically are not usually faster than 200 ms
	# However, minor peaks may occur much closer to a major peak
	min_dist <- 0.1 * hz

	# The distance between signals should be relatively small, like 20-30 ms
	min_width <- 0.03 * hz

	# Major peak height should not vary too much for EGMs
	# Minor peaks must be "shorter" than major peaks by some percent
	max_ht <- specificity * max(abs(x), na.rm = TRUE)

	# Minimum height is to remove artifact
	min_ht <- sensitivity * max(abs(x), na.rm = TRUE)

	# Get all the individual peak locations and return them as index of position
	# Position as part of initial vector
	pks <-
		gsignal::findpeaks(
			data = y,
			MinPeakDistance = min_dist,
			MinPeakWidth = min_width,
			MinPeakHeight = min_ht,
			DoubleSided = TRUE
		)


	# Return list
	list(peaks = pks$pks, location = pks$loc)

}
