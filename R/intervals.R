#' Finds peaks that represent near-field signal
#' @param hz the frequency that the signal was sampled on. Default is 1000/sec.
#' @export
find_major_peaks <- function(x, hz = 1000L, sensitivity = .2) {
	stopifnot("Should be a double vector" =
							typeof(x) == "double")

	if (inherits(x, "eps")) {
		hz <- attr(x, "frequency")
	}

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
