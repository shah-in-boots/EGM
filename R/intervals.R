#' Finds peaks that represent near-field signal
#' @param hz the frequency that the signal was sampled on. Default is 1000/sec.
#' @export
find_major_peaks <- function(x, hz = 1000L) {
	stopifnot("Should be a double vector" =
							typeof(x) == "double")

	if (inherits(x, "eps")) {
		hz <- attr(x, "frequency")
	}

	# Major peaks physiologically are not usually faster than 200 ms
	# For margin of safety, will minimize this to 100 ms
	min_dist <- 0.1 * hz

	# The distance between signals should be relatively small, like 20-30 ms
	min_width <- 0.02 * hz

	# Get all the individual peak locations and return them as index of position
	# Position as part of initial vector
	gsignal::findpeaks(
		data = x,
		MinPeakDistance = min_dist,

	)


}
