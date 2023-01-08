test_that("can find peaks in a single channel", {
	df <- read_lspro(file = test_path("sample-egm.txt"))

	hz <- attr(df$I, "frequency")
	sig <- as.numeric(df$I)

	plot(sig, type = "l")
	plot(df$I, type = "l")
	plot(df$`CS 9-10`, type = "l")
	plot(df$`HIS D`, type = "l")

	peaks <-
		gsignal::findpeaks(
			sig,
			MinPeakDistance = 100,
			MinPeakWidth = 20,
			DoubleSided = TRUE
		)
	plot(sig, type = "l")
	points(peaks$loc, peaks$pks, col = "orange", pch = 1)

	# Subset data for an annotation channel
	ann <- dt[label == channel]
	t <- ann$index
	peaks <-
		gsignal::findpeaks(
			ann$mV,
			MinPeakHeight = quantile(ann$mV, probs = 0.99),
			MinPeakDistance = minimum_interval,
			DoubleSided = TRUE
		)

	# Find peaks and intervals, and trim data for mapping purposes
	pk_locs <- ann$time[peaks$loc]
	ints <- diff(peaks$loc)
	ints_locs <- pk_locs[1:length(ints)] + ints/(2 * hz)
	ht <- mean(abs(peaks$pks), na.rm = TRUE)


	dt_locs <- round(ints_locs*hz, digits = 0)
	dt_ann <- ann[dt_locs, ]
})
