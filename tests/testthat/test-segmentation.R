test_that("ECG segmentation works", {

	rec <- 'ecg'
	dir <- test_path()
	sig <- read_wfdb(rec, dir)
	hea <- read_header(rec, dir)
	ann <- read_annotation(rec, dir, 'ecgpuwave')
	ecg <- egm(sig, hea, ann)

})
