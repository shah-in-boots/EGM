## code to prepare `leads` dataset goes here

# Catheters/leads are specifically included

# Up to duodecapolar, with each lead pair being a biopole
bipoles <-
	paste0(seq(from = 1, to = 19, by = 2), "-", seq(from = 2, to = 20, by = 2))

# For non-numeric location of leads
lead_loc <- c("D",
							"DIST",
							"DISTAL",
							"M",
							"MID",
							"MIDDLE",
							"P",
							"PROX",
							"PROXIMAL")

# Top to bottom should be from high to low, and then from left to right
.ecg <-
	factor(c("I", "II", "III", "AVF", "AVL", "AVR", paste0("V", 1:6)),
				 ordered = TRUE)
.hra <-
	factor(rev(paste("RA", bipoles[1:2])),
				 ordered = TRUE)
.his <-
	factor(rev(paste("HIS", c(bipoles[1:3], lead_loc))),
				 ordered = TRUE)
.cs <-
	factor(rev(paste("CS", bipoles[1:5])),
				 ordered = TRUE)
.dd <-
	factor(rev(paste("DD", bipoles[1:10])),
				 ordered = TRUE)
.rv <-
	factor(rev(paste("RV", bipoles[1:2])),
				 ordered = TRUE)
.abl <-
	factor(rev(paste("ABL", c(bipoles[1:2], lead_loc[c(1:3, 7:9)]))),
				 ordered = TRUE)

# Order patterns
.labels <- c(.ecg, .hra, .his, .cs, .dd, .rv, .abl)
.leads <- list(
	ECG = .ecg,
	HRA = .hra,
	HIS = .his,
	CS = .cs,
	DD = .dd,
	RV = .rv,
	ABL = .abl
)
.source <-
	factor(c("ECG", "HRA", "RA", "HIS", "CS", "DD", "RV", "ABL"), ordered = TRUE)

usethis::use_data(.labels,
									.leads,
									.source,
									.ecg,
									.hra,
									.his,
									.cs,
									.dd,
									.rv,
									.abl,
									overwrite = TRUE,
									internal = TRUE)
