# Kors regression transformation ---------------------------

## code to prepare `kors` dataset goes here

# Maps the eight independent leads of the 12-lead ECG onto the orthogonal X, Y,
# Z axes of the Frank system. III, aVR, aVL and aVF are exact linear
# combinations of I and II, so they carry no information the other eight do not.
#
# Kors JA, van Herpen G, Sittig AC, van Bemmel JH (1990). Reconstruction of the
# Frank vectorcardiogram from standard electrocardiographic leads: diagnostic
# comparison of different methods. Eur Heart J 11(12):1083-1092.

# Rows are X (left), Y (inferior), Z (posterior)
kors <- rbind(
  X = c(0.38, -0.07, -0.13, 0.05, -0.01, 0.14, 0.06, 0.54),
  Y = c(-0.07, 0.93, 0.06, -0.02, -0.05, 0.06, -0.17, 0.13),
  Z = c(0.11, -0.23, -0.43, -0.06, -0.14, -0.20, -0.11, 0.31)
)

colnames(kors) <- c("I", "II", "V1", "V2", "V3", "V4", "V5", "V6")

# Exported data ---------------------------------------------

usethis::use_data(kors, overwrite = TRUE)
