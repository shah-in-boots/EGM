## code to prepare `DATASET` dataset goes here

# Location of data
data_folder <- function() {
x <- sessionInfo()$running
if (grepl("mac", x)) {
file.path("/Users",
			"asshah4",
			"OneDrive - University of Illinois at Chicago",
			"data")
} else if (grepl("Windows", x)) {
file.path("C:/Users",
			"asshah4",
			"OneDrive - University of Illinois at Chicago",
			"data")
}
}
sample <- "m3916_avnrt.txt"
data_loc <- file.path(data_folder(), "signals", "lspro", sample)


usethis::use_data(DATASET, overwrite = TRUE)
