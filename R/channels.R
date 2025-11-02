#' @keywords internal
identify_channel_source <- function(x) {
  # Intakes character vector and identifies the source
  stopifnot("Not a known/supported channel yet." = x %in% .labels)

  # Find source of lead bipole
  for (i in names(.leads)) {
    if (x %in% .leads[[i]]) {
      y <- i
    }
  }

  # Return
  y
}
