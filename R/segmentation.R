#' Segmentation of surface ECG into individual beats
#'
#' @details
#' Requires a 12-lead ECG that has been digitized, and input as an `egm` object.
#' This object must have an annotation file associated with it that contains
#' demarcation of **P** waves, **R** waves, and **T** waves.
#'
#' @return
#' Returns a single beat on all input channels (if available) as a list of `egm`
#' object. It will attempt to optimize and pick the best annotations to help
#' create a consistent **P**, **R**, and **T** wave amongst all channels. If a
#' channel does not have, for example, a visible **T** wave, it will still label
#' it as information gained from other channels.
#'
#' @param object Object of the `egm` class, which includes header (meta), signal
#'   information, and annotation information.
#' @rdname segmentation
#' @export
segment_ecg <- function() {

}



