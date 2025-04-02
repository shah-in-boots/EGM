#' Calculate Median beat
#' 
#' @description This function finds the median beat of an ECG. This can be performed on a single lead or on a 12 lead ECG. 
#' 
#' @param object Object of class 'egm'. 'annotation' subset may have a single lead of annotation, or 12 leads
#' 
#' @param total_length Optional. Total length of resulting median beat. Padded with 0s.

# Median beat for EGM package:
find_median_beat <- function(object, total_length = NULL) {
  
  frequency <- attributes(object$header)$record_line$frequency
  annotation <- object$annotation
  signal <- object$signal
  
  # Remove 'sample' column if it exists
  if (any(colnames(signal) %in% "sample")) {
    signal <- subset(signal, select = -which(colnames(signal) %in% "sample"))
  }
  
  # Find names of leads
  lead_names <- names(signal)
  
  # Predefine output
  output <- list()
  
  for (lead in 1:ncol(signal)) {
    signal_single_lead <- signal[[lead]]
    
    # If there are separate annotations for each lead, switch to correct lead
    #   Could be useful when using 12 lead annotations
    if (any(class(annotation) == 'list')) {
      annotation_single_lead <- annotation[[lead]]
    } else {
      annotation_single_lead <- annotation
    }
    
    # Find onsets/offsets:
    
    # Find the indices of the onset "(" preceding a "p" in 'type'
    pwave_indices <- which(annotation_single_lead$type == "p") - 1
    # Verify onset
    pwave_indices <- pwave_indices[annotation_single_lead$type[pwave_indices] == "("]
    
    # Find the indices of the offset ")" following a "t" in 'type'
    twave_indicies <- which(annotation_single_lead$type == "t") + 1
    # Verify offset
    twave_indicies <- twave_indicies[annotation_single_lead$type[twave_indicies] == ")"]
    
    
    
    # If the start of the ECG contains a t-wave without paired p-wave, remove
    if (twave_indicies[1] < pwave_indices[1]) {
      twave_indicies <- twave_indicies[-1]
    }
    
    # If the end of the ECG contains a p-wave without paired t-wave, remove
    if (pwave_indices[length(pwave_indices)] > twave_indicies[length(twave_indicies)]) {
      pwave_indices <- pwave_indices[-c(length(pwave_indices))]
    }
    
    beats <- data.frame(Onset = annotation_single_lead$sample[pwave_indices], Offset = annotation_single_lead$sample[twave_indicies])
    
    # Verify there is a QRS ('N') between each p and t. If not, remove row
    #   Can be used to filter PVCs
    qrs_indices <- annotation_single_lead$sample[annotation_single_lead$type == "N"]
    rows_to_keep <- apply(beats, 1, function(row) {
      any(qrs_indices > row['Onset'] & qrs_indices < row['Offset'])
    })
    beats <- beats[rows_to_keep, ]
    
    # Find R peaks using detect_QRS(). Should not assume 'N' index is Rpeak
    Rpeaks <- EGM::detect_QRS(signal_single_lead, frequency = frequency)
    # Match the Rpeaks to their corresponding P-T interval
    beats$Rpeak <- NA
    for (i in 1:nrow(beats)) {
      Rpeak <- Rpeaks[Rpeaks > beats$Onset[i] & Rpeaks < beats$Offset[i]]
      
      if (length(Rpeak == 1)) {
        beats$Rpeak[i] <- Rpeak
      } else if (length(Rpeak > 1)) {
        # If there are multiple Rpeak values within a P-T interval, choose the one with the greatest value, and warn user
        final_candidate <- Rpeak[which.max(signal_single_lead[Rpeak])]
        # If there is no Rpeak in P-T interval, remove row
        warning(
          'Multiple RPeaks found in beat No.',
          i,
          ' Choosing index ',
          final_candidate,
          ' from indices ',
          paste(Rpeak, collapse = ', ')
        )
      } else {
        warning('No Rpeak values found in beat No. ', i, '. Removing beat')
      }
    }
    beats <- beats[!is.na(beats$Rpeak), ]
    
    
    # Build median beat:
    
    # Find maximum window size, if pad_length is not specified:
    max_onset_length <- max(beats$Rpeak - beats$Onset)
    max_offset_length <- max(beats$Offset - beats$Rpeak)
    window <- max_onset_length + max_offset_length + 1
    
    # Build signal matrix:
    aligned_matrix <- array(NA, c(nrow(beats), window))
    for (i in 1:nrow(beats)) {
      onset <- beats$Rpeak[i] - max_onset_length
      offset <- beats$Rpeak[i] + max_offset_length
      aligned_matrix[i, ] <- signal_single_lead[onset:offset]
    }
    
    # Create median beat
    median_beat <- apply(aligned_matrix, 2, median, na.rm = TRUE)
    
    # Pad with zeros if needed
    if (!missing(total_length) && !is.null(total_length)) {
      if (length(median_beat < total_length)) {
        # Calculate padding lengths
        pad_start <- floor((total_length - length(median_beat)) / 2)
        pad_end <- ceiling((total_length - length(median_beat)) / 2)
        
        # Create the padded vector
        median_beat <- c(rep(0, pad_start), median_beat, rep(0, pad_end))
      } else if (median_beat > total_length) {
        warning(
          'Warning: Median beat is longer than the requested total length. Returning full length vector'
        )
      }
    }
    
    # Output list of median beats
    output[lead] <- list(median_beat)
    names(output)[lead] <- lead_names[lead]
  }
  return(output)
}
