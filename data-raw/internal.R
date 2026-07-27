# Lead & Catheter Names -------------------------------------

## code to prepare `leads` dataset goes here

# Catheters/leads are specifically included

# Up to duodecapolar, with each lead pair being a biopole
bipoles <-
  paste0(seq(from = 1, to = 19, by = 2), "-", seq(from = 2, to = 20, by = 2))

# For non-numeric location of leads
lead_loc <- c(
  "D",
  "DIST",
  "DISTAL",
  "M",
  "MID",
  "MIDDLE",
  "P",
  "PROX",
  "PROXIMAL"
)

# These are display orders, so the levels must be the order written rather than
# whatever `factor()` sorts to. Without `levels =` an "ordered" factor orders
# alphabetically, which put AVF before I on the surface leads and DD 11-12
# before DD 3-4 on the duodecapolar - orders that are not wrong so much as
# meaningless, and silently so, since the factor still claims to be ordered.
in_order <- function(x) factor(x, levels = x, ordered = TRUE)

# The surface leads in the AHA/ACCF/HRS display sequence (Kligfield et al.
# 2007): the three bipolar limb leads, the three augmented limb leads, then the
# precordials left to right. This is the order PhysioNet's 12-lead databases are
# written in, and the order every ECG cart prints. The Cabrera sequence (aVL, I,
# -aVR, II, aVF, III) is the recognised alternative and is deliberately not what
# this is; see `ecg_leads()`, which offers both.
.ecg <- in_order(c("I", "II", "III", "AVR", "AVL", "AVF", paste0("V", 1:6)))

# Catheter channels run distal to proximal, which is top to bottom on a display
.hra <- in_order(rev(paste0("RA ", bipoles[1:2])))
.his <- in_order(rev(paste0("HIS ", c(bipoles[1:3], lead_loc))))
.cs <- in_order(rev(paste0("CS ", bipoles[1:5])))
.dd <- in_order(rev(paste0("DD ", bipoles[1:10])))
.rv <- in_order(rev(paste0("RV ", bipoles[1:2])))
.abl <- in_order(rev(paste0("ABL ", c(bipoles[1:2], lead_loc[c(1:3, 7:9)]))))

# Order patterns
charLabels <- unlist(lapply(
  list(.ecg, .hra, .his, .cs, .dd, .rv, .abl),
  as.character
))
.labels <- factor(charLabels, levels = charLabels, ordered = TRUE)
.leads <- list(
  ECG = .ecg,
  HRA = .hra,
  HIS = .his,
  CS = .cs,
  DD = .dd,
  RV = .rv,
  ABL = .abl
)
.source <- in_order(c("ECG", "HRA", "RA", "HIS", "CS", "DD", "RV", "ABL"))

# WFDB Annotations ------------------------------------------

## code to prepare WFDB annotation labels dataset

# WFDB annotation labels based on the WFDB Applications Guide
# Reference: Moody GB. WFDB Applications Guide. PhysioNet.
# Available at https://www.physionet.org/physiotools/wag/

# Standard WFDB annotation label store values (codes)
label_store <- c(
  0L,
  1L,
  2L,
  3L,
  4L,
  5L,
  6L,
  7L,
  8L,
  9L,
  10L,
  11L,
  12L,
  13L,
  14L,
  16L,
  18L,
  19L,
  20L,
  21L,
  22L,
  23L,
  24L,
  25L,
  26L,
  27L,
  28L,
  29L,
  30L,
  31L,
  32L,
  33L,
  34L,
  35L,
  36L,
  37L,
  38L,
  39L,
  40L,
  41L
)

# Single-character symbols used in WFDB annotation files
symbol <- c(
  " ",
  "N",
  "L",
  "R",
  "a",
  "V",
  "F",
  "J",
  "A",
  "S",
  "E",
  "j",
  "/",
  "Q",
  "~",
  "|",
  "s",
  "T",
  "*",
  "D",
  "\"",
  "=",
  "p",
  "B",
  "^",
  "t",
  "+",
  "u",
  "?",
  "!",
  "[",
  "]",
  "e",
  "n",
  "@",
  "x",
  "f",
  "(",
  ")",
  "r"
)

# Text mnemonics for annotation types
mnemonic <- c(
  "NOTANN",
  "NORMAL",
  "LBBB",
  "RBBB",
  "ABERR",
  "PVC",
  "FUSION",
  "NPC",
  "APC",
  "SVPB",
  "VESC",
  "NESC",
  "PACE",
  "UNKNOWN",
  "NOISE",
  "ARFCT",
  "STCH",
  "TCH",
  "SYSTOLE",
  "DIASTOLE",
  "NOTE",
  "MEASURE",
  "PWAVE",
  "BBB",
  "PACESP",
  "TWAVE",
  "RHYTHM",
  "UWAVE",
  "LEARN",
  "FLWAV",
  "VFON",
  "VFOFF",
  "AESC",
  "SVESC",
  "LINK",
  "NAPC",
  "PFUS",
  "WFON",
  "WFOFF",
  "RONT"
)

# Human-readable descriptions
description <- c(
  "Not an actual annotation",
  "Normal beat",
  "Left bundle branch block beat",
  "Right bundle branch block beat",
  "Aberrated atrial premature beat",
  "Premature ventricular contraction",
  "Fusion of ventricular and normal beat",
  "Nodal (junctional) premature beat",
  "Atrial premature contraction",
  "Premature or ectopic supraventricular beat",
  "Ventricular escape beat",
  "Nodal (junctional) escape beat",
  "Paced beat",
  "Unclassifiable beat",
  "Signal quality change",
  "Isolated QRS-like artifact",
  "ST change",
  "T-wave change",
  "Systole",
  "Diastole",
  "Comment annotation",
  "Measurement annotation",
  "P-wave peak",
  "Left or right bundle branch block",
  "Non-conducted pacer spike",
  "T-wave peak",
  "Rhythm change",
  "U-wave peak",
  "Learning",
  "Ventricular flutter wave",
  "Start of ventricular flutter/fibrillation",
  "End of ventricular flutter/fibrillation",
  "Atrial escape beat",
  "Supraventricular escape beat",
  "Link to external data (aux_note contains URL)",
  "Non-conducted P-wave (blocked APB)",
  "Fusion of paced and normal beat",
  "Waveform onset",
  "Waveform end",
  "R-on-T premature ventricular contraction"
)

# Create the annotation labels data frame
.surface_annotations <- data.frame(
  label_store = label_store,
  symbol = symbol,
  mnemonic = mnemonic,
  description = description,
  stringsAsFactors = FALSE
)

# Internal data ---------------------------------------------

usethis::use_data(
  .labels,
  .leads,
  .source,
  .surface_annotations,
  overwrite = TRUE,
  internal = TRUE
)
