test_that("ECG class can be created", {
  # Create a simple ECG object with minimal data
  lead_names <- c(
    "I",
    "II",
    "III",
    "AVR",
    "AVL",
    "AVF",
    "V1",
    "V2",
    "V3",
    "V4",
    "V5",
    "V6"
  )
  sample_data <- as.data.frame(matrix(rnorm(1200), nrow = 100))
  colnames(sample_data) <- lead_names

  sig <- signal_table(sample_data)
  hea <- header_table(
    record_name = "test_ecg",
    number_of_channels = 12,
    frequency = 500,
    samples = 100,
    label = lead_names
  )

  ecg_obj <- ECG(signal = sig, header = hea)

  # Test class inheritance
  expect_s3_class(ecg_obj, "ECG")
  expect_s3_class(ecg_obj, "EGM")
  expect_s3_class(ecg_obj, "list")

  # Test structure
  expect_length(ecg_obj, 3)
  expect_true(is_ECG(ecg_obj))
  expect_true(is_EGM(ecg_obj))

  # Test internal components
  expect_s3_class(ecg_obj$signal, 'signal_table')
  expect_s3_class(ecg_obj$header, 'header_table')
  # annotation is now always a list
  expect_type(ecg_obj$annotation, 'list')
})

test_that("ecg validation accepts standard lead names", {
  # Create data with standard lead names
  lead_names <- c(
    "I",
    "II",
    "III",
    "AVR",
    "AVL",
    "AVF",
    "V1",
    "V2",
    "V3",
    "V4",
    "V5",
    "V6"
  )
  sample_data <- as.data.frame(matrix(rnorm(1200), nrow = 100))
  colnames(sample_data) <- lead_names

  sig <- signal_table(sample_data)
  hea <- header_table(
    record_name = "test_ecg",
    number_of_channels = 12,
    frequency = 500,
    samples = 100,
    label = lead_names
  )

  # Should create without warnings
  expect_silent(ECG(signal = sig, header = hea))
})

test_that("ecg validation handles non-standard lead names with warnings", {
  # Create data with non-standard lead names
  lead_names <- c(
    "Lead_I",
    "Lead_II",
    "Lead_III",
    "aVR",
    "aVL",
    "aVF",
    "Chest1",
    "Chest2",
    "Chest3",
    "Chest4",
    "Chest5",
    "Chest6"
  )
  sample_data <- as.data.frame(matrix(rnorm(1200), nrow = 100))
  colnames(sample_data) <- lead_names

  sig <- signal_table(sample_data)
  hea <- header_table(
    record_name = "test_ecg",
    number_of_channels = 12,
    frequency = 500,
    samples = 100,
    label = lead_names
  )

  # Should create with warnings
  expect_warning(ECG(signal = sig, header = hea), "Non-standard ECG lead names")
})

test_that("ecg validation warns about incorrect lead count", {
  # Create data with wrong number of leads
  lead_names <- c("I", "II", "III", "AVR", "AVL", "AVF", "V1", "V2", "V3")
  sample_data <- as.data.frame(matrix(rnorm(900), nrow = 100))
  colnames(sample_data) <- lead_names

  sig <- signal_table(sample_data)
  hea <- header_table(
    record_name = "test_ecg",
    number_of_channels = 9,
    frequency = 500,
    samples = 100,
    label = lead_names
  )

  # Should create with warnings
  expect_warning(ECG(signal = sig, header = hea), "should contain 12 leads")
})

test_that("format and print methods work correctly", {
  # Create a simple ECG object
  lead_names <- c(
    "I",
    "II",
    "III",
    "AVR",
    "AVL",
    "AVF",
    "V1",
    "V2",
    "V3",
    "V4",
    "V5",
    "V6"
  )
  sample_data <- as.data.frame(matrix(rnorm(1200), nrow = 100))
  colnames(sample_data) <- lead_names

  sig <- signal_table(sample_data)
  hea <- header_table(
    record_name = "test_ecg",
    number_of_channels = 12,
    frequency = 500,
    samples = 100,
    label = lead_names
  )

  ecg_obj <- ECG(signal = sig, header = hea)

  # Check print output
  expect_output(print(ecg_obj), "Electrogram")
  expect_output(print(ecg_obj), "Type: 12 of 12 surface ECG leads")
})

test_that("as_ECG conversion works", {
  # Create a simple EGM object
  lead_names <- c(
    "I",
    "II",
    "III",
    "AVR",
    "AVL",
    "AVF",
    "V1",
    "V2",
    "V3",
    "V4",
    "V5",
    "V6"
  )
  sample_data <- as.data.frame(matrix(rnorm(1200), nrow = 100))
  colnames(sample_data) <- lead_names

  sig <- signal_table(sample_data)
  hea <- header_table(
    record_name = "test_egm",
    number_of_channels = 12,
    frequency = 500,
    samples = 100,
    label = lead_names
  )

  egm_obj <- EGM(signal = sig, header = hea)

  # Convert to ECG
  ecg_obj <- as_ECG(egm_obj)

  # Test class conversion
  expect_s3_class(ecg_obj, "ECG")
  expect_s3_class(ecg_obj, "EGM")

  # Test object structure preservation
  expect_equal(ecg_obj$signal, egm_obj$signal)
  expect_equal(ecg_obj$header, egm_obj$header)
  expect_equal(ecg_obj$annotation, egm_obj$annotation)
})

test_that("as_ECG conversion rejects non-egm objects", {
  # Try converting something that isn't an egm
  not_egm <- list(a = 1, b = 2)

  # Should error
  expect_error(as_ECG(not_egm), "must be of class 'EGM'")
})

test_that("surface lead matching handles separators", {
  expect_equal(
    EGM:::surface_leads(c("V 1", "V_1", "V-1", "II")),
    c(V1 = "V 1", V1 = "V_1", V1 = "V-1", II = "II")
  )

  # `[_\\s-]` in the default engine matches the letter "s", not whitespace, so
  # this used to be accepted by stripping a character out of the name
  expect_equal(EGM:::surface_leads(c("Vs1", "XYZ", "V1")), c(V1 = "V1"))

  # Order follows the record, not the canonical list
  expect_named(EGM:::surface_leads(c("V6", "I")), c("V6", "I"))
})

# Display order ----

test_that("the twelve leads are held in the AHA display sequence", {

  # I, II, III, aVR, aVL, aVF, V1-V6. Not alphabetical, which is what an
  # `ordered = TRUE` factor built without `levels =` silently gives instead -
  # it put AVF before I, so every sort and every facet came out wrong while
  # still claiming to be ordered.
  expect_equal(
    as.character(ecg_leads()),
    c("I", "II", "III", "AVR", "AVL", "AVF", paste0("V", 1:6))
  )

  # The levels are the sequence, so the order is the one written
  expect_true(is.ordered(ecg_leads()))
  expect_equal(levels(ecg_leads()), as.character(ecg_leads()))
  expect_equal(sort(ecg_leads()), ecg_leads())
  expect_lt(ecg_leads()[1], ecg_leads()[12])

  # The internal list the rest of the package reads is the same order
  expect_equal(as.character(EGM:::.leads$ECG), as.character(ecg_leads()))
  expect_equal(levels(EGM:::.leads$ECG), as.character(ecg_leads()))
})

test_that("catheter channels are ordered as written, not alphabetically", {

  # `DD 11-12` sorts before `DD 3-4` as text, so an ordered factor of these
  # without explicit levels is ordered by nothing meaningful
  leads <- EGM:::.leads
  for (catheter in names(leads)) {
    expect_equal(
      levels(leads[[catheter]]),
      as.character(leads[[catheter]]),
      info = catheter
    )
  }

  expect_equal(levels(EGM:::.source), as.character(EGM:::.source))

  # `.labels` drives the plot facet order, and takes the sequence from these
  expect_equal(
    head(as.character(EGM:::.labels), 12),
    as.character(ecg_leads())
  )
  expect_equal(levels(EGM:::.labels), as.character(EGM:::.labels))
})

test_that("the Cabrera sequence runs the frontal leads contiguously", {

  cabrera <- ecg_leads("cabrera")
  expect_equal(
    as.character(cabrera)[1:6],
    c("AVL", "I", "AVR", "II", "AVF", "III")
  )

  # Same twelve leads, and the precordials are untouched
  expect_setequal(as.character(cabrera), as.character(ecg_leads()))
  expect_equal(as.character(cabrera)[7:12], paste0("V", 1:6))
})

test_that("lead_factor puts any labelling onto the display order", {

  # Canonicalised the same way `as_ECG()` canonicalises, then ordered
  expect_equal(
    as.character(lead_factor(c("v2", "aVR", "II", "AV-L"))),
    c("V2", "AVR", "II", "AVL")
  )
  expect_equal(
    as.character(sort(lead_factor(c("V6", "I", "aVF", "V1")))),
    c("I", "AVF", "V1", "V6")
  )

  # A label that is not a surface lead is `NA` rather than dropped, so it stays
  # visible in whatever it was going to be plotted or sorted into
  mixed <- lead_factor(c("II", "CS 1-2"))
  expect_length(mixed, 2L)
  expect_true(is.na(mixed[2]))

  # Every record keeps the same levels by default, which is what lets a cohort
  # be compared; `drop` is for a plot of a subset
  expect_length(levels(lead_factor(c("V6", "I"))), 12L)
  expect_equal(levels(lead_factor(c("V6", "I"), drop = TRUE)), c("I", "V6"))

  expect_equal(levels(lead_factor("I", order = "cabrera")),
               as.character(ecg_leads("cabrera")))

  # Degenerate input keeps the levels rather than erroring
  expect_length(lead_factor(character()), 0L)
  expect_length(levels(lead_factor(character())), 12L)
  expect_equal(as.character(lead_factor(factor(c("aVF", "I")))), c("AVF", "I"))
})

test_that("as_ECG extracts the surface leads from an EP study", {
  study <- read_wfdb("egm", test_path())

  expect_message(
    ecg_obj <- suppressWarnings(as_ECG(study)),
    "dropping 11 other channel"
  )

  expect_s3_class(ecg_obj, "ECG")
  expect_equal(names(ecg_obj$signal), c("sample", "I", "III", "V1"))
  expect_equal(as.character(ecg_obj$header$label), c("I", "III", "V1"))
  expect_equal(
    attributes(ecg_obj$header)$record_line$number_of_channels,
    3
  )

  # Signal itself is untouched by the selection
  expect_equal(ecg_obj$signal$V1, study$signal$V1)
})

test_that("as_ECG refuses a record with no surface leads", {
  study <- read_wfdb("egm", test_path())
  intracardiac <- study
  intracardiac$signal <- study$signal[, !c("I", "III", "V1"), with = FALSE]

  expect_error(as_ECG(intracardiac), "No surface ECG leads")
})

test_that("as_ECG refuses channels that resolve to the same lead", {
  labels <- c("V1", "V 1", "II")
  data <- as.data.frame(matrix(rnorm(30), nrow = 10))
  colnames(data) <- labels

  object <- EGM(
    signal = signal_table(data),
    header = header_table(
      record_name = "ambiguous",
      number_of_channels = 3,
      frequency = 500,
      samples = 10,
      label = labels
    )
  )

  expect_error(as_ECG(object), "resolve to the same surface lead")
})

test_that("as_ECG renames leads canonically", {
  labels <- c("i", "ii", "V 1")
  data <- as.data.frame(matrix(rnorm(30), nrow = 10))
  colnames(data) <- labels

  object <- EGM(
    signal = signal_table(data),
    header = header_table(
      record_name = "aliased",
      number_of_channels = 3,
      frequency = 500,
      samples = 10,
      label = labels
    )
  )

  ecg_obj <- suppressWarnings(as_ECG(object))
  expect_equal(names(ecg_obj$signal), c("sample", "I", "II", "V1"))
  expect_equal(ecg_obj$signal$V1, object$signal$`V 1`)
})

test_that("as_ECG flags annotation channels it cannot renumber", {
  study <- read_wfdb("egm", test_path())
  study$annotation <- list(
    manual = annotation_table(
      annotator = "manual",
      time = "00:00:00.100",
      sample = 100L,
      type = "N",
      channel = 5L
    )
  )

  # The incomplete lead set also warns, so the warnings are collected rather
  # than matched one at a time
  raised <- character()
  withCallingHandlers(
    suppressMessages(as_ECG(study)),
    warning = function(w) {
      raised <<- c(raised, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_true(any(grepl("refer to the original record", raised)))
})

test_that("require_ECG enforces the leads an analysis needs", {
  study <- suppressWarnings(suppressMessages(
    as_ECG(read_wfdb("egm", test_path()))
  ))

  # A partial surface set is fine when no specific leads are demanded
  expect_s3_class(EGM:::require_ECG(study), "ECG")

  expect_error(
    EGM:::require_ECG(study, leads = c("I", "II"), what = "Test"),
    "Test requires the surface leads I, II; missing II"
  )
  expect_error(EGM:::require_ECG("not an EGM"), "class <EGM> or <ECG>")

  # An ECG that already satisfies the contract passes through untouched
  ecg_obj <- read_wfdb("muse-sinus", system.file("extdata", package = "EGM"))
  expect_equal(
    EGM:::require_ECG(ecg_obj, leads = "V6")$signal,
    ecg_obj$signal
  )
})

test_that("read_muse returns ECG object", {
  # Skip if file doesn't exist
  file <- system.file("extdata", "muse-sinus.xml", package = "EGM")
  if (file == "") {
    skip("Test MUSE file not available")
  }

  # Read ECG from MUSE
  ecg_obj <- read_muse(file)

  # Check class
  expect_s3_class(ecg_obj, "ECG")
  expect_s3_class(ecg_obj, "EGM")

  # Check structure
  expect_length(ecg_obj, 3)
  expect_s3_class(ecg_obj$signal, 'signal_table')
  expect_s3_class(ecg_obj$header, 'header_table')
})
