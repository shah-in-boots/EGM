# Helper shared by the transform tests below: a set of sinus P->T beats guided
# by lead 2, deliberately ragged in length.
make_sinus_windows <- function() {
  object <- read_wfdb("ecg-sinus", test_path(), "ann")
  suppressMessages(get_windows(object, by = by_rhythm(channel = 2)))
}

test_that("pad_window anchors the QRS at a common index", {

  windows <- make_sinus_windows()
  padded <- pad_window(
    windows, align = "feature", align_feature = "N", channel = 2
  )

  # All windows share one length, and their QRS annotations coincide
  pad_lengths <- vapply(padded, function(w) nrow(w$signal), integer(1))
  expect_length(unique(pad_lengths), 1)

  qrs_pos <- vapply(padded, function(w) {
    a <- EGM:::get_single_annotation(w)
    n <- a$sample[a$type == "N" & a$channel == 2L]
    if (length(n) > 0) n[1] else NA_integer_
  }, integer(1))
  expect_length(unique(qrs_pos[!is.na(qrs_pos)]), 1)

  # No signal is lost: the padded length spans the full feature extent
  expect_gte(pad_lengths[1], max(vapply(windows, function(w) nrow(w$signal), integer(1))))
})

test_that("pad_window places signal by alignment with absent edges", {

  windows <- make_sinus_windows()
  n1 <- nrow(windows[[1]]$signal)

  left <- pad_window(windows, target_samples = 800, align = "left")
  right <- pad_window(windows, target_samples = 800, align = "right")

  # Left alignment: real signal at the front, padding at the tail
  expect_equal(nrow(left[[1]]$signal), 800)
  expect_true(is.na(left[[1]]$signal$II[800]))
  expect_equal(left[[1]]$signal$II[seq_len(n1)], windows[[1]]$signal$II)

  # Right alignment: padding at the front, real signal at the tail
  expect_true(is.na(right[[1]]$signal$II[1]))
  expect_equal(
    right[[1]]$signal$II[(800 - n1 + 1):800],
    windows[[1]]$signal$II
  )

  # Padding marks the samples absent rather than asserting a potential of zero,
  # which would drag a median toward the origin at the edges of a beat
  expect_equal(
    sum(!is.na(left[[1]]$signal$II)),
    n1
  )
  expect_equal(pad_window(windows, target_samples = 800, pad_value = 0)[[1]]$signal$II[800], 0)
})

test_that("edge padding extends the nearest observed sample", {

  windows <- make_sinus_windows()
  n1 <- nrow(windows[[1]]$signal)
  edged <- pad_window(
    windows, target_samples = 800, align = "center", pad_value = "edge"
  )

  # Nothing absent, and the observed samples are untouched
  first <- edged[[1]]$signal$II
  expect_false(anyNA(first))
  place <- floor((800 - n1) / 2)
  expect_equal(first[place + seq_len(n1)], windows[[1]]$signal$II)

  # Both tails hold the value of the sample they extend, and nothing else
  expect_equal(unique(first[seq_len(place)]), first[place + 1L])
  expect_equal(unique(first[(place + n1 + 1L):800]), first[place + n1])

  # It is the fill that changes, not the geometry: the same alignment either way
  na_padded <- pad_window(windows, target_samples = 800, align = "center")
  expect_equal(
    which(!is.na(na_padded[[1]]$signal$II)),
    place + seq_len(n1)
  )
})

test_that("pad_value rejects what it cannot fill with", {

  windows <- make_sinus_windows()

  # A mistyped string would otherwise become `NA` through `as.numeric()` and
  # silently pass for the default
  expect_error(pad_window(windows, pad_value = "zero"), "must be")
  expect_error(pad_window(windows, pad_value = c(0, 1)), "must be")

  expect_silent(pad_window(windows, pad_value = NA))
  expect_silent(pad_window(windows, pad_value = 0))
  expect_silent(pad_window(windows, pad_value = "edge"))
})

# Baseline ----

test_that("baseline_window shifts the level and leaves the shape alone", {

  windows <- make_sinus_windows()
  corrected <- baseline_window(windows, reference = "start", channel = 2)

  before <- windows[[1]]$signal$II
  after <- corrected[[1]]$signal$II

  # A constant shift: every difference between neighbouring samples survives
  expect_equal(diff(before), diff(after))
  expect_length(unique(round(before - after, 9)), 1)

  # And the level it removed is the one it was asked for
  expect_equal(
    unique(round(before - after, 9)),
    stats::median(head(before, round(20 / 1000 * stats::frequency(windows[[1]]))))
  )

  # Each lead is corrected by its own level, not by one shared level
  levels <- vapply(
    setdiff(names(windows[[1]]$signal), "sample"),
    function(l) {
      unique(round(windows[[1]]$signal[[l]] - corrected[[1]]$signal[[l]], 9))[1]
    },
    numeric(1)
  )
  expect_gt(length(unique(levels)), 1)
})

test_that("baseline_window anchors on a fiducial and carries provenance", {

  windows <- make_sinus_windows()
  corrected <- baseline_window(
    windows, reference = list(type = "(", wave = "QRS"), channel = 2
  )

  expect_s3_class(corrected, "windows")
  expect_equal(tail(attr(corrected, "history"), 1), "baselined")
  expect_equal(
    nrow(EGM:::get_single_annotation(corrected[[1]])),
    nrow(EGM:::get_single_annotation(windows[[1]]))
  )

  # The isoelectric segment ends at the fiducial, so that is where the corrected
  # signal sits near zero
  at <- EGM:::locate_feature(
    EGM:::get_single_annotation(corrected[[1]]),
    list(type = "(", wave = "QRS"),
    2
  )
  span <- max(1L, at - 9L):at
  expect_lt(
    abs(stats::median(corrected[[1]]$signal$II[span])),
    abs(stats::median(windows[[1]]$signal$II[span]))
  )
})

test_that("baseline_window accepts a single beat, a level, and a missing anchor", {

  windows <- make_sinus_windows()
  beat <- median_window(windows, align_feature = "N", channel = 2)

  # A single EGM in, a single EGM out - the median beat is the object most
  # likely to be corrected, and it is not a collection
  one <- baseline_window(beat, reference = "start")
  expect_s3_class(one, "EGM")
  expect_equal(signal_units(one$signal), signal_units(beat$signal))

  # A numeric reference is subtracted as given
  flat <- baseline_window(beat, reference = 100)
  expect_equal(flat$signal$II, beat$signal$II - 100)

  # A window that cannot be anchored is returned uncorrected rather than shifted
  # by some other window's baseline
  blank <- windows
  blank[[1]]$annotation[[1]] <- blank[[1]]$annotation[[1]][0, ]
  expect_warning(
    left <- baseline_window(blank, reference = "(", channel = 2),
    "left uncorrected"
  )
  expect_equal(left[[1]]$signal$II, windows[[1]]$signal$II)

  expect_error(baseline_window(windows, width = 0), "positive")
})

test_that("baseline_window locates the fiducial by sample, not by row", {

  object <- read_wfdb("ecg-sinus", test_path(), "ann")
  reference <- list(type = "(", wave = "QRS")

  # Windows cut with `adjust_sample_indices = FALSE` keep the record's own
  # indices, so a fiducial at sample 4000 is not row 4000 of a 300-row window
  raw <- suppressMessages(get_windows(
    object,
    by = by_rhythm(channel = 2, adjust_sample_indices = FALSE)
  ))
  rebased <- suppressMessages(get_windows(object, by = by_rhythm(channel = 2)))
  expect_gt(raw[[2]]$signal$sample[1], 0)
  expect_equal(rebased[[2]]$signal$sample[1], 0)

  shift <- function(before, after) {
    unique(round(before[[2]]$signal$II - after[[2]]$signal$II, 9))
  }
  from_raw <- shift(raw, baseline_window(raw, reference, channel = 2))
  from_rebased <- shift(rebased, baseline_window(rebased, reference, channel = 2))

  expect_length(from_raw, 1)
  expect_equal(from_raw, from_rebased)

  # And the level is the span of samples ending just before the fiducial, the
  # fiducial's own sample excluded
  window <- rebased[[2]]
  row <- match(
    EGM:::locate_feature(EGM:::get_single_annotation(window), reference, 2),
    window$signal$sample
  )
  span <- round(20 / 1000 * stats::frequency(window))
  expect_equal(
    from_rebased,
    stats::median(window$signal$II[(row - span):(row - 1L)])
  )
})

test_that("median_window collapses windows to a single beat", {

  windows <- make_sinus_windows()

  # Ragged windows require an explicit alignment choice
  expect_error(median_window(windows), "differing lengths")

  beat <- median_window(windows, align_feature = "N", channel = 2)
  expect_s3_class(beat, "EGM")
  expect_true(is_EGM(beat))

  # One beat, all 12 leads retained, named after the source record
  leads <- setdiff(names(beat$signal), "sample")
  expect_length(leads, 12)
  expect_match(
    attributes(beat$header)$record_line$record_name,
    "_median$"
  )

  # Uniform-length input needs no alignment feature
  padded <- pad_window(windows, align = "feature", channel = 2)
  beat2 <- median_window(padded)
  expect_s3_class(beat2, "EGM")
  expect_equal(nrow(beat2$signal), nrow(padded[[1]]$signal))

  # The median lies within the beat-to-beat range at each sample, over the beats
  # that actually reach it
  mat <- vapply(padded, function(w) w$signal$II, numeric(nrow(padded[[1]]$signal)))
  expect_true(all(beat2$signal$II >= apply(mat, 1, min, na.rm = TRUE) - 1e-8))
  expect_true(all(beat2$signal$II <= apply(mat, 1, max, na.rm = TRUE) + 1e-8))

  # No sample is left missing: every position is reached by at least one beat
  expect_false(anyNA(beat2$signal$II))
})

test_that("the median beat carries the fiducials that produced it", {

  windows <- make_sinus_windows()
  aligned <- pad_window(
    windows, align = "feature", align_feature = "N", channel = 2
  )
  beat <- median_window(aligned)
  fiducials <- EGM:::get_single_annotation(beat)

  expect_s3_class(fiducials, "annotation_table")
  expect_gt(nrow(fiducials), 0)
  expect_named(beat$annotation, "ann")

  # Sample order is preserved, and every fiducial lands inside the beat
  expect_false(is.unsorted(fiducials$sample))
  expect_true(all(fiducials$sample >= 0 & fiducials$sample < nrow(beat$signal)))

  # Alignment put the QRS at one index in every window, so the median is that
  # same index rather than something near it
  aligned_qrs <- vapply(aligned, function(w) {
    a <- EGM:::get_single_annotation(w)
    a$sample[a$type == "N" & a$channel == 2L][1]
  }, integer(1))
  median_qrs <- fiducials$sample[fiducials$type == "N" & fiducials$channel == 2L]
  expect_equal(median_qrs, unique(aligned_qrs))

  # A fiducial that does vary is placed at the median of where it fell
  p_onsets <- vapply(aligned, function(w) {
    a <- EGM:::label_waves(EGM:::get_single_annotation(w))
    a$sample[a$type == "(" & a$wave == "P" & a$channel == 2L][1]
  }, integer(1))
  labelled <- EGM:::label_waves(fiducials)
  expect_equal(
    labelled$sample[
      labelled$type == "(" & labelled$wave == "P" & labelled$channel == 2L
    ],
    as.integer(stats::median(p_onsets))
  )

  # Repeated symbols are matched by their order within the window, so the beat
  # keeps a full P/QRS/T bracket structure rather than one collapsed onset
  lead_two <- fiducials[fiducials$channel == 2L, ]
  expect_equal(sum(lead_two$type == "("), 3L)
  expect_equal(sum(lead_two$type == ")"), 3L)
  expect_equal(lead_two$type, c("(", "p", ")", "(", "N", ")", "(", "t", ")"))
})

test_that("a median beat keeps its fiducials where every other EGM keeps them", {

  # `$annotation` is a *named list* of annotation tables on every `EGM`, so that
  # a record can carry more than one annotator; `get_annotation()` is what
  # unwraps the single-annotator case. A median beat is no exception, and reading
  # `nrow(beat$annotation)` gives `NULL` on it for the same reason it does on a
  # record straight off disk - which reads as "the fiducials were lost" and is
  # worth pinning as deliberate.
  source <- read_wfdb("ecg-sinus", test_path(), "ann")
  beat <- median_window(make_sinus_windows(), align_feature = "N", channel = 2)

  expect_type(source$annotation, "list")
  expect_type(beat$annotation, "list")
  expect_named(beat$annotation, names(source$annotation))
  expect_null(nrow(source$annotation))
  expect_null(nrow(beat$annotation))

  # And the accessor answers the same shape for both
  expect_s3_class(get_annotation(source), "annotation_table")
  expect_s3_class(get_annotation(beat), "annotation_table")
  expect_gt(nrow(get_annotation(beat)), 0)
})

test_that("the median beat says in its header what it is", {

  windows <- make_sinus_windows()
  beat <- median_window(windows, align_feature = "N", channel = 2)
  info <- attributes(beat$header)$info_strings

  expect_match(info$median_info, "median beat of 8 windows")

  # The source window's own info names a single window, which this is not
  expect_true(!is.null(attributes(windows[[1]]$header)$info_strings$window_info))
  expect_null(info$window_info)
})

test_that("a median of unannotated windows has no fiducials to report", {

  windows <- pad_window(
    make_sinus_windows(),
    align = "feature", align_feature = "N", channel = 2
  )
  bare <- lapply(windows, function(w) {
    w$annotation <- list(annotation_table())
    w
  })

  fiducials <- EGM:::get_single_annotation(median_window(bare))
  expect_s3_class(fiducials, "annotation_table")
  expect_equal(nrow(fiducials), 0)
})

test_that("a fiducial most windows lack is not part of their median", {

  windows <- pad_window(
    make_sinus_windows(),
    align = "feature", align_feature = "N", channel = 2
  )

  # One window alone carries an annotation of type "A"
  annotation <- EGM:::get_single_annotation(windows[[1]])
  annotation$type[1] <- "A"
  windows[[1]]$annotation <- list(ann = annotation)

  fiducials <- EGM:::get_single_annotation(median_window(windows))
  expect_false("A" %in% fiducials$type)
})

test_that("normalize_window stretches every window to a fixed length", {

  windows <- make_sinus_windows()
  normalized <- normalize_window(windows, target_samples = 400)

  expect_s3_class(normalized, "windows")
  norm_lengths <- vapply(normalized, function(w) nrow(w$signal), integer(1))
  expect_true(all(norm_lengths == 400))

  # Now uniform, they can be averaged directly into a median beat
  beat <- median_window(normalized)
  expect_equal(nrow(beat$signal), 400)
})

test_that("normalize_window centers align_feature on the guiding lead", {

  windows <- make_sinus_windows()

  target <- 300
  center_point <- floor((target - 1L) / 2L)

  guided <- normalize_window(
    windows,
    align_feature = "N",
    channel = 2,
    target_samples = target
  )

  # The guiding lead's QRS peak must land exactly at the window center in every
  # normalized beat, regardless of how the per-lead fiducials are ordered.
  ch2_pos <- vapply(
    guided,
    function(w) {
      a <- EGM:::get_single_annotation(w)
      n <- a$sample[a$type == "N" & a$channel == 2L]
      if (length(n) > 0) n[1] else NA_integer_
    },
    integer(1)
  )
  expect_true(all(ch2_pos == center_point))
  expect_equal(nrow(guided[[1]]$signal), target)
  expect_identical(guided[[1]]$signal$sample, 0:(target - 1L))

  # Without channel guidance the feature would align on whichever lead sorted
  # first, so multi-lead annotations are refused rather than resolved silently
  expect_error(
    normalize_window(windows, align_feature = "N", target_samples = target),
    "needs a guiding `channel`"
  )

  # preserve_class = FALSE drops back to a plain list of EGM objects
  plain <- normalize_window(windows, target_samples = target, preserve_class = FALSE)
  expect_false(is_window_set(plain))
  expect_s3_class(plain[[1]], "EGM")
})
