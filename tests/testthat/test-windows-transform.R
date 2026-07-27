# Helper shared by the transform tests below: a set of sinus P->T beats guided
# by lead 2, deliberately ragged in length.
make_sinus_windows <- function() {
  object <- read_wfdb("ecg-sinus", test_path(), "ann")
  suppressMessages(get_windows(object, by = by_rhythm(channel = 2)))
}

test_that("pad_window anchors the QRS at a common index", {

  windows <- make_sinus_windows()
  padded <- pad_window(
    windows, align = "feature", align_feature = "N", channel_criteria = 2
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

test_that("median_window collapses windows to a single beat", {

  windows <- make_sinus_windows()

  # Ragged windows require an explicit alignment choice
  expect_error(median_window(windows), "differing lengths")

  beat <- median_window(windows, align_feature = "N", channel_criteria = 2)
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
  padded <- pad_window(windows, align = "feature", channel_criteria = 2)
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
    windows, align = "feature", align_feature = "N", channel_criteria = 2
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

test_that("the median beat says in its header what it is", {

  windows <- make_sinus_windows()
  beat <- median_window(windows, align_feature = "N", channel_criteria = 2)
  info <- attributes(beat$header)$info_strings

  expect_match(info$median_info, "median beat of 8 windows")

  # The source window's own info names a single window, which this is not
  expect_true(!is.null(attributes(windows[[1]]$header)$info_strings$window_info))
  expect_null(info$window_info)
})

test_that("a median of unannotated windows has no fiducials to report", {

  windows <- pad_window(
    make_sinus_windows(),
    align = "feature", align_feature = "N", channel_criteria = 2
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
    align = "feature", align_feature = "N", channel_criteria = 2
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
    channel_criteria = 2,
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

  # Without channel guidance the feature aligns on whichever lead sorts first,
  # so the guiding lead's peak is generally NOT centered.
  unguided <- normalize_window(
    windows,
    align_feature = "N",
    target_samples = target
  )
  ch2_pos_unguided <- vapply(
    unguided,
    function(w) {
      a <- EGM:::get_single_annotation(w)
      n <- a$sample[a$type == "N" & a$channel == 2L]
      if (length(n) > 0) n[1] else NA_integer_
    },
    integer(1)
  )
  expect_false(all(ch2_pos_unguided == center_point))

  # preserve_class = FALSE drops back to a plain list of EGM objects
  plain <- normalize_window(windows, target_samples = target, preserve_class = FALSE)
  expect_false(is_window_set(plain))
  expect_s3_class(plain[[1]], "EGM")
})
