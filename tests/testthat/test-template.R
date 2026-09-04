# Shared fixture: ragged sinus P-to-T beats guided by lead 2.
template_sinus_windows <- function() {
  object <- read_wfdb("ecg-sinus", test_path(), "ann")
  suppressMessages(get_windows(
    object,
    by = by_rhythm(channel = 2)
  ))
}

test_that("landmark is a validated S7 object", {
  point <- landmark(
    name = "QRS",
    criteria = list(type = "N"),
    channel = 2L,
    position = 178
  )
  expect_true(S7::S7_inherits(point, landmark))
  expect_equal(point@name, "QRS")
  expect_equal(point@channel, 2L)
  expect_equal(point@position, 178)

  unfitted <- landmark(name = "x", criteria = list(type = "N"))
  expect_true(is.na(unfitted@channel))
  expect_true(is.na(unfitted@position))

  expect_error(
    landmark(name = c("a", "b"), criteria = list(type = "N")),
    "single string"
  )
  expect_error(
    landmark(name = "x", criteria = list(1, 2)),
    "named list"
  )
})

test_that("learn_template learns a template from EGM examples", {
  windows <- template_sinus_windows()
  examples <- lapply(windows, identity)
  learned <- learn_template(
    examples,
    channel = 2,
    target_samples = 500
  )

  expect_true(is_template(learned))
  expect_true(S7::S7_inherits(learned, template))
  expect_equal(learned@method, "learned")
  expect_equal(learned@target_samples, 500L)
  expect_equal(learned@frequency, 500)

  names <- vapply(learned@landmarks, function(x) x@name, character(1))
  positions <- vapply(
    learned@landmarks,
    function(x) x@position,
    numeric(1)
  )
  expect_equal(names, c("P_onset", "QRS", "T_offset"))
  expect_false(is.unsorted(positions))
  expect_equal(positions[c(1, 3)], c(0, 499))
})

test_that("template supports direct manual construction", {
  manual <- template(
    landmarks = list(
      landmark(
        "onset",
        list(type = "(", wave = "P"),
        channel = 2,
        position = 0
      ),
      landmark("QRS", list(type = "N"), channel = 2, position = 300),
      landmark(
        "offset",
        list(type = ")", wave = "T"),
        channel = 2,
        position = 599
      )
    ),
    target_samples = 600
  )
  expect_true(is_template(manual))
  expect_equal(manual@method, "manual")
  expect_equal(
    vapply(manual@landmarks, function(x) x@position, numeric(1)),
    c(0, 300, 599)
  )
})

test_that("learn_template pins landmarks to their own channels", {
  learned <- learn_template(
    template_sinus_windows(),
    landmarks = list(
      QRS_ch1 = list(type = "N", channel = 1),
      QRS_ch2 = list(type = "N", channel = 2)
    ),
    target_samples = 400
  )
  expect_setequal(
    vapply(learned@landmarks, function(x) x@channel, integer(1)),
    c(1L, 2L)
  )
})

test_that("learn_template reports missing landmarks and invalid examples", {
  windows <- template_sinus_windows()
  expect_error(
    learn_template(
      windows,
      landmarks = list(ghost = list(type = "ZZZ")),
      channel = 2
    ),
    "never located"
  )
  expect_error(
    learn_template(list(), landmarks = list(QRS = list(type = "N"))),
    "non-empty list"
  )
})

test_that("template and landmark have print methods", {
  learned <- learn_template(template_sinus_windows(), channel = 2)
  expect_true(any(grepl("<template: learned>", capture.output(print(learned)))))
  expect_true(any(grepl(
    "<landmark:",
    capture.output(print(learned@landmarks[[1]]))
  )))
})

test_that("warp_window aligns beats to a learned template", {
  windows <- template_sinus_windows()
  learned <- learn_template(windows, channel = 2, target_samples = 500)
  warped <- warp_window(windows, learned)

  expect_s3_class(warped, "windows")
  expect_true(all(vapply(
    warped,
    function(x) nrow(x$signal) == 500L,
    logical(1)
  )))
  qrs_target <- vapply(
    learned@landmarks,
    function(x) if (x@name == "QRS") x@position else NA_real_,
    numeric(1)
  )
  qrs_target <- round(qrs_target[!is.na(qrs_target)])
  warped_qrs <- vapply(warped, function(x) {
    annotations <- EGM:::get_single_annotation(x)
    qrs <- annotations$sample[
      annotations$type == "N" & annotations$channel == 2L
    ]
    if (length(qrs) > 0L) qrs[1] else NA_integer_
  }, integer(1))
  expect_true(all(abs(warped_qrs[!is.na(warped_qrs)] - qrs_target) <= 1))
})

test_that("warp_window honours a manual template", {
  windows <- template_sinus_windows()
  manual <- template(
    landmarks = list(
      landmark(
        "onset",
        list(type = "(", wave = "P"),
        channel = 2,
        position = 0
      ),
      landmark("QRS", list(type = "N"), channel = 2, position = 300),
      landmark(
        "offset",
        list(type = ")", wave = "T"),
        channel = 2,
        position = 599
      )
    ),
    target_samples = 600
  )
  warped <- warp_window(windows, manual)
  expect_true(all(vapply(
    warped,
    function(x) nrow(x$signal) == 600L,
    logical(1)
  )))
})

test_that("warp_window warns about absent landmarks", {
  windows <- template_sinus_windows()
  manual <- template(
    landmarks = list(
      landmark("onset", list(type = "(", wave = "P"), 2, 0),
      landmark("QRS", list(type = "N"), 2, 250),
      landmark("ghost", list(type = "ZZZ"), 2, 400),
      landmark("offset", list(type = ")", wave = "T"), 2, 499)
    ),
    target_samples = 500
  )
  expect_warning(
    warped <- warp_window(windows, manual),
    "not found in any window"
  )
  expect_length(warped, length(windows))
})

test_that("warp_window requires a template object", {
  expect_error(
    warp_window(template_sinus_windows(), list(a = 1)),
    "template object"
  )
})

test_that("learning uses the signal sample coordinate", {
  shifted <- template_sinus_windows()[[1]]
  shifted$signal <- data.table::copy(shifted$signal)
  shifted$signal$sample <- shifted$signal$sample + 1000L
  shifted$annotation <- lapply(shifted$annotation, function(x) {
    x <- data.table::copy(x)
    x$sample <- x$sample + 1000L
    x
  })
  learned <- learn_template(shifted, channel = 2, target_samples = 500)
  positions <- vapply(
    learned@landmarks,
    function(x) x@position,
    numeric(1)
  )
  expect_equal(positions[c(1, 3)], c(0, 499))
})

test_that("channel-specific annotations take precedence over global fallback", {
  annotations <- annotation_table(
    sample = c(5L, 10L),
    type = c("N", "N"),
    channel = c(0L, 2L)
  )
  expect_equal(EGM:::locate_feature(annotations, "N", 2L), 10L)
  expect_equal(
    EGM:::locate_feature(annotations[annotations$channel == 0L, ], "N", 2L),
    5L
  )
})

test_that("landmarks can use stable channel names", {
  learned <- learn_template(
    template_sinus_windows(),
    landmarks = list(QRS_II = list(type = "N", channel = "II")),
    target_samples = 300
  )
  expect_equal(learned@landmarks[[1]]@channel, "II")
  warped <- warp_window(template_sinus_windows()[1], learned)
  mappings <- attr(warped, "warp_mappings")
  expect_length(mappings, 1L)
  expect_true(mappings[[1]]$found[[1]])
})

test_that("feature criteria support ranges and preserve ambiguity", {
  annotations <- annotation_table(
    sample = 1:3,
    type = rep("x", 3),
    channel = rep(1L, 3)
  )
  annotations$voltage <- c(-2, 0, 2)
  expect_equal(
    EGM:::locate_features(
      annotations,
      list(type = "x", voltage = feature_range(-1, 1)),
      1L
    ),
    2L
  )
  expect_error(
    EGM:::locate_feature(annotations, "x", 1L, multiple = "error"),
    "more than one"
  )
})

test_that("template validates the target grid and positions", {
  expect_error(
    template(
      landmarks = list(landmark("x", list(type = "N"), position = 0)),
      target_samples = 1
    ),
    "at least 2"
  )
  expect_error(
    template(
      landmarks = list(landmark("x", list(type = "N"), position = 500)),
      target_samples = 500
    ),
    "within the target grid"
  )
})

test_that("crossed warp anchors fail explicitly", {
  windows <- template_sinus_windows()
  manual <- template(
    landmarks = list(
      landmark("N_first", list(type = "N"), 2, 100),
      landmark("P_later", list(type = "p"), 2, 200)
    ),
    target_samples = 400
  )
  expect_error(
    warp_window(windows[1], manual, order_policy = "error"),
    "crossed or duplicate"
  )
  dropped <- warp_window(
    windows[1],
    manual,
    order_policy = "drop",
    preserve_class = FALSE
  )
  expect_length(dropped, 0L)
})

test_that("a channel is named the same way everywhere it is accepted", {
  windows <- template_sinus_windows()[1:3]

  # A number, the criteria-list shape the neighbouring arguments take, and a
  # stable channel name all name the same lead
  by_number <- learn_template(windows, channel = 2, target_samples = 300)
  by_list <- learn_template(
    windows,
    channel = list(channel = 2),
    target_samples = 300
  )
  by_name <- learn_template(windows, channel = "II", target_samples = 300)
  expect_equal(by_list@statistics, by_number@statistics)
  expect_equal(by_name@statistics, by_number@statistics)
})

test_that("ambiguity from a per-lead annotator names the argument that fixes it", {
  windows <- template_sinus_windows()[1:2]

  # The count rising with the number of leads is the clue, so the message says
  # so rather than reporting the bare count as a symptom
  expect_error(
    learn_template(windows),
    "annotations across [0-9]+ channels.*set `channel`"
  )
})
