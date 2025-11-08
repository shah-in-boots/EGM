# Plotting ----

test_that("plots can be generated easily", {
  # EPS data
  data <- read_bard(test_path('bard-egm.txt'))
  expect_s3_class(data, "egm")

  channels <- c("I", "CS", "HIS D", "HIS M", "RV")
  time_frame <- c(.1, 3)

  object <- ggm(
    data = data,
    channels = channels,
    time_frame = time_frame
  )

  expect_s3_class(object, "ggm")
  expect_s3_class(object, "ggplot")

  # ECG data
  data <- read_muse(test_path('ecg.xml'))
  g <- ggm(data, channels = c("I", "II", "III"))
  expect_s3_class(g, "ggm")
  expect_s3_class(g, "ggplot")
})

test_that('header and labels work fluidly when plotting', {
  skip_on_ci()

  data <- read_wfdb(
    record = 'ludb-ecg',
    record_dir = test_path(),
    annotator = 'i'
  )

  object <- ggm(data, channels = data$header$label)

  expect_s3_class(object, 'ggm')
  expect_equal(
    attributes(object@theme$axis.ticks)$class[1],
    "ggplot2::element_blank"
  )
})

test_that("multiple channel data from different leads can be theme", {
  fp <- system.file('extdata', 'bard-avnrt.txt', package = 'EGM')
  dat <- read_bard(fp)

  # Similarly, can be visualized with ease
  g <-
    ggm(dat, channels = c('HIS', 'CS', 'RV')) +
    theme_egm_dark()

  expect_true(inherits(g, 'ggm') & inherits(g, 'ggplot'))
})

test_that("ggm defaults to dark background when white channels present", {
  fp <- system.file('extdata', 'bard-avnrt.txt', package = 'EGM')
  dat <- read_bard(fp)

  g <- ggm(dat)

  expect_identical(g$theme$panel.background$fill, "black")
  expect_identical(g$theme$strip.text.y.left$colour, "white")
  expect_identical(g$theme$axis.text.x$colour, "white")
  expect_identical(g$theme$axis.ticks.x$colour, "white")
})

test_that("ggm defaults to light background when black channels present", {
  data <- read_wfdb(
    record = 'ludb-ecg',
    record_dir = test_path(),
    annotator = 'i'
  )

  g <- ggm(data, channels = data$header$label)

  expect_identical(g$theme$panel.background$fill, "white")
  expect_identical(g$theme$strip.text.y.left$colour, "black")
  expect_identical(g$theme$axis.text.x$colour, "black")
  expect_identical(g$theme$axis.ticks.x$colour, "black")
})
