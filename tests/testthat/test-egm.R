test_that("EGM class can be made", {
  df <- read_bard(file = test_path("bard-egm.txt"))

  file <- test_path("bard-egm.txt")
  sig <- read_bard_signal(file)
  hea <- read_bard_header(file)

  x <- new_EGM(signal = sig, header = hea)
  expect_s3_class(x, "EGM")
  expect_s3_class(x$signal, 'signal_table')
  expect_s3_class(x$signal, "data.table")
  expect_s3_class(x$signal, "data.frame")
  expect_s3_class(x$header, 'header_table')
})

test_that("EGM/signal class definition works", {
  # Class definition
  x <- new_EGM()
  expect_length(x, 3)
  expect_true(is_EGM(x))
  expect_equal(new_EGM(), EGM())

  # Random signal with peaks and troughs, cosine pattern
  x <- cos(2 * pi * (1:1000) * (1:100) / 1e+5)

  # Components of header
  label <- "V1"
  label <- .labels[.labels == "V1"]
  for (i in names(.leads)) {
    if (label %in% .leads[[i]]) {
      source <- i
    }
  }
  color <- "#0000000"
  voltage <- "mV"
  frequency <- as.integer(1000)

  sig <- signal_table(V1 = x)
  hea <- header_table(
    label = label,
    color = color,
    ADC_units = voltage,
    frequency = frequency
  )

  s1 <- new_EGM(signal = sig, header = hea)
  expect_s3_class(s1, "EGM")
  s2 <- EGM(sig, hea)
  expect_equal(s1, s2)

  # Basic output data
  expect_output(print(s1), "[Electrical Signal]")
})


test_that('signal can be removed from EGM object', {
  skip_on_ci()

  object <- read_wfdb('ecg', test_path())
  expect_s3_class(object, 'EGM')

  # Default = data.frame
  raw <- get_signal(object)
  expect_s3_class(raw, 'data.frame')
  expect_length(raw, 13)

  # Matrix
  raw <- get_signal(object, data_format = 'matrix')
  expect_type(raw, 'double')
  expect_equal(class(raw)[1], 'matrix')
  expect_equal(dim(raw)[1], 5000)
  expect_equal(dim(raw)[2], 12)

  # Array
  raw_array <- get_signal(object, data_format = 'array')
  expect_equal(dim(raw_array), c(5000, 12))
  expect_identical(dimnames(raw_array)[[2]], names(object$signal)[-1])
})

test_that("print dispatches to this package's methods", {
  # `S7::method(print, cls) <- f` is a replacement call: written at the top level
  # of a package it leaves a copy of `print` in the namespace, and every
  # `S3method(print, ...)` directive then registers against that copy instead of
  # `base::print`, which silently kills S3 print dispatch for every class here.
  # The S7 methods are wrapped in `local()` to prevent it; this is the guard.
  expect_false(exists("print", envir = asNamespace("EGM"), inherits = FALSE))

  table <- get(".__S3MethodsTable__.", envir = asNamespace("base"))
  for (method in c("print.EGM", "print.ECG", "print.windows",
                   "print.signal_table", "print.header_table",
                   "print.annotation_table")) {
    expect_true(exists(method, envir = table), info = method)
  }

  # And the dispatch itself, rather than only its registration
  windows <- new_windows(list(), method = "beat", source_record = "x")
  expect_match(paste(capture.output(print(windows)), collapse = " "), "<windows:")
  expect_match(
    paste(capture.output(print(by_rhythm())), collapse = " "),
    "<window_strategy: rhythm>"
  )
})
