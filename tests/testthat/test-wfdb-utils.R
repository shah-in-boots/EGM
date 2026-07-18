test_that("paths are available", {
  on.exit(options(wfdb_path = NULL), add = TRUE)
  options(wfdb_path = tempdir())

  path <- find_wfdb_software()

  expect_identical(path, getOption("wfdb_path"))
})

test_that("WFDB time parameters normalize clocks and compact durations", {
  start <- as.POSIXct("2026-01-01 12:00:00", tz = "UTC")

  value <- validate_time_parameters(
    begin = "00:00:00.100",
    interval = "100 ms",
    start_time = start,
    study_duration = 1
  )
  expect_equal(value$begin, 0.1)
  expect_equal(value$end, 0.2)
  expect_equal(value$interval, 0.1)

  expect_equal(
    validate_time_parameters(
      begin = as.difftime(0.25, units = "secs"),
      interval = 0.5,
      start_time = start,
      study_duration = 1
    )$end,
    0.75
  )

  expect_equal(
    validate_time_parameters(
      begin = data.table::as.ITime("00:00:00"),
      end = start + 0.5,
      start_time = start,
      study_duration = 1
    )$end,
    0.5
  )
})

test_that("WFDB time validation requires dates for multi-day studies", {
  start <- as.POSIXct("2026-01-01 12:00:00", tz = "UTC")

  expect_error(
    validate_time_parameters(
      begin = "00:00:01",
      start_time = start,
      study_duration = 2 * 86400
    ),
    "must include a date"
  )
  expect_error(
    validate_time_parameters(
      begin = as.difftime(1, units = "days"),
      start_time = start,
      study_duration = 2 * 86400
    ),
    "must include a date"
  )

  value <- validate_time_parameters(
    begin = "2026-01-02 12:00:00",
    interval = "2 hours",
    start_time = start,
    study_duration = 2 * 86400
  )
  expect_equal(value$begin, 86400)
  expect_equal(value$end, 93600)

  bracketed <- validate_time_parameters(
    begin = "[12:00:00 02/01/2026]",
    interval = "100 ms",
    start_time = start,
    study_duration = 2 * 86400
  )
  expect_equal(bracketed$begin, 86400)
  expect_equal(bracketed$end, 86400.1)
})

test_that("WFDB time validation clamps intervals to the study end", {
  start <- as.POSIXct("2026-01-01 12:00:00", tz = "UTC")

  value <- validate_time_parameters(
    begin = "00:00:09.500",
    interval = "30s",
    start_time = start,
    study_duration = 10
  )
  expect_equal(value$begin, 9.5)
  expect_equal(value$end, 10)

  expect_error(
    validate_time_parameters(
      begin = 1,
      start_time = start,
      study_duration = 10
    ),
    "character timestamp"
  )
})
