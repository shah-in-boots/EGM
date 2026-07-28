# Shared fixture: the twelve LUDB per-lead delineations written as one
# signal-numbered file, which is the convention `channel_zero = "signal"` names.
# LUDB itself ships one file per lead with `chan` left at 0 throughout, so the
# two readings can be checked against the same fiducials.
ludb_leads <- function() {
  c("i", "ii", "iii", "avr", "avl", "avf", "v1", "v2", "v3", "v4", "v5", "v6")
}

signal_numbered_record <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  file.copy(test_path(c("ludb-ecg.dat", "ludb-ecg.hea")), dir)

  parts <- lapply(seq_along(ludb_leads()), function(k) {
    ann <- data.table::as.data.table(
      read_annotation("ludb-ecg", ludb_leads()[k], test_path())
    )
    ann$channel <- as.integer(k - 1L)
    ann
  })
  combined <- data.table::rbindlist(parts)
  data.table::setorderv(combined, c("sample", "channel"))

  write_annotation(
    annotation_table(
      annotator = "sig",
      time = combined$time,
      sample = combined$sample,
      type = combined$type,
      subtype = combined$subtype,
      channel = combined$channel,
      number = combined$number,
      aux = combined$aux
    ),
    "sig",
    "ludb-ecg",
    dir
  )
  dir
}

test_that("channel zero is read as global by default", {
  # Every annotator that does not populate the field leaves it at 0, so an
  # all-zero column is an absence of information rather than a claim that every
  # fiducial belongs to the first signal
  ecg <- read_wfdb("ludb-ecg", test_path(), "ii")

  expect_equal(channel_zero(ecg), "global")
  expect_equal(channel_zero(get_annotation(ecg)), "global")
  expect_length(EGM:::annotation_channels(get_annotation(ecg)), 0)

  # Which is what lets such a record be windowed without naming a channel
  expect_length(get_windows(ecg, by = by_beat()), 6)
})

test_that("a signal-numbered file declares itself and keeps its first lead", {
  dir <- signal_numbered_record()
  ecg <- read_wfdb("ludb-ecg", dir, "sig", channel_zero = "signal")

  expect_equal(channel_zero(ecg), "signal")
  expect_equal(EGM:::annotation_channels(get_annotation(ecg)), 0:11)

  # Channel 0 is a lead here, and it is the lead LUDB delineated in its own file
  lead_i <- read_annotation("ludb-ecg", "i", test_path())
  expect_equal(
    EGM:::locate_features(get_annotation(ecg), "N", 0L),
    as.integer(lead_i$sample[lead_i$type == "N"])
  )
  expect_length(get_windows(ecg, by = by_beat(channel = 0)), 6)

  # Twelve channels are still ambiguous without one being named
  expect_error(get_windows(ecg, by = by_beat()), "needs a guiding `channel`")
})

test_that("a lead name resolves against the file's own convention", {
  dir <- signal_numbered_record()
  by_signal <- read_wfdb("ludb-ecg", dir, "sig", channel_zero = "signal")
  by_global <- read_wfdb("ludb-ecg", test_path(), "ii")

  # `header$number` counts signals from one, so the annotation channel is one
  # lower wherever the file numbers its channels by signal
  expect_equal(EGM:::resolve_channel_spec(by_signal, "i"), 0L)
  expect_equal(EGM:::resolve_channel_spec(by_signal, "ii"), 1L)
  expect_equal(EGM:::resolve_channel_spec(by_global, "ii"), 2L)
})

test_that("a channel column that fills the signals is reported when read as global", {
  dir <- signal_numbered_record()

  # Read as global the file loses its first lead silently, so it is said once,
  # where the file is opened and both the channels and the signal count are known
  expect_warning(
    read_wfdb("ludb-ecg", dir, "sig"),
    "one channel per signal"
  )
  expect_warning(read_wfdb("ludb-ecg", dir, "sig"), 'channel_zero = "signal"')

  # Declaring it is what silences it
  expect_no_warning(read_wfdb("ludb-ecg", dir, "sig", channel_zero = "signal"))

  # And an ordinary per-lead file that leaves room for a global channel is not
  # reported, since nothing about it is ambiguous
  expect_no_warning(read_wfdb("ecg-sinus", test_path(), "ann"))
})

test_that("the global channel is a fallback only where there is one", {
  annotations <- annotation_table(
    sample = 1:2,
    type = c("N", "N"),
    channel = c(0L, 2L)
  )

  # Under the global reading, a channel with no match falls back to channel 0
  expect_equal(EGM:::locate_features(annotations, "N", 3L), 1L)

  # Under the signal reading channel 0 is a lead, so there is nothing to fall
  # back to and an absent channel matches nothing
  by_signal <- annotation_table(
    sample = 1:2,
    type = c("N", "N"),
    channel = c(0L, 2L),
    channel_zero = "signal"
  )
  expect_length(EGM:::locate_features(by_signal, "N", 3L), 0)
  expect_equal(EGM:::locate_features(by_signal, "N", 0L), 1L)
})

test_that("the declared convention rides on the table", {
  ann <- annotation_table(
    sample = 1:2,
    type = c("N", "N"),
    channel = c(0L, 1L),
    channel_zero = "signal"
  )

  expect_equal(channel_zero(ann), "signal")
  expect_match(paste(capture.output(print(ann)), collapse = " "), "first signal")

  # Anything built without the label follows the convention the files use
  expect_equal(channel_zero(annotation_table(sample = 1L, type = "N")), "global")
  expect_equal(channel_zero(data.frame(sample = 1L)), "global")
})
