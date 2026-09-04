# Shared fixture: the twelve LUDB per-lead delineations written as one file that
# counts its signals from 0, which is the convention `channel_zero = "signal"`
# names. LUDB itself ships one file per lead with `chan` left at 0 throughout,
# so the two readings can be checked against the same fiducials.
#
# The record is read where the package ships it rather than from a copy kept
# here, so a header that stops naming the files beside it fails these tests too.
ludb_dir <- function() {
  system.file("extdata", package = "EGM")
}

ludb_leads <- function() {
  c("i", "ii", "iii", "avr", "avl", "avf", "v1", "v2", "v3", "v4", "v5", "v6")
}

signal_numbered_record <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  # The installed copy may be read-only; the temp copy is written beside a new
  # annotation file, so take the contents without the permissions
  file.copy(
    fs::path(ludb_dir(), c("ludb-ecg.dat", "ludb-ecg.hea")),
    dir,
    copy.mode = FALSE
  )

  parts <- lapply(seq_along(ludb_leads()), function(k) {
    ann <- data.table::as.data.table(
      read_annotation("ludb-ecg", ludb_leads()[k], ludb_dir())
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
  ecg <- read_wfdb("ludb-ecg", ludb_dir(), "ii")

  expect_equal(channel_zero(ecg), "global")
  expect_equal(channel_zero(get_annotation(ecg)), "global")
  expect_length(EGM:::annotation_channels(get_annotation(ecg)), 0)

  # Which is what lets such a record be windowed without naming a channel
  expect_length(get_windows(ecg, by = by_beat()), 6)
})

test_that("a file counting from 0 is renumbered from 1 as it is read", {
  dir <- signal_numbered_record()
  ecg <- read_wfdb("ludb-ecg", dir, "sig", channel_zero = "signal")

  # In memory every table counts signals from 1, so lead I is channel 1 here as
  # it is in a file that counted from 1; only the label remembers the difference
  expect_equal(channel_zero(ecg), "signal")
  expect_equal(EGM:::annotation_channels(get_annotation(ecg)), 1:12)

  lead_i <- read_annotation("ludb-ecg", "i", ludb_dir())
  expect_equal(
    EGM:::locate_features(get_annotation(ecg), "N", 1L),
    as.integer(lead_i$sample[lead_i$type == "N"])
  )
  expect_length(get_windows(ecg, by = by_beat(channel = 1)), 6)
  expect_length(get_windows(ecg, by = by_beat(channel = "i")), 6)

  # Twelve channels are still ambiguous without one being named
  expect_error(get_windows(ecg, by = by_beat()), "needs a guiding `channel`")
})

test_that("a lead name resolves to its signal number", {
  dir <- signal_numbered_record()
  by_signal <- read_wfdb("ludb-ecg", dir, "sig", channel_zero = "signal")
  by_global <- read_wfdb("ludb-ecg", ludb_dir(), "ii")

  # `header$number` counts signals from one, and so does every table, so a name
  # resolves the same way whichever convention its file used
  expect_equal(EGM:::resolve_channel_spec(by_signal, "i"), 1L)
  expect_equal(EGM:::resolve_channel_spec(by_signal, "ii"), 2L)
  expect_equal(EGM:::resolve_channel_spec(by_global, "ii"), 2L)
})

test_that("a channel column that fills the signals is refused until declared", {
  dir <- signal_numbered_record()

  # `0 .. nsig-1` fills the signals if 0 is the first of them and is a global
  # channel plus every lead but the last otherwise; nothing in the file says
  # which, and each reading misplaces a lead under the other, so it is refused
  # where the file is opened rather than read one way and warned about
  expect_error(read_wfdb("ludb-ecg", dir, "sig"), "runs 0 to 11")
  expect_error(read_wfdb("ludb-ecg", dir, "sig"), 'channel_zero = "signal"')
  expect_error(read_wfdb("ludb-ecg", dir, "sig"), 'channel_zero = "global"')

  # Either declaration settles it
  expect_no_error(read_wfdb("ludb-ecg", dir, "sig", channel_zero = "signal"))
  verbatim <- read_wfdb("ludb-ecg", dir, "sig", channel_zero = "global")
  expect_equal(EGM:::annotation_channels(get_annotation(verbatim)), 1:11)

  # And a file that counts from 1 is not ambiguous, so it reads without one
  expect_no_error(read_wfdb("ecg-sinus", test_path(), "ann"))
  expect_no_error(read_wfdb("ludb-ecg", ludb_dir(), "ii"))
})

test_that("a file goes back out the way it came in", {
  dir <- signal_numbered_record()
  ann <- read_annotation("ludb-ecg", "sig", dir, channel_zero = "signal")
  expect_equal(sort(unique(ann$channel)), 1:12)

  out <- withr::local_tempdir()
  file.copy(fs::path(dir, c("ludb-ecg.dat", "ludb-ecg.hea")), out)
  write_annotation(ann, "sig", "ludb-ecg", out)

  # On disk the numbering is the one the file arrived with, so a tool that reads
  # it counting from 0 still finds lead I at 0
  on_disk <- read_annotation("ludb-ecg", "sig", out, channel_zero = "global")
  expect_equal(sort(unique(on_disk$channel)), 0:11)

  # And read as it was written it comes back identical, label included
  expect_equal(read_annotation("ludb-ecg", "sig", out, channel_zero = "signal"), ann)

  # Told to count from 1 instead, it reads back with no declaration at all
  write_annotation(ann, "one", "ludb-ecg", out, channel_zero = "global")
  from_one <- read_annotation("ludb-ecg", "one", out)
  expect_equal(sort(unique(from_one$channel)), 1:12)
  expect_equal(channel_zero(from_one), "global")
})

test_that("a global annotation cannot be written counting from 0", {
  # The value it would take on disk is the first signal's, so the writer refuses
  # rather than merge it into lead I
  ann <- annotation_table(
    annotator = "x",
    sample = 1:3,
    type = c("N", "N", "+"),
    channel = c(0L, 1L, 0L)
  )
  dir <- withr::local_tempdir()

  expect_error(
    write_annotation(ann, "x", "rec", dir, channel_zero = "signal"),
    "global channel 0"
  )
  expect_no_error(write_annotation(ann, "x", "rec", dir, channel_zero = "global"))
})

test_that("the global channel is a fallback where the requested one is silent", {
  annotations <- annotation_table(
    sample = 1:2,
    type = c("N", "N"),
    channel = c(0L, 2L)
  )

  # A channel with no match falls back to channel 0, in every table
  expect_equal(EGM:::locate_features(annotations, "N", 3L), 1L)
  expect_equal(EGM:::locate_features(annotations, "N", 2L), 2L)
})

test_that("the declared convention rides on the table", {
  # Given channels counted from 0, the constructor renumbers them from 1 and
  # keeps the label so the writer can count them back down
  ann <- annotation_table(
    sample = 1:2,
    type = c("N", "N"),
    channel = c(0L, 1L),
    channel_zero = "signal"
  )

  expect_equal(ann$channel, c(1L, 2L))
  expect_equal(channel_zero(ann), "signal")
  expect_match(paste(capture.output(print(ann)), collapse = " "), "from 0")

  # Anything built without the label is held, and written, counting from 1
  expect_equal(channel_zero(annotation_table(sample = 1L, type = "N")), "global")
  expect_equal(channel_zero(data.frame(sample = 1L)), "global")

  # Renumbering needs numbers
  expect_error(
    annotation_table(sample = 1L, type = "N", channel = "II", channel_zero = "signal"),
    "must be numeric"
  )
})
