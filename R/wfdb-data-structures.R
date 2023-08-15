# Signal Table -----------------------------------------------------------------

#' Signal Table
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' `signal_table()` modifies the `data.table` class to work with electrical
#' signal data. The input should be a data set of equal number of rows.
#' @export
signal_table <- function(...) {

	# Invariant rules:
	# 	Can add and remove rows (each row is a time point)
	# 	Rows can NOT be re-ordered
	# 	Columns CAN be re-ordered
	# 	Signal columns must be numeric (integer or double)
	#
	# Invariant columns:
	# 	sample Position: represents a time point and order of data (<integer>)

	x <- df_list(..., .name_repair = 'minimal')

	if (length(x) == 0) {
		return(new_signal_table())
	}

	new_signal_table(x = x)
}

#' @export
new_signal_table <- function(x = list()) {
	checkmate::assert_list(x, types = 'numeric')
	new_data_frame(x, class = c('signal_table', 'data.table'))

}

#' @export
print.signal_table <- function(x, ...) {
	cat(sprintf('<%s: %s x %s>\n', class(x)[[1]], dim(x)[1], dim(x)[2]))
	if (length(x) > 0) {
		NextMethod()
	}
}

#' @export
vec_ptype_abbr.signal_table <- function(x, ...) "sig_tbl"

#' @export
vec_ptype_full.signal_table <- function(x, ...) "signal_table"

#' @export
#' @rdname signal_table
is_signal_table <- function(x) {
	inherits(x, "signal_table")
}

#' @importFrom vctrs vec_ptype2 vec_cast
NULL

#' @export
signal_table_ptype2 <- function(x, y, ...) {
	as.data.table(df_ptype2(x, y, ...))
}

#' @export
signal_table_cast <- function(x, to, ...) {
	as.data.table(df_cast(x, to, ...))
}

## signal_table ----

#' @export
vec_ptype2.signal_table.signal_table <- function(x, y, ...) {
	new_signal_table()
}

#' @export
vec_cast.signal_table.signal_table <- function(x, to, ...) {
	x
}

## data.table ----

#' @export
vec_ptype2.signal_table.data.table <- function(x, y, ...) {
	signal_table_ptype2(x, y, ...)
}

#' @export
vec_cast.signal_table.data.table <- function(x, to, ...) {
	signal_table_cast(x, to, ...)
}

## data.frame ----

#' @export
vec_ptype2.signal_table.data.frame <- function(x, y, ...) {
	signal_table_ptype2(x, y, ...)
}

#' @export
vec_cast.signal_table.data.frame <- function(x, to, ...) {
	signal_table_cast(x, to, ...)
}

# Annotation Table -------------------------------------------------------------

#' Annotation Table
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' `annotation_table()` modifies the `data.table` class to work with annotation
#' data.
#'
#' @export
annotation_table <- function(TIME = numeric(),
														 SAMPLE = integer(),
														 TYPE = character(),
														 SUBTYPE = character(),
														 CHANNEL = integer(),
														 NUMBER = integer()) {


	# Invariant rules:
	# 	Can add and remove rows (each row is an annotation)
	# 	Rows CAN be re-ordered
	# 	Columns CANNOT be re-ordered
	# 	Each column type is specific and invariant
	#
	# Invariant columns:
	#		time: <double>
	# 	sample: <integer>
	#		type: <character>
	# 	subtype: <character>
	#		channel: <integer>
	# 	number: <integer>

	x <- df_list(TIME = TIME,
							 SAMPLE = SAMPLE,
							 TYPE = TYPE,
							 SUBTYPE = SUBTYPE,
							 CHANNEL = CHANNEL,
							 NUMBER = NUMBER)

	new_annotation_table(x = x)
}

#' @export
new_annotation_table <- function(x = list()) {

	checkmate::assert_list(
		x,
		types = c('numeric', 'integer', 'character')
	)

	checkmate::assert_names(
		names(x),
		identical.to = c('TIME', 'SAMPLE', 'TYPE', 'SUBTYPE', 'CHANNEL', 'NUMBER')
	)

	new_data_frame(x, class = c('annotation_table', 'data.table'))

}

#' @export
print.annotation_table <- function(x, ...) {
	cat(sprintf('<%s: %s annotations>\n', class(x)[[1]], dim(x)[1]))
	if (lengths(x)[1] > 0) {
		NextMethod()
	}
}

#' @export
vec_ptype_abbr.annotation_table <- function(x, ...) "ann_tbl"

#' @export
vec_ptype_full.annotation_table <- function(x, ...) "annotation_table"

#' @export
#' @rdname annotation_table
is_annotation_table <- function(x) {
	inherits(x, "annotation_table")
}

#' @export
annotation_table_ptype2 <- function(x, y, ...) {
	as.data.table(df_ptype2(x, y, ...))
}

#' @export
annotation_table_cast <- function(x, to, ...) {
	as.data.table(df_cast(x, to, ...))
}

#' @export
vec_ptype2.annotation_table.annotation_table <- function(x, y, ...) {
	new_annotation_table()
}

#' @export
vec_cast.annotation_table.annotation_table <- function(x, to, ...) {
	x
}

#' @export
vec_ptype2.annotation_table.data.table <- function(x, y, ...) {
	annotation_table_ptype2(x, y, ...)
}

#' @export
vec_cast.annotation_table.data.table <- function(x, to, ...) {
	annotation_table_cast(x, to, ...)
}

#' @export
vec_ptype2.annotation_table.data.frame <- function(x, y, ...) {
	annotation_table_ptype2(x, y, ...)
}

#' @export
vec_cast.annotation_table.data.frame <- function(x, to, ...) {
	annotation_table_cast(x, to, ...)
}

# Header Table -------------------------------------------------------------

#' Header Table
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' `header_table()` modifies the `data.table` class to work with header data.
#' The header data is read in from a similar format as to that of WFDB files and
#' should be compatible/interchangeable when writing out to disk.
#'
#' @details
#'
#' # Header file structure
#'
#' There are three components to the header file
#'
#' 1. __Record line__ that contains the following information, in the order
#' documented, however pieces may be missing based on different parameters. From
#' left to right...
#'
#'		- Record name
#'		- Number of signals: represents number of segments/channels
#'		- Sampling frequency [optional]
#'		- Number of samples [optional]
#'		- Time: in HH:MM:SS format [optional]
#'		- Date: in DD/MM/YYYY [optional]
#'
#' 1. __Signal specification lines__ contains specifications for individual
#' signals, and there must be as many signal lines as there are reported by the
#' above record line. From left to right....
#'
#'		- File name: (usually *.dat)
#'		- Format <integer>: represents storage type, e.g. 8-bit or 16-bit
#'		- ADC gain: (ADC units per physical unit) [optional]
#'		- Baseline ADC: that corresponds to 0 physical units [optional]
#'		- Units: follows ADC gain with '/' as a field separator e.g '300/mV'
#'		- ADC resolution <integer>: bits, usually 8 or 16 [optional]
#'		- ADC zero: represents middle of ADC input range [optional]
#'		- Initial value [optional]
#'		- Checksum [optional]
#'		- Block size [optional]
#'		- Description: text or label information [optional]
#'
#' 1. __Info strings__ are unstructured lines that contains information about
#' the record. Usually are descriptive. Starts with initial '#' without
#' preceding white space at beginning of line.
#'
#' @param INFO List of characters that will be applied when writing out file
#' @export
header_table <- function(FILE_NAME = character(),
												NUMBER_OF_CHANNELS = integer(),
												SAMPLES = integer(),
												START_TIME = Sys.time(),
												END_TIME = Sys.time(),
												FREQUENCY = integer(),
												ADC_SATURATION = integer(),
												LABEL = character(),
												GAIN = integer(),
												ADC_GAIN = numeric(),
												LOW_PASS = integer(),
												HIGH_PASS = integer(),
												COLOR = character(),
												SCALE = integer(),
												INFO_STRINGS = list()) {

	# Three components to the header structure as described above
	# 	Record line
	# 	Signal line(s)
	# 	Info strings

	# First line of (*.hea) equivalent
	RECORD_LINE <- list(
		FILE_NAME = FILE_NAME,
		NUMBER_OF_CHANNELS = NUMBER_OF_CHANNELS,
		SAMPLES = SAMPLES,
		START_TIME = START_TIME,
		END_TIME = END_TIME,
		FREQUENCY = FREQUENCY,
		ADC_SATURATION = ADC_SATURATION
	)

	# Process labels
	LABEL <- toupper(LABEL)
	lab_splits <-
		stringr::str_split(LABEL,
											 pattern = "(?<=[a-zA-Z])\\s*(?=[0-9])",
											 n = 2,
											 simplify = TRUE) |>
		gsub("^$", NA, x = _)
	SOURCE <- lab_splits[,1]
	LEAD <- lab_splits[,2]
	LEAD <- ifelse(LABEL %in% .leads$ECG, LABEL, LEAD)
	SOURCE <- ifelse(LABEL %in% .leads$ECG, "ECG", SOURCE)
	src_splits <-
		stringr::str_split(SOURCE,
											 pattern = "\ ",
											 n = 2,
											 simplify = TRUE) |>
		gsub("^$", NA, x = _)
	SOURCE <- src_splits[,1]
	LEAD <- ifelse(is.na(LEAD), src_splits[,2], LEAD)
	LABEL <- sub("ECG\ ", "", paste(SOURCE, LEAD))
	SOURCE <- factor(SOURCE, levels = intersect(.source, SOURCE))
	LABEL <- factor(LABEL, levels = intersect(.labels, LABEL))

	# Signal specifications
	x <- df_list(
		NUMBER = 1:NUMBER_OF_CHANNELS,
		LABEL = LABEL,
		LEAD = LEAD,
		SOURCE = SOURCE,
		GAIN = GAIN,
		ADC_GAIN = ifelse(length(ADC_GAIN) == 0, ADC_SATURATION / GAIN, ADC_GAIN),
		LOW_PASS = ifelse(length(LOW_PASS) == 0, NA, LOW_PASS),
		HIGH_PASS = ifelse(length(HIGH_PASS) == 0, NA, HIGH_PASS),
		COLOR = ifelse(length(COLOR) == 0, '#FFFFFF', COLOR),
		SCALE = ifelse(length(SCALE) == 0, NA, SCALE)
	)

	# Info strings

	# Construct new table
	new_header_table(
		x = x,
		RECORD_LINE = RECORD_LINE,
		INFO_STRINGS = INFO_STRINGS
	)

}

#' @export
new_header_table <- function(x = list(),
														 RECORD_LINE = list(),
														 INFO_STRINGS = list()) {

	structure(
		new_data_frame(x),
		RECORD_LINE = RECORD_LINE,
		INFO_STRINGS = INFO_STRINGS,
		class = c('header_table', 'data.table', class(x))
	)

}

#' @export
#' @rdname header_table
is_header_table <- function(x) {
	inherits(x, "header_table")
}

#' @export
print.header_table <- function(x, ...) {
	cat(
		sprintf(
			'<%s: %s channels, %s samples @ %s Hz>\n',
			class(x)[[1]],
			attributes(x)$RECORD_LINE$NUMBER_OF_CHANNELS,
			attributes(x)$RECORD_LINE$SAMPLES,
			attributes(x)$RECORD_LINE$FREQUENCY
		)
	)
	if (lengths(x)[1] > 0) {
		NextMethod()
	}
}
