# WFDB Signal Handling Review
## Analysis and Proposed Refactoring

**Date**: 2025-10-24
**Branch**: `wfdb_merge`
**Objective**: Align signal handling with WFDB C library principles for maintainability and correctness

---

## Executive Summary

After reviewing the current implementation and the WFDB C library approach, I recommend refactoring the signal handling to **primarily store digital (integer) values** with **on-demand conversion to physical units**. This aligns with the WFDB C library philosophy and provides:

1. **Memory efficiency**: Integer storage is more compact than doubles
2. **Precision preservation**: No accumulated floating-point errors in round-trips
3. **Type safety**: Clear distinction between digital and physical representations
4. **Simplicity**: Easier to reason about and debug
5. **WFDB compatibility**: Direct alignment with established standards

---

## Current Implementation Analysis

### 1. Storage on Disk (C++ Layer)
**Location**: `src/wfdb.cpp`

**Current Behavior**: ✅ **Correct**
- Signals stored as integers in various formats (8, 16, 24, 32, 212-bit)
- Format correctly follows WFDB specification
- Binary read/write operations are properly implemented

**Key Code**:
```cpp
// Reading (lines 745-783)
double raw_value = static_cast<double>(read_int16_little(stream));
double value = raw_value;
if (baseline_value != NA_INTEGER) {
    value -= static_cast<double>(baseline_value);
}
if (physical) {
    if (gain_value != NA_REAL && gain_value != 0.0) {
        value /= gain_value;
    }
}
```

**Issue**: Returns doubles for both digital and physical modes, losing type distinction.

### 2. Memory Representation (R Layer)
**Location**: `R/wfdb-structures.R`

**Current Behavior**: ⚠️ **Needs Improvement**
- `signal_table` stores all signal columns as generic `numeric` (double in R)
- No distinction between digital vs. physical units
- No enforcement of integer storage for digital values
- Validation only checks for numeric types (line 45):
  ```r
  checkmate::assert_list(y, types = 'numeric')
  ```

**Issue**: Users can accidentally store physical values, or mix units without type safety.

### 3. Read/Write Operations
**Location**: `R/wfdb-io.R`

**Current Behavior**: ⚠️ **Inconsistent**

**Reading** (lines 403-513):
- Accepts `units = c("digital", "physical")` parameter
- Passes `physical` boolean to C++ layer
- Returns `signal_table` with doubles regardless of units
- No metadata tracking which units are in use

**Writing** (lines 154-346):
- Accepts both integer and double matrices (lines 834-839)
- Always converts to integers for disk storage
- Assumes input is in digital units (no conversion back from physical)

**Issue**: No clear contract about expected units in memory.

---

## WFDB C Library Approach

Based on documentation and the PhysioNet specification:

### Core Principle
**"Store digital, convert physical"**

### Key Components

1. **Primary Storage**: Integer digital values (ADC units)
   - Stored in header: `storage_format`, `ADC_gain`, `ADC_baseline`, `ADC_zero`
   - Signal data: Integer values read from disk

2. **Conversion Formula**:
   ```
   Physical = (Digital - Baseline) / Gain
   Digital = Physical × Gain + Baseline
   ```

3. **Conversion Functions** (from WFDB library):
   - `aduphys()`: ADC units → physical (absolute levels, with baseline)
   - `physadu()`: Physical → ADC units (absolute levels, with baseline)
   - `adumuv()`: ADC units → millivolts (differences only)
   - `muvadu()`: Millivolts → ADC units (differences only)

4. **Library Behavior**:
   - Always reads signals as integers
   - Provides conversion functions when physical values needed
   - Never stores physical values - always computed on demand
   - Preserves exact digital values for round-trip fidelity

---

## Proposed Refactoring

### Design Principles (First Principles Approach)

1. **Single Source of Truth**: Store only digital values in memory
2. **Lazy Evaluation**: Compute physical values only when requested
3. **Type Safety**: Make unit type explicit and enforced
4. **Immutability**: Conversions create new objects, don't mutate
5. **Discoverability**: Clear API that guides users to correct usage

---

### Proposed Architecture

#### 1. Enhanced `signal_table` Class

**Core Changes**:

```r
signal_table <- function(..., units = "digital") {
  x <- df_list(..., .name_repair = ~ make.unique(.x, sep = "_"))

  if (length(x) == 0) {
    return(new_signal_table(units = units))
  }

  # Check to see if a `sample` column exists
  if ('sample' %in% names(x)) {
    y <- x[c('sample', names(x)[which(names(x) != 'sample')])]
  } else {
    x$sample <- 1:max(lengths(x))
    y <- x[c('sample', names(x)[which(names(x) != 'sample')])]
  }

  # Enforce type based on units
  if (units == "digital") {
    # All signal columns must be integers
    checkmate::assert_list(y, types = c('integer', 'numeric'))
    # Coerce numeric to integer for signal columns
    signal_cols <- setdiff(names(y), 'sample')
    for (col in signal_cols) {
      if (!is.integer(y[[col]])) {
        y[[col]] <- as.integer(round(y[[col]]))
      }
    }
  } else if (units == "physical") {
    # Physical units can be numeric (double)
    checkmate::assert_list(y, types = 'numeric')
  }

  checkmate::assert_names(names(y), must.include = 'sample')
  checkmate::assert_integer(y$sample)

  new_signal_table(data = y, units = units)
}

new_signal_table <- function(data = list(), units = "digital") {
  new_data_frame(
    data,
    units = units,  # Store units as attribute
    class = c('signal_table', 'data.table')
  )
}
```

**New Methods**:

```r
#' Get units of signal_table
#' @export
units.signal_table <- function(x) {
  attr(x, "units") %||% "digital"
}

#' Convert signal_table to physical units
#' @param x signal_table object
#' @param header header_table with gain/baseline information
#' @export
to_physical <- function(x, header) {
  UseMethod("to_physical")
}

#' @export
to_physical.signal_table <- function(x, header) {
  if (units(x) == "physical") {
    warning("Signal is already in physical units")
    return(x)
  }

  # Extract conversion parameters from header
  signal_cols <- setdiff(names(x), 'sample')
  result <- copy(x)

  for (i in seq_along(signal_cols)) {
    col <- signal_cols[i]
    gain <- header$ADC_gain[i]
    baseline <- header$ADC_baseline[i]

    if (is.na(baseline)) baseline <- header$ADC_zero[i]
    if (is.na(baseline)) baseline <- 0L

    if (!is.na(gain) && gain != 0) {
      result[[col]] <- (as.numeric(x[[col]]) - baseline) / gain
    }
  }

  attr(result, "units") <- "physical"
  result
}

#' Convert signal_table to digital units
#' @param x signal_table object
#' @param header header_table with gain/baseline information
#' @export
to_digital <- function(x, header) {
  UseMethod("to_digital")
}

#' @export
to_digital.signal_table <- function(x, header) {
  if (units(x) == "digital") {
    warning("Signal is already in digital units")
    return(x)
  }

  # Convert from physical back to digital
  signal_cols <- setdiff(names(x), 'sample')
  result <- copy(x)

  for (i in seq_along(signal_cols)) {
    col <- signal_cols[i]
    gain <- header$ADC_gain[i]
    baseline <- header$ADC_baseline[i]

    if (is.na(baseline)) baseline <- header$ADC_zero[i]
    if (is.na(baseline)) baseline <- 0L

    if (!is.na(gain) && gain != 0) {
      result[[col]] <- as.integer(round(x[[col]] * gain + baseline))
    }
  }

  attr(result, "units") <- "digital"
  result
}
```

#### 2. Modified C++ Reading Function

**Change**: Return integers for digital mode

```cpp
// In read_signal_native_cpp
// Replace lines 683-687 with:

std::vector<writable::integers> output_columns_int;
std::vector<writable::doubles> output_columns_dbl;

if (physical) {
    output_columns_dbl.reserve(output_channels);
    for (int i = 0; i < output_channels; ++i) {
        output_columns_dbl.emplace_back(samples_to_read);
    }
} else {
    output_columns_int.reserve(output_channels);
    for (int i = 0; i < output_channels; ++i) {
        output_columns_int.emplace_back(samples_to_read);
    }
}

// ... in the reading loop:
if (physical) {
    // Store as double with conversion
    for (size_t output_idx : channel_map[channel_idx]) {
        output_columns_dbl[output_idx][output_index] = value;
    }
} else {
    // Store as integer without conversion
    int32_t digital_value = static_cast<int32_t>(raw_value);
    int baseline_value = adc_baseline[channel_idx];
    if (baseline_value != NA_INTEGER) {
        digital_value -= baseline_value;
    }
    for (size_t output_idx : channel_map[channel_idx]) {
        output_columns_int[output_idx][output_index] = digital_value;
    }
}

// ... when building result:
if (physical) {
    for (int i = 0; i < output_channels; ++i) {
        result.push_back(output_columns_dbl[i]);
    }
} else {
    for (int i = 0; i < output_channels; ++i) {
        result.push_back(output_columns_int[i]);
    }
}
```

#### 3. Modified R Reading Function

**Changes**:
- Default to digital units
- Store units metadata in signal_table
- Document the units clearly

```r
read_signal <- function(
  record,
  record_dir = ".",
  header = NULL,
  begin = 0,
  end = NA_integer_,
  interval = NA_integer_,
  units = c("digital", "physical"),
  channels = character(),
  ...
) {
  units <- match.arg(units)

  # ... existing code ...

  signal_list <- read_signal_native_cpp(
    data_path = fs::path(record_dir, file_names),
    number_of_channels = number_of_channels,
    total_samples = total_samples,
    storage_format = as.integer(header$storage_format),
    begin_sample = begin_sample,
    end_sample = end_sample,
    channel_indices = as.integer(header$number[selection] - 1L),
    adc_gain = as.numeric(header$ADC_gain),
    adc_baseline = as.integer(header$ADC_baseline),
    physical = identical(units, "physical"),
    channel_names = channel_names
  )

  signal <- data.table::as.data.table(signal_list)
  signal[, sample := as.integer(sample)]

  # Create signal_table with units metadata
  signal_table(signal, units = units)
}
```

#### 4. Modified Writing Function

**Changes**:
- Check units and convert if needed
- Document expected input format

```r
write_wfdb <- function(
  data,
  record,
  record_dir = ".",
  header = NULL,
  info_strings = list(),
  ...
) {
  # ... directory creation code ...

  if (inherits(data, "egm")) {
    if (!is.null(header)) {
      message(
        "Ignoring the supplied `header` because `data` is an `egm` object"
      )
    }
    signal <- data$signal
    header <- data$header
  } else {
    signal <- signal_table(data)
  }

  # NEW: Check units and convert if necessary
  if (is_signal_table(signal)) {
    signal_units <- units(signal)
    if (signal_units == "physical") {
      warning(
        "Signal data is in physical units. ",
        "Converting to digital units for WFDB storage. ",
        "Pass signal in digital units to avoid this conversion."
      )
      signal <- to_digital(signal, header)
    }
  }

  # ... rest of existing code ...

  # Ensure we're working with integers
  signal_dt <- data.table::as.data.table(signal)

  # ... existing column alignment code ...

  # Coerce to integer matrix for writing
  signal_matrix <- matrix(
    data = unlist(lapply(channel_list, as.integer), use.names = FALSE),
    nrow = nrow(channel_data),
    ncol = length(channel_cols),
    dimnames = list(NULL, channel_cols)
  )

  # ... rest of existing code ...
}
```

---

## Migration Strategy

### Phase 1: Add New Functions (Non-Breaking)
1. Add `units()`, `to_physical()`, `to_digital()` methods
2. Add `units` parameter to `signal_table()` (default "digital")
3. Add warnings in `write_wfdb()` for physical units
4. Update documentation

### Phase 2: Update C++ Layer (Non-Breaking)
1. Modify `read_signal_native_cpp()` to return integers for digital mode
2. Keep backward compatibility by checking `physical` flag
3. Add comprehensive tests for both modes

### Phase 3: Update Examples & Vignettes (Non-Breaking)
1. Update all examples to use digital units by default
2. Add examples showing physical conversion
3. Document the rationale in vignettes

### Phase 4: Deprecation (Breaking, v2.0)
1. Make `units = "digital"` mandatory for new code
2. Consider making `signal_table` integer-only for digital
3. Phase out implicit conversions

---

## Benefits of Proposed Approach

### 1. **Memory Efficiency**
- Integer storage: 4 bytes per value
- Double storage: 8 bytes per value
- **50% memory reduction** for digital signals

### 2. **Precision Preservation**
- No floating-point rounding errors
- Perfect round-trip: `read → write → read` yields identical values
- Critical for reproducibility

### 3. **Type Safety**
```r
# Clear and explicit
sig_digital <- read_signal(record, units = "digital")
sig_physical <- to_physical(sig_digital, header)

# Error prevented at write time
write_wfdb(sig_physical, record)  # Warning: converting back to digital

# Correct usage
write_wfdb(sig_digital, record)  # No conversion needed
```

### 4. **WFDB Compatibility**
- Aligns with C library semantics
- Tools like `wfdbcal`, `rdann`, `wrsamp` expect integers
- Easier integration with PhysioNet tools

### 5. **Debugging & Maintenance**
```r
# Easy to verify
units(signal)  # "digital" or "physical"

# Clear errors
if (units(signal) == "physical") {
  stop("This function requires digital units")
}
```

---

## Testing Strategy

### Unit Tests

```r
test_that("signal_table enforces integer storage for digital units", {
  sig <- signal_table(ch1 = 1:10, units = "digital")
  expect_true(is.integer(sig$ch1))
  expect_equal(units(sig), "digital")
})

test_that("conversion to physical units is accurate", {
  # Create test signal
  sig_digital <- signal_table(ch1 = c(100L, 200L, 300L), units = "digital")

  # Create test header
  hdr <- header_table(
    record_name = "test",
    number_of_channels = 1L,
    ADC_gain = 200,
    ADC_baseline = 0L,
    label = "CH1"
  )

  # Convert to physical
  sig_physical <- to_physical(sig_digital, hdr)

  # Expected: (digital - baseline) / gain = (100 - 0) / 200 = 0.5
  expect_equal(sig_physical$ch1[1], 0.5)
  expect_equal(sig_physical$ch1[2], 1.0)
  expect_equal(sig_physical$ch1[3], 1.5)
  expect_equal(units(sig_physical), "physical")
})

test_that("round-trip conversion preserves values", {
  sig_original <- signal_table(ch1 = c(100L, 200L, 300L), units = "digital")
  hdr <- header_table(
    record_name = "test",
    number_of_channels = 1L,
    ADC_gain = 200,
    ADC_baseline = 0L,
    label = "CH1"
  )

  sig_physical <- to_physical(sig_original, hdr)
  sig_back <- to_digital(sig_physical, hdr)

  expect_equal(sig_back$ch1, sig_original$ch1)
  expect_equal(units(sig_back), "digital")
})

test_that("write_wfdb converts physical to digital with warning", {
  sig_physical <- signal_table(ch1 = c(0.5, 1.0, 1.5), units = "physical")
  hdr <- header_table(
    record_name = "test",
    number_of_channels = 1L,
    ADC_gain = 200,
    ADC_baseline = 0L,
    label = "CH1"
  )

  tmp <- tempdir()

  expect_warning(
    write_wfdb(sig_physical, "test", tmp, header = hdr),
    "Converting to digital units"
  )
})

test_that("WFDB round-trip preserves integer values", {
  # Create digital signal
  sig_original <- signal_table(ch1 = c(100L, 200L, 300L), units = "digital")
  hdr <- header_table(
    record_name = "test",
    number_of_channels = 1L,
    frequency = 250,
    samples = 3L,
    ADC_gain = 200,
    ADC_baseline = 0L,
    label = "CH1"
  )

  tmp <- tempdir()
  egm_obj <- egm(sig_original, hdr)

  # Write
  write_wfdb(egm_obj, "roundtrip", tmp)

  # Read back
  egm_back <- read_wfdb("roundtrip", tmp, units = "digital")

  # Verify exact match
  expect_equal(egm_back$signal$ch1, sig_original$ch1)
  expect_true(is.integer(egm_back$signal$ch1))
})
```

---

## Code Removal Candidates

The following code can be simplified or removed:

1. **`R/wfdb-io.R` lines 830-883**: The dual integer/double matrix handling in `write_wfdb_native_cpp` can be simplified to only accept integer matrices when the signal is in digital units.

2. **Mixed type handling**: Since we enforce units at the R level, the C++ layer can be stricter about types.

3. **Implicit conversions**: Remove any implicit physical↔digital conversions that happen without user awareness.

---

## Documentation Updates

### User-Facing Documentation

**Key Messages**:
1. "WFDB files store signals as integers (digital/ADC units)"
2. "By default, signals are read as integers for efficiency and precision"
3. "Convert to physical units (mV, etc.) when needed for analysis"
4. "Always write signals in digital units to avoid conversion overhead"

**Examples to Add**:
```r
# Reading and working with digital units (recommended)
ecg <- read_wfdb("record", units = "digital")
plot(ecg$signal$sample, ecg$signal$I)  # Plot digital values

# Converting to physical units for analysis
ecg_mv <- to_physical(ecg$signal, ecg$header)
mean(ecg_mv$I)  # Mean in millivolts

# Converting back (rare, usually unnecessary)
ecg_digital <- to_digital(ecg_mv, ecg$header)

# Writing (always uses digital)
write_wfdb(ecg, "output")  # No conversion if already digital
```

---

## Conclusion

The proposed refactoring aligns the R implementation with the WFDB C library's fundamental design: **store digital, convert physical**. This approach:

- ✅ Reduces memory usage by 50%
- ✅ Eliminates floating-point precision issues
- ✅ Makes unit handling explicit and type-safe
- ✅ Simplifies debugging and maintenance
- ✅ Improves WFDB ecosystem compatibility
- ✅ Follows first principles of data representation

The migration can be done incrementally, maintaining backward compatibility during the transition. The final result will be a cleaner, more maintainable, and more correct implementation that better serves users working with WFDB physiological signals.

---

## Next Steps

1. **Review & Discuss**: Get feedback on this proposal
2. **Prototype**: Implement Phase 1 changes in a feature branch
3. **Test**: Comprehensive testing with real data
4. **Document**: Update all documentation
5. **Release**: Plan versioning strategy for breaking changes

