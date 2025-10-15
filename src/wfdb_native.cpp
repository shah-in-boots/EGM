#include "cpp11.hpp"
#include <algorithm>
#include <cstdint>
#include <fstream>
#include <limits>
#include <string>
#include <vector>

using cpp11::integers;
using cpp11::stop;
using cpp11::writable::doubles_matrix;

namespace {

constexpr int kFormat16 = 16;
constexpr int kFormat32 = 32;
constexpr int kFormat212 = 212;

[[nodiscard]] inline bool is_supported_format(const int storage_format) {
  return storage_format == kFormat16 ||
         storage_format == kFormat32 ||
         storage_format == kFormat212;
}

[[nodiscard]] inline int32_t decode_le_signed(const uint8_t* data,
                                              const int bytes_per_sample) {
  if (bytes_per_sample == 2) {
    const uint16_t value =
        static_cast<uint16_t>(data[0]) |
        (static_cast<uint16_t>(data[1]) << 8);
    return static_cast<int16_t>(value);
  }
  if (bytes_per_sample == 4) {
    const uint32_t value =
        static_cast<uint32_t>(data[0]) |
        (static_cast<uint32_t>(data[1]) << 8) |
        (static_cast<uint32_t>(data[2]) << 16) |
        (static_cast<uint32_t>(data[3]) << 24);
    return static_cast<int32_t>(value);
  }
  stop("Unsupported byte width encountered while decoding WFDB data");
}

[[nodiscard]] std::vector<int32_t> read_fixed_width_samples(
    std::ifstream& input,
    const std::string& path,
    const std::size_t offset_bytes,
    const std::size_t total_values,
    const int bytes_per_sample) {
  input.seekg(static_cast<std::streamoff>(offset_bytes), std::ios::beg);
  if (!input.good()) {
    stop("Failed to seek to requested offset in '%s'", path.c_str());
  }

  std::vector<uint8_t> raw(total_values * static_cast<std::size_t>(bytes_per_sample));
  input.read(reinterpret_cast<char*>(raw.data()),
             static_cast<std::streamsize>(raw.size()));
  if (input.gcount() < static_cast<std::streamsize>(raw.size())) {
    stop("WFDB signal file '%s' ended before the requested number of samples could be read", path.c_str());
  }

  std::vector<int32_t> values(total_values);
  for (std::size_t idx = 0; idx < total_values; ++idx) {
    values[idx] = decode_le_signed(
        raw.data() + idx * static_cast<std::size_t>(bytes_per_sample),
        bytes_per_sample);
  }

  return values;
}

[[nodiscard]] inline int16_t sign_extend_12bit(const uint16_t value) {
  const uint16_t masked = static_cast<uint16_t>(value & 0x0FFFu);
  if ((masked & 0x0800u) != 0) {
    return static_cast<int16_t>(static_cast<int16_t>(masked) | static_cast<int16_t>(0xF000));
  }
  return static_cast<int16_t>(masked);
}

[[nodiscard]] std::vector<int32_t> read_format212_samples(
    std::ifstream& input,
    const std::string& path,
    const int n_channels,
    const int start_sample,
    const int n_samples) {
  const std::size_t start_value =
      static_cast<std::size_t>(start_sample) *
      static_cast<std::size_t>(n_channels);
  const std::size_t total_values =
      static_cast<std::size_t>(n_samples) *
      static_cast<std::size_t>(n_channels);
  const std::size_t start_pair = start_value / 2;
  const bool skip_first = (start_value % 2) == 1;

  std::size_t values_to_decode = total_values + (skip_first ? 1u : 0u);
  if ((values_to_decode % 2u) != 0u) {
    ++values_to_decode;
  }

  const std::size_t pairs_needed = values_to_decode / 2u;
  const std::size_t offset_bytes = start_pair * 3u;
  const std::size_t bytes_to_read = pairs_needed * 3u;

  input.seekg(static_cast<std::streamoff>(offset_bytes), std::ios::beg);
  if (!input.good()) {
    stop("Failed to seek to requested offset in '%s'", path.c_str());
  }

  std::vector<uint8_t> raw(bytes_to_read);
  input.read(reinterpret_cast<char*>(raw.data()),
             static_cast<std::streamsize>(bytes_to_read));
  if (input.gcount() < static_cast<std::streamsize>(bytes_to_read)) {
    stop("WFDB signal file '%s' ended before the requested number of samples could be read", path.c_str());
  }

  std::vector<int32_t> decoded;
  decoded.reserve(values_to_decode + 1u);

  for (std::size_t pair = 0; pair < pairs_needed; ++pair) {
    const std::size_t offset = pair * 3u;
    const uint8_t b0 = raw[offset];
    const uint8_t b1 = raw[offset + 1u];
    const uint8_t b2 = raw[offset + 2u];

    const uint16_t sample1 = static_cast<uint16_t>(b0) |
                             (static_cast<uint16_t>(b1 & 0x0Fu) << 8u);
    const uint16_t sample2 = static_cast<uint16_t>(b2) |
                             (static_cast<uint16_t>(b1 & 0xF0u) << 4u);

    decoded.push_back(static_cast<int32_t>(sign_extend_12bit(sample1)));
    decoded.push_back(static_cast<int32_t>(sign_extend_12bit(sample2)));
  }

  const std::size_t begin_index = skip_first ? 1u : 0u;
  if (decoded.size() < begin_index + total_values) {
    stop("WFDB signal file '%s' did not contain enough data for the requested range", path.c_str());
  }

  std::vector<int32_t> values(total_values);
  std::copy(decoded.begin() + static_cast<std::ptrdiff_t>(begin_index),
            decoded.begin() + static_cast<std::ptrdiff_t>(begin_index + total_values),
            values.begin());

  return values;
}

[[nodiscard]] std::vector<int32_t> read_samples(
    const std::string& path,
    const int n_channels,
    const int start_sample,
    const int n_samples,
    const int storage_format) {
  std::ifstream input(path, std::ios::binary);
  if (!input.is_open()) {
    stop("Unable to open WFDB signal file '%s'", path.c_str());
  }

  const std::size_t start_sample_u = static_cast<std::size_t>(start_sample);
  const std::size_t n_channels_u = static_cast<std::size_t>(n_channels);
  const std::size_t total_values =
      static_cast<std::size_t>(n_samples) * n_channels_u;

  switch (storage_format) {
  case kFormat16: {
    const std::size_t offset_bytes = start_sample_u * n_channels_u * 2u;
    return read_fixed_width_samples(input, path, offset_bytes, total_values, 2);
  }
  case kFormat32: {
    const std::size_t offset_bytes = start_sample_u * n_channels_u * 4u;
    return read_fixed_width_samples(input, path, offset_bytes, total_values, 4);
  }
  case kFormat212:
    return read_format212_samples(input, path, n_channels, start_sample, n_samples);
  default:
    stop("Unsupported WFDB storage format %d", storage_format);
  }
}

[[nodiscard]] inline uint16_t encode_12bit(const int value,
                                           const std::string& path) {
  if (value < -2048 || value > 2047) {
    stop("Sample value %d is out of range for 12-bit storage in '%s'",
         value, path.c_str());
  }
  if (value >= 0) {
    return static_cast<uint16_t>(value);
  }
  return static_cast<uint16_t>(4096 + value);
}

void write_fixed_width_samples(std::ofstream& output,
                               const std::string& path,
                               const std::vector<int32_t>& values,
                               const int bytes_per_sample) {
  const std::size_t total_values = values.size();
  std::vector<uint8_t> raw(total_values * static_cast<std::size_t>(bytes_per_sample));

  for (std::size_t idx = 0; idx < total_values; ++idx) {
    const int32_t sample = values[idx];
    const std::size_t offset = idx * static_cast<std::size_t>(bytes_per_sample);

    if (bytes_per_sample == 2) {
      if (sample < std::numeric_limits<int16_t>::min() ||
          sample > std::numeric_limits<int16_t>::max()) {
        stop("Sample value %d is out of range for 16-bit storage in '%s'",
             sample, path.c_str());
      }
      const int16_t stored = static_cast<int16_t>(sample);
      raw[offset] = static_cast<uint8_t>(stored & 0xFF);
      raw[offset + 1u] = static_cast<uint8_t>((stored >> 8) & 0xFF);
    } else if (bytes_per_sample == 4) {
      const int32_t stored = sample;
      raw[offset] = static_cast<uint8_t>(stored & 0xFF);
      raw[offset + 1u] = static_cast<uint8_t>((stored >> 8) & 0xFF);
      raw[offset + 2u] = static_cast<uint8_t>((stored >> 16) & 0xFF);
      raw[offset + 3u] = static_cast<uint8_t>((stored >> 24) & 0xFF);
    } else {
      stop("Unsupported byte width encountered while writing WFDB data");
    }
  }

  output.write(reinterpret_cast<const char*>(raw.data()),
               static_cast<std::streamsize>(raw.size()));
}

void write_format212_samples(std::ofstream& output,
                             const std::string& path,
                             const std::vector<int32_t>& values) {
  const std::size_t total_values = values.size();
  const std::size_t pairs = (total_values + 1u) / 2u;
  std::vector<uint8_t> raw(pairs * 3u);

  std::size_t value_index = 0;
  for (std::size_t pair = 0; pair < pairs; ++pair) {
    const int32_t value1 = values[value_index];
    const uint16_t packed1 = encode_12bit(value1, path);

    int32_t value2 = 0;
    if (value_index + 1u < total_values) {
      value2 = values[value_index + 1u];
    }
    const uint16_t packed2 = encode_12bit(value2, path);

    const std::size_t offset = pair * 3u;
    raw[offset] = static_cast<uint8_t>(packed1 & 0xFF);
    raw[offset + 1u] = static_cast<uint8_t>(((packed2 >> 8u) & 0x0Fu) << 4u |
                                            ((packed1 >> 8u) & 0x0Fu));
    raw[offset + 2u] = static_cast<uint8_t>(packed2 & 0xFF);

    value_index += 2u;
  }

  output.write(reinterpret_cast<const char*>(raw.data()),
               static_cast<std::streamsize>(raw.size()));
}

} // namespace

// The cpp11 bridge returns a dense matrix of WFDB signal samples. The caller
// passes the path to the binary signal file along with metadata describing how
// many channels are interleaved in the file and which sample range should be
// read. Supported storage formats include classic 16-bit, 32-bit and packed
// 12-bit (format 212) signals.
[[cpp11::register]]
doubles_matrix<> read_wfdb_dat_cpp(const std::string& path,
                                   int n_channels,
                                   int start_sample,
                                   int n_samples,
                                   int storage_format) {
  if (n_channels <= 0) {
    stop("`n_channels` must be positive");
  }
  if (start_sample < 0) {
    stop("`start_sample` must be non-negative");
  }
  if (n_samples <= 0) {
    stop("`n_samples` must be positive");
  }
  if (!is_supported_format(storage_format)) {
    stop("Unsupported WFDB storage format %d", storage_format);
  }

  const std::vector<int32_t> values =
      read_samples(path, n_channels, start_sample, n_samples, storage_format);
  const std::size_t expected =
      static_cast<std::size_t>(n_samples) *
      static_cast<std::size_t>(n_channels);

  if (values.size() != expected) {
    stop("Unexpected number of samples returned from '%s'", path.c_str());
  }

  doubles_matrix<> result(n_samples, n_channels);
  for (int i = 0; i < n_samples; ++i) {
    for (int j = 0; j < n_channels; ++j) {
      const std::size_t idx =
          static_cast<std::size_t>(i) * static_cast<std::size_t>(n_channels) +
          static_cast<std::size_t>(j);
      result(i, j) = static_cast<double>(values[idx]);
    }
  }

  return result;
}

// The write helper accepts a row-major sequence of integer samples and stores
// them as an interleaved WFDB signal file.
[[cpp11::register]]
void write_wfdb_dat_cpp(const std::string& path,
                        const integers& samples,
                        int n_channels,
                        int storage_format) {
  if (n_channels <= 0) {
    stop("`n_channels` must be positive");
  }
  if (!is_supported_format(storage_format)) {
    stop("Unsupported WFDB storage format %d", storage_format);
  }

  const std::size_t total_values = static_cast<std::size_t>(samples.size());
  if (total_values == 0) {
    stop("No samples supplied for writing to '%s'", path.c_str());
  }
  if (total_values % static_cast<std::size_t>(n_channels) != 0u) {
    stop("Sample vector length must be divisible by the number of channels");
  }

  std::vector<int32_t> values(total_values);
  for (std::size_t idx = 0; idx < total_values; ++idx) {
    if (cpp11::is_na(samples[idx])) {
      stop("Cannot write NA samples to '%s'", path.c_str());
    }
    values[idx] = samples[idx];
  }

  std::ofstream output(path, std::ios::binary | std::ios::trunc);
  if (!output.is_open()) {
    stop("Unable to open '%s' for writing", path.c_str());
  }

  switch (storage_format) {
  case kFormat16:
    write_fixed_width_samples(output, path, values, 2);
    break;
  case kFormat32:
    write_fixed_width_samples(output, path, values, 4);
    break;
  case kFormat212:
    write_format212_samples(output, path, values);
    break;
  default:
    stop("Unsupported WFDB storage format %d", storage_format);
  }

  if (!output.good()) {
    stop("Failed to write WFDB signal data to '%s'", path.c_str());
  }
}
