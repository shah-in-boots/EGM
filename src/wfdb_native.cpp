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

constexpr int kCodeShift = 10;
constexpr uint16_t kDataMask = 0x03FFu;

constexpr int kPseudoSkip = 59;
constexpr int kPseudoNum = 60;
constexpr int kPseudoSub = 61;
constexpr int kPseudoChan = 62;
constexpr int kPseudoAux = 63;

constexpr int kMaxSkip = 0x7FFFFFFF;
constexpr int kMinSkip = -0x7FFFFFFF;

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

[[nodiscard]] inline bool read_uint16(std::ifstream& input,
                                      const std::string& path,
                                      uint16_t& value) {
  unsigned char buffer[2] = {0, 0};
  input.read(reinterpret_cast<char*>(buffer), 2);
  const std::streamsize read = input.gcount();
  if (read == 0) {
    return false;
  }
  if (read < 2) {
    stop("Annotation file '%s' ended unexpectedly", path.c_str());
  }
  value = static_cast<uint16_t>(buffer[0]) |
          (static_cast<uint16_t>(buffer[1]) << 8u);
  return true;
}

[[nodiscard]] inline int32_t read_int32(std::ifstream& input,
                                        const std::string& path) {
  uint16_t high = 0;
  uint16_t low = 0;
  if (!read_uint16(input, path, high) || !read_uint16(input, path, low)) {
    stop("Annotation file '%s' ended unexpectedly", path.c_str());
  }
  const uint32_t combined =
      (static_cast<uint32_t>(high) << 16u) | static_cast<uint32_t>(low);
  return static_cast<int32_t>(combined);
}

[[nodiscard]] inline int decode_signed_10bit(const uint16_t word) {
  const int value = static_cast<int>(word & kDataMask);
  if ((value & 0x0200) != 0) {
    return value - 0x0400;
  }
  return value;
}

[[nodiscard]] inline uint16_t encode_signed_10bit(const int value,
                                                  const std::string& path,
                                                  const char* field) {
  if (value < -512 || value > 511) {
    stop("Annotation %s value %d is outside the supported range [-512, 511] for '%s'",
         field, value, path.c_str());
  }
  if (value < 0) {
    return static_cast<uint16_t>(value + 0x0400);
  }
  return static_cast<uint16_t>(value);
}

inline void write_uint16(std::ofstream& output,
                         const std::string& path,
                         const uint16_t value) {
  const unsigned char buffer[2] = {
      static_cast<unsigned char>(value & 0x00FFu),
      static_cast<unsigned char>((value >> 8u) & 0x00FFu)};
  output.write(reinterpret_cast<const char*>(buffer), 2);
  if (!output.good()) {
    stop("Failed to write annotation data to '%s'", path.c_str());
  }
}

inline void write_int32(std::ofstream& output,
                        const std::string& path,
                        const int32_t value) {
  const uint32_t stored = static_cast<uint32_t>(value);
  const unsigned char buffer[4] = {
      static_cast<unsigned char>((stored >> 0u) & 0xFFu),
      static_cast<unsigned char>((stored >> 8u) & 0xFFu),
      static_cast<unsigned char>((stored >> 16u) & 0xFFu),
      static_cast<unsigned char>((stored >> 24u) & 0xFFu)};
  // The MIT format stores the high 16 bits first, then the low 16 bits, with
  // little-endian byte order within each pair. Writing the bytes in this order
  // achieves the same layout when interpreted sequentially.
  const unsigned char ordered[4] = {buffer[2], buffer[3], buffer[0], buffer[1]};
  output.write(reinterpret_cast<const char*>(ordered), 4);
  if (!output.good()) {
    stop("Failed to write annotation data to '%s'", path.c_str());
  }
}

inline void write_skip(std::ofstream& output,
                       const std::string& path,
                       const int32_t delta) {
  const uint16_t word = static_cast<uint16_t>(kPseudoSkip << kCodeShift);
  write_uint16(output, path, word);
  write_int32(output, path, delta);
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

[[cpp11::register]]
cpp11::writable::list read_wfdb_ann_cpp(const std::string& path) {
  std::ifstream input(path, std::ios::binary);
  if (!input.is_open()) {
    stop("Unable to open annotation file '%s'", path.c_str());
  }

  std::vector<int32_t> samples;
  std::vector<int32_t> types;
  std::vector<int32_t> subtypes;
  std::vector<int32_t> channels;
  std::vector<int32_t> numbers;
  std::vector<std::string> aux_strings;

  int64_t current_time = 0;
  int current_num = 0;
  int current_channel = 0;
  bool has_pending = false;
  uint16_t pending_word = 0;
  bool reached_physical_eof = false;

  while (true) {
    uint16_t word = 0;
    if (has_pending) {
      word = pending_word;
      has_pending = false;
    } else {
      if (!read_uint16(input, path, word)) {
        break;
      }
    }

    if (word == 0u) {
      break;
    }

    const int code = static_cast<int>(word >> kCodeShift);
    if (code == kPseudoSkip) {
      const int32_t skip = read_int32(input, path);
      current_time += static_cast<int64_t>(skip);
      continue;
    }

    const int interval = decode_signed_10bit(word);
    current_time += static_cast<int64_t>(interval);

    int ann_type = code;
    int ann_subtype = 0;
    int ann_channel = current_channel;
    int ann_number = current_num;
    std::string ann_aux;

    while (true) {
      uint16_t next_word = 0;
      if (!read_uint16(input, path, next_word)) {
        reached_physical_eof = true;
        break;
      }
      if (next_word == 0u) {
        has_pending = true;
        pending_word = next_word;
        break;
      }
      const int next_code = static_cast<int>(next_word >> kCodeShift);
      if (next_code < kPseudoSkip) {
        has_pending = true;
        pending_word = next_word;
        break;
      }

      switch (next_code) {
      case kPseudoSkip: {
        const int32_t skip = read_int32(input, path);
        current_time += static_cast<int64_t>(skip);
        break;
      }
      case kPseudoNum: {
        current_num = decode_signed_10bit(next_word);
        ann_number = current_num;
        break;
      }
      case kPseudoSub: {
        ann_subtype = decode_signed_10bit(next_word);
        break;
      }
      case kPseudoChan: {
        current_channel = decode_signed_10bit(next_word);
        ann_channel = current_channel;
        break;
      }
      case kPseudoAux: {
        const int len = static_cast<int>(next_word & 0x00FFu);
        std::string buffer;
        if (len > 0) {
          buffer.resize(static_cast<std::size_t>(len));
          input.read(buffer.data(), len);
          if (input.gcount() != len) {
            stop("Annotation file '%s' ended unexpectedly while reading auxiliary data",
                 path.c_str());
          }
        }
        if ((len & 1) != 0) {
          char padding = 0;
          input.read(&padding, 1);
          if (!input.good()) {
            stop("Annotation file '%s' ended unexpectedly while reading auxiliary data",
                 path.c_str());
          }
        }
        ann_aux = buffer;
        break;
      }
      default:
        break;
      }
    }

    if (current_time > std::numeric_limits<int32_t>::max() ||
        current_time < std::numeric_limits<int32_t>::min()) {
      stop("Annotation sample %lld cannot be represented as a 32-bit integer",
           static_cast<long long>(current_time));
    }

    samples.push_back(static_cast<int32_t>(current_time));
    types.push_back(ann_type);
    subtypes.push_back(ann_subtype);
    channels.push_back(ann_channel);
    numbers.push_back(ann_number);
    aux_strings.push_back(ann_aux);

    if (reached_physical_eof) {
      break;
    }
  }

  const std::size_t n = samples.size();
  cpp11::writable::integers samples_r(n);
  cpp11::writable::integers types_r(n);
  cpp11::writable::integers subtypes_r(n);
  cpp11::writable::integers channels_r(n);
  cpp11::writable::integers numbers_r(n);
  cpp11::writable::strings aux_r(n);

  for (std::size_t i = 0; i < n; ++i) {
    samples_r[i] = samples[i];
    types_r[i] = types[i];
    subtypes_r[i] = subtypes[i];
    channels_r[i] = channels[i];
    numbers_r[i] = numbers[i];
    aux_r[i] = aux_strings[i];
  }

  cpp11::writable::list result;
  result.push_back(samples_r);
  result.push_back(types_r);
  result.push_back(subtypes_r);
  result.push_back(channels_r);
  result.push_back(numbers_r);
  result.push_back(aux_r);

  cpp11::writable::strings names = {"sample", "type", "subtype", "channel", "number", "aux"};
  result.attr("names") = names;

  return result;
}

[[cpp11::register]]
void write_wfdb_ann_cpp(const std::string& path,
                         const integers& samples,
                         const integers& types,
                         const integers& subtypes,
                         const integers& channels,
                         const integers& numbers,
                         const cpp11::strings& aux) {
  const std::size_t n = static_cast<std::size_t>(samples.size());
  if (types.size() != static_cast<R_xlen_t>(n) ||
      subtypes.size() != static_cast<R_xlen_t>(n) ||
      channels.size() != static_cast<R_xlen_t>(n) ||
      numbers.size() != static_cast<R_xlen_t>(n)) {
    stop("Annotation fields must have the same length");
  }
  if (aux.size() != 0 && aux.size() != static_cast<R_xlen_t>(n)) {
    stop("Auxiliary data must be empty or match the number of annotations");
  }

  std::ofstream output(path, std::ios::binary | std::ios::trunc);
  if (!output.is_open()) {
    stop("Unable to open annotation file '%s' for writing", path.c_str());
  }

  int64_t previous_time = 0;
  int last_channel = 0;
  int last_number = 0;

  for (std::size_t i = 0; i < n; ++i) {
    if (cpp11::is_na(samples[i])) {
      stop("Annotation sample index cannot be NA");
    }
    if (cpp11::is_na(types[i])) {
      stop("Annotation type code cannot be NA");
    }

    const int64_t current_time = static_cast<int64_t>(samples[i]);
    const int type_code = types[i];
    if (type_code < 0 || type_code > 63) {
      stop("Annotation type code %d is outside the supported range [0, 63]", type_code);
    }

    int subtype_value = subtypes[i];
    if (cpp11::is_na(subtype_value)) {
      subtype_value = 0;
    }

    int channel_value = channels[i];
    if (cpp11::is_na(channel_value)) {
      channel_value = 0;
    }

    int number_value = numbers[i];
    if (cpp11::is_na(number_value)) {
      number_value = 0;
    }

    std::string aux_value;
    if (aux.size() == static_cast<R_xlen_t>(n)) {
      const SEXP element = aux[i];
      if (element != NA_STRING) {
        aux_value = cpp11::as_cpp<std::string>(element);
      }
    }

    int64_t delta = current_time - previous_time;
    while (delta > kMaxSkip) {
      write_skip(output, path, kMaxSkip);
      previous_time += kMaxSkip;
      delta = current_time - previous_time;
    }
    while (delta < kMinSkip) {
      write_skip(output, path, kMinSkip);
      previous_time += kMinSkip;
      delta = current_time - previous_time;
    }

    if (type_code == 0) {
      const int64_t adjust = delta - 1;
      if (adjust > kMaxSkip || adjust < kMinSkip) {
        stop("Unable to encode null annotation with the requested timing");
      }
      write_skip(output, path, static_cast<int32_t>(adjust));
      previous_time += adjust;
      delta = 1;
    }

    if (delta < 0 || delta > kDataMask) {
      if (delta > kMaxSkip || delta < kMinSkip) {
        stop("Annotation timing difference %lld exceeds the supported range",
             static_cast<long long>(delta));
      }
      write_skip(output, path, static_cast<int32_t>(delta));
      previous_time += delta;
      delta = 0;
    }

    const uint16_t data = static_cast<uint16_t>(delta) & kDataMask;
    const uint16_t word =
        static_cast<uint16_t>((type_code << kCodeShift) | data);
    write_uint16(output, path, word);
    previous_time += delta;

    if (subtype_value != 0) {
      const uint16_t sub_word = static_cast<uint16_t>(
          (kPseudoSub << kCodeShift) |
          encode_signed_10bit(subtype_value, path, "subtype"));
      write_uint16(output, path, sub_word);
    }

    if (channel_value != last_channel) {
      const uint16_t chan_word = static_cast<uint16_t>(
          (kPseudoChan << kCodeShift) |
          encode_signed_10bit(channel_value, path, "channel"));
      write_uint16(output, path, chan_word);
      last_channel = channel_value;
    }

    if (number_value != last_number) {
      const uint16_t num_word = static_cast<uint16_t>(
          (kPseudoNum << kCodeShift) |
          encode_signed_10bit(number_value, path, "number"));
      write_uint16(output, path, num_word);
      last_number = number_value;
    }

    if (!aux_value.empty()) {
      if (aux_value.size() > 255u) {
        stop("Auxiliary strings longer than 255 bytes are not supported");
      }
      const uint16_t aux_word = static_cast<uint16_t>(
          (kPseudoAux << kCodeShift) |
          static_cast<uint16_t>(aux_value.size()));
      write_uint16(output, path, aux_word);
      output.write(aux_value.data(),
                   static_cast<std::streamsize>(aux_value.size()));
      if (!output.good()) {
        stop("Failed to write auxiliary annotation data to '%s'", path.c_str());
      }
      if ((aux_value.size() & 1u) != 0u) {
        const char pad = '\0';
        output.write(&pad, 1);
        if (!output.good()) {
          stop("Failed to write auxiliary annotation padding to '%s'", path.c_str());
        }
      }
    }
  }

  write_uint16(output, path, 0u);

  output.flush();
  if (!output.good()) {
    stop("Failed to finalise annotation file '%s'", path.c_str());
  }
}
