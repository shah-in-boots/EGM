#include <cpp11.hpp>
#include <algorithm>
#include <array>
#include <cctype>
#include <cmath>
#include <cstdint>
#include <fstream>
#include <limits>
#include <sstream>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

// Code attempted to be written by a non-native C++ user
// Last updated: 2025-10-20
using namespace cpp11;

namespace {
// trim ----------------------------------------------------------------------
// Remove leading and trailing whitespace characters from a line read from a
// WFDB header file.  Header parsing relies heavily on splitting tokens and the
// reference format allows optional spacing, so we defensively normalise every
// string field before interpretation.
std::string trim(const std::string &input) {
        auto begin = input.begin();
        while (begin != input.end() && std::isspace(static_cast<unsigned char>(*begin))) {
                ++begin;
        }
        auto end = input.end();
        do {
                if (end == begin) {
                        break;
                }
                --end;
        } while (std::isspace(static_cast<unsigned char>(*end)));
        if (begin == input.end()) {
                return std::string();
        }
        return std::string(begin, end + 1);
}

// parse_double ---------------------------------------------------------------
// Convert a token extracted from the header file into a numeric value.  The
// WFDB specification allows missing values, which the R interface represents
// with NA.  Rather than forcing callers to handle exceptions we intercept any
// conversion error and return NA directly.
double parse_double(const std::string &value) {
        try {
                return std::stod(value);
        } catch (...) {
                return NA_REAL;
        }
}

// parse_int ------------------------------------------------------------------
// Same as parse_double() but for integer fields (e.g. ADC resolution).  These
// fields may be empty in legacy headers, therefore NA is the most informative
// failure mode for the R side of the API.
int parse_int(const std::string &value) {
        try {
                return std::stoi(value);
        } catch (...) {
                return NA_INTEGER;
        }
}

// ensure_can_open ------------------------------------------------------------
// Guard utility that throws a descriptive error when a file cannot be opened.
// All entry points call this immediately after constructing an ifstream so that
// downstream logic can assume a valid stream object.
void ensure_can_open(const std::ifstream &stream, const std::string &path) {
        if (!stream.is_open()) {
                stop("Unable to open '%s'", path.c_str());
        }
}

// FormatSize -----------------------------------------------------------------
// Helper for format_size(); numerator/denominator pairs are used to keep track
// of how many bytes are consumed by a sample for a particular storage format.
struct FormatSize {
        int numerator;
        int denominator;
};

// format_size ----------------------------------------------------------------
// Translate WFDB storage format identifiers into byte sizes.  Some formats,
// notably 212, span multiple channels and therefore require fractional values
// until we combine them later using an LCM (see lcm_ll()).
FormatSize format_size(int storage_format) {
        switch (storage_format) {
        case 8:
        case 80:
                return {1, 1};
        case 16:
                return {2, 1};
        case 24:
                return {3, 1};
        case 32:
                return {4, 1};
        case 212:
                return {3, 2};
        default:
                stop("Unsupported WFDB storage format: %d", storage_format);
        }
}

// gcd_ll / lcm_ll ------------------------------------------------------------
// Compute greatest common divisor / least common multiple for 64-bit integers.
// The sample-width computation uses these helpers to find a common denominator
// across channels that store different numbers of bytes per sample.
long long gcd_ll(long long a, long long b) {
        while (b != 0) {
                long long t = a % b;
                a = b;
                b = t;
        }
        return a;
}

long long lcm_ll(long long a, long long b) {
        if (a == 0 || b == 0) {
                return 0;
        }
        return (a / gcd_ll(a, b)) * b;
}

// Binary readers -------------------------------------------------------------
// The following helpers read signed integers encoded in little-endian order
// from the WFDB signal file.  Each function performs the read, checks for early
// EOF, and returns the correctly sign-extended value.
int8_t read_int8(std::istream &stream) {
        unsigned char buffer;
        stream.read(reinterpret_cast<char *>(&buffer), 1);
        if (!stream) {
                stop("Unexpected end of signal file");
        }
        return static_cast<int8_t>(buffer);
}

int16_t read_int16_little(std::istream &stream) {
        unsigned char buffer[2];
        stream.read(reinterpret_cast<char *>(buffer), 2);
        if (!stream) {
                stop("Unexpected end of signal file");
        }
        return static_cast<int16_t>(buffer[0] | (static_cast<int16_t>(buffer[1]) << 8));
}

int32_t read_int24_little(std::istream &stream) {
        unsigned char buffer[3];
        stream.read(reinterpret_cast<char *>(buffer), 3);
        if (!stream) {
                stop("Unexpected end of signal file");
        }
        int32_t value = static_cast<int32_t>(buffer[0]) |
                        (static_cast<int32_t>(buffer[1]) << 8) |
                        (static_cast<int32_t>(buffer[2]) << 16);
        if (value & 0x800000) {
                value |= ~0xFFFFFF;
        }
        return value;
}

int32_t read_int32_little(std::istream &stream) {
        unsigned char buffer[4];
        stream.read(reinterpret_cast<char *>(buffer), 4);
        if (!stream) {
                stop("Unexpected end of signal file");
        }
        return static_cast<int32_t>(buffer[0] | (static_cast<int32_t>(buffer[1]) << 8) |
                                    (static_cast<int32_t>(buffer[2]) << 16) |
                                    (static_cast<int32_t>(buffer[3]) << 24));
}

// read_format212_pair --------------------------------------------------------
// WFDB storage format 212 stores two 12-bit samples across three bytes.  This
// routine unpacks the bit fields into a pair of 16-bit integers while applying
// two's-complement sign extension to match the original values.
std::pair<int16_t, int16_t> read_format212_pair(std::istream &stream) {
        unsigned char buffer[3];
        stream.read(reinterpret_cast<char *>(buffer), 3);
        if (!stream) {
                stop("Unexpected end of signal file");
        }

        int raw_first = ((buffer[1] & 0x0F) << 8) | buffer[0];
        if (raw_first & 0x800) {
                raw_first -= 0x1000;
        }

        int raw_second = ((buffer[1] & 0xF0) << 4) | buffer[2];
        if (raw_second & 0x800) {
                raw_second -= 0x1000;
        }

        return {static_cast<int16_t>(raw_first), static_cast<int16_t>(raw_second)};
}

// Binary writers -------------------------------------------------------------
// Counterparts to the reader helpers above.  Each method writes a value back to
// disk in the WFDB-prescribed layout and surfaces any I/O failure immediately.
void write_int16_little(std::ostream &stream, int16_t value) {
        unsigned char buffer[2];
        buffer[0] = static_cast<unsigned char>(value & 0xFF);
        buffer[1] = static_cast<unsigned char>((value >> 8) & 0xFF);
        stream.write(reinterpret_cast<const char *>(buffer), 2);
        if (!stream) {
                stop("Failed to write signal data");
        }
}

void write_int24_little(std::ostream &stream, int32_t value) {
        unsigned char buffer[3];
        buffer[0] = static_cast<unsigned char>(value & 0xFF);
        buffer[1] = static_cast<unsigned char>((value >> 8) & 0xFF);
        buffer[2] = static_cast<unsigned char>((value >> 16) & 0xFF);
        stream.write(reinterpret_cast<const char *>(buffer), 3);
        if (!stream) {
                stop("Failed to write signal data");
        }
}

void write_int32_little(std::ostream &stream, int32_t value) {
        unsigned char buffer[4];
        buffer[0] = static_cast<unsigned char>(value & 0xFF);
        buffer[1] = static_cast<unsigned char>((value >> 8) & 0xFF);
        buffer[2] = static_cast<unsigned char>((value >> 16) & 0xFF);
        buffer[3] = static_cast<unsigned char>((value >> 24) & 0xFF);
        stream.write(reinterpret_cast<const char *>(buffer), 4);
        if (!stream) {
                stop("Failed to write signal data");
        }
}

// write_format212_pair -------------------------------------------------------
// Mirror of read_format212_pair(); converts two samples into the packed
// 3-byte representation while clamping the range to +/- 2048 as required by the
// original WFDB specification.
void write_format212_pair(std::ostream &stream, int16_t first, int16_t second) {
        int value1 = std::max<int>(-2048, std::min<int>(2047, first));
        int value2 = std::max<int>(-2048, std::min<int>(2047, second));

        unsigned int packed1 = static_cast<unsigned int>(value1) & 0x0FFFU;
        unsigned int packed2 = static_cast<unsigned int>(value2) & 0x0FFFU;

        unsigned char b0 = static_cast<unsigned char>(packed1 & 0xFFU);
        unsigned char b1 = static_cast<unsigned char>(((packed2 >> 8) & 0x0FU) << 4 |
                                                      ((packed1 >> 8) & 0x0FU));
        unsigned char b2 = static_cast<unsigned char>(packed2 & 0xFFU);

        unsigned char buffer[3] = {b0, b1, b2};
        stream.write(reinterpret_cast<const char *>(buffer), 3);
        if (!stream) {
                stop("Failed to write signal data");
        }
}

// annotation_symbol / annotation_code ---------------------------------------
// Bidirectional conversion between WFDB numeric annotation codes and their
// textual representations.  These helpers centralise the mapping used by both
// the reader and writer to remain consistent with upstream tools.
std::string annotation_symbol(int code) {
        static const std::array<const char *, 64> symbols = {{
                " ", "N", "L", "R", "a", "V", "F", "J", "A", "S",
                "E", "j", "/", "Q", "~", "", "|", "", "s", "T",
                "*", "D", "\"", "=", "p", "B", "^", "t", "+", "u",
                "?", "!", "[", "]", "e", "n", "@", "x", "f", "(",
                ")", "r", "", "", "", "", "", "", "", "",
                "", "", "", "", "", "", "", "", "", "",
                "", "", "", ""
        }};

        if (code >= 0 && code < static_cast<int>(symbols.size())) {
                const char *symbol = symbols[code];
                if (symbol[0] != '\0') {
                        return std::string(symbol);
                }
        }
        return std::string("?");
}

int annotation_code(const std::string &symbol) {
        static const std::unordered_map<std::string, int> code_map = {
                {"", 0},   {" ", 0}, {"N", 1}, {"L", 2}, {"R", 3}, {"a", 4}, {"V", 5}, {"F", 6},
                {"J", 7},  {"A", 8}, {"S", 9}, {"E", 10}, {"j", 11}, {"/", 12}, {"Q", 13},
                {"~", 14}, {"|", 16}, {"s", 18}, {"T", 19}, {"*", 20}, {"D", 21}, {"\"", 22},
                {"=", 23}, {"p", 24}, {"B", 25}, {"^", 26}, {"t", 27}, {"+", 28}, {"u", 29},
                {"?", 30}, {"!", 31}, {"[", 32}, {"]", 33}, {"e", 34}, {"n", 35}, {"@", 36},
                {"x", 37}, {"f", 38}, {"(", 39}, {")", 40}, {"r", 41}
        };

        auto it = code_map.find(symbol);
        if (it != code_map.end()) {
                return it->second;
        }

        if (symbol.size() == 1) {
                std::string single(1, symbol[0]);
                auto it_single = code_map.find(single);
                if (it_single != code_map.end()) {
                        return it_single->second;
                }
        }

        stop("Unsupported annotation symbol '%s'", symbol.c_str());
}

// read_skip_value ------------------------------------------------------------
// Annotation files use code 59 to encode "SKIP" records that store a 32-bit
// offset in a slightly unusual high/low word order.  This helper decodes the
// value and reports a clear error when the file terminates unexpectedly.
int32_t read_skip_value(std::istream &stream) {
        unsigned char buffer[4];
        stream.read(reinterpret_cast<char *>(buffer), 4);
        if (!stream) {
                stop("Unexpected end of annotation file while reading SKIP value");
        }

        uint32_t high = static_cast<uint32_t>(buffer[0]) |
                        (static_cast<uint32_t>(buffer[1]) << 8);
        uint32_t low = static_cast<uint32_t>(buffer[2]) |
                       (static_cast<uint32_t>(buffer[3]) << 8);

        uint32_t combined = (high << 16) | low;
        return static_cast<int32_t>(combined);
}

// write_skip_value -----------------------------------------------------------
// Serialise a SKIP record (code 59).  The WFDB format inserts the code header
// first, followed by the high and low halves of the offset.
void write_skip_value(std::ostream &stream, int32_t value) {
        unsigned char first[2] = {0, static_cast<unsigned char>(59 << 2)};
        stream.write(reinterpret_cast<const char *>(first), 2);
        if (!stream) {
                        stop("Failed to write annotation data");
        }

        uint32_t encoded = static_cast<uint32_t>(value);
        uint16_t high = static_cast<uint16_t>((encoded >> 16) & 0xFFFFu);
        uint16_t low = static_cast<uint16_t>(encoded & 0xFFFFu);

        unsigned char high_bytes[2] = {static_cast<unsigned char>(high & 0xFFu),
                                       static_cast<unsigned char>((high >> 8) & 0xFFu)};
        unsigned char low_bytes[2] = {static_cast<unsigned char>(low & 0xFFu),
                                      static_cast<unsigned char>((low >> 8) & 0xFFu)};

        stream.write(reinterpret_cast<const char *>(high_bytes), 2);
        stream.write(reinterpret_cast<const char *>(low_bytes), 2);
        if (!stream) {
                stop("Failed to write annotation data");
        }
}

// skip_auxiliary -------------------------------------------------------------
// Code 63 annotations carry arbitrary auxiliary data.  We intentionally ignore
// these payloads for now but must consume the byte stream (with odd-length
// padding) to keep subsequent reads aligned.
void skip_auxiliary(std::istream &stream, int length) {
        if (length <= 0) {
                return;
        }
        std::vector<char> buffer(length);
        stream.read(buffer.data(), length);
        if (!stream) {
                stop("Unexpected end of annotation file while reading AUX data");
        }
        if (length % 2 == 1) {
                stream.get();
                if (!stream) {
                        stop("Unexpected end of annotation file while reading AUX padding");
                }
        }
}

// write_annotation_pair ------------------------------------------------------
// Write a single two-byte annotation entry consisting of a 10-bit interval and
// a 6-bit code.  Validation ensures that a corrupted annotation table is caught
// before any data is flushed to disk.
void write_annotation_pair(std::ostream &stream, int code, int interval) {
        if (interval < 0 || interval > 1023) {
                stop("Annotation interval out of range");
        }
        if (code < 0 || code > 63) {
                stop("Annotation code out of range");
        }

        unsigned char byte0 = static_cast<unsigned char>(interval & 0xFF);
        unsigned char byte1 = static_cast<unsigned char>(((code & 0x3F) << 2) |
                                                         ((interval >> 8) & 0x03));
        unsigned char buffer[2] = {byte0, byte1};
        stream.write(reinterpret_cast<const char *>(buffer), 2);
        if (!stream) {
                stop("Failed to write annotation data");
        }
}

// encode_interval ------------------------------------------------------------
// Encode the elapsed sample count between annotations.  When the difference is
// larger than 10 bits we emit a separate SKIP record to preserve the full
// distance and return the remaining lower bits for the actual annotation pair.
int encode_interval(std::ostream &stream, int diff) {
        int final_interval = static_cast<int>(static_cast<uint32_t>(diff) & 0x3FFu);
        int skip_value = diff - final_interval;
        if (skip_value != 0) {
                write_skip_value(stream, static_cast<int32_t>(skip_value));
        }
        return final_interval;
}

} // namespace

[[cpp11::register]]
cpp11::writable::list read_header_native_cpp(const std::string &header_path) {
        std::ifstream stream(header_path);
        ensure_can_open(stream, header_path);

        // The first line of a WFDB header summarises the record level metadata
        // (record name, channel count, sampling frequency, etc.).  Subsequent
        // lines describe individual channels.  Each branch below mirrors the
        // layout documented in the WFDB specification so that the R front-end
        // can expose a tidy header_table object.
        std::string record_line;
        if (!std::getline(stream, record_line)) {
                stop("Header file '%s' is empty", header_path.c_str());
        }

        std::istringstream record_stream(record_line);
        std::string record_name;
        int number_of_channels = 0;
        double frequency = NA_REAL;
        int samples = NA_INTEGER;

        record_stream >> record_name;
        record_stream >> number_of_channels;
        record_stream >> frequency;
        record_stream >> samples;

        writable::strings file_name(number_of_channels);
        writable::integers storage_format(number_of_channels);
        writable::doubles adc_gain(number_of_channels);
        writable::integers adc_baseline(number_of_channels);
        writable::strings adc_units(number_of_channels);
        writable::integers adc_resolution(number_of_channels);
        writable::integers adc_zero(number_of_channels);
        writable::integers initial_value(number_of_channels);
        writable::integers checksum(number_of_channels);
        writable::integers blocksize(number_of_channels);
        writable::strings label(number_of_channels);

        std::string line;
        for (int i = 0; i < number_of_channels; ++i) {
                // Each channel line is tokenised in a WFDB-specific order.  We
                // read mandatory pieces first, then progressively parse the
                // optional gain/baseline/units components while gracefully
                // falling back to NA when the data is absent.
                if (!std::getline(stream, line)) {
                        stop("Header file '%s' ended unexpectedly while reading channel specifications", header_path.c_str());
                }
                if (line.empty()) {
                        --i;
                        continue;
                }

                std::istringstream line_stream(line);
                std::string file_token;
                std::string storage_token;
                std::string adc_token;

                line_stream >> file_token;
                line_stream >> storage_token;
                line_stream >> adc_token;

                file_name[i] = file_token;
                storage_format[i] = parse_int(storage_token);

                double gain_value = NA_REAL;
                int baseline_value = NA_INTEGER;
                std::string units_value;

                std::string before_units = adc_token;
                std::string after_units;
                auto slash_pos = adc_token.find('/');
                if (slash_pos != std::string::npos) {
                        before_units = adc_token.substr(0, slash_pos);
                        after_units = adc_token.substr(slash_pos + 1);
                        units_value = trim(after_units);
                }

                auto open_pos = before_units.find('(');
                auto close_pos = before_units.find(')');
                std::string gain_token = before_units;
                if (open_pos != std::string::npos && close_pos != std::string::npos && close_pos > open_pos) {
                        gain_token = before_units.substr(0, open_pos);
                        std::string baseline_token = before_units.substr(open_pos + 1, close_pos - open_pos - 1);
                        baseline_value = parse_int(trim(baseline_token));
                }

                gain_value = parse_double(trim(gain_token));

                adc_gain[i] = gain_value;
                adc_baseline[i] = baseline_value;
                adc_units[i] = units_value;

                std::string resolution_token;
                std::string zero_token;
                std::string initial_token;
                std::string checksum_token;
                std::string blocksize_token;

                // Remaining fields follow the ADC specification and can be
                // empty depending on the recording system.  parse_int/double
                // return NA which ensures the R side receives explicit missing
                // values rather than hard failures.
                line_stream >> resolution_token;
                line_stream >> zero_token;
                line_stream >> initial_token;
                line_stream >> checksum_token;
                line_stream >> blocksize_token;

                adc_resolution[i] = parse_int(resolution_token);
                adc_zero[i] = parse_int(zero_token);
                initial_value[i] = parse_int(initial_token);
                checksum[i] = parse_int(checksum_token);
                blocksize[i] = parse_int(blocksize_token);

                std::string description;
                std::getline(line_stream, description);
                description = trim(description);
                label[i] = description;
        }

        std::vector<writable::strings> info_values;
        std::vector<std::string> info_names;
        while (std::getline(stream, line)) {
                if (line.empty()) {
                        continue;
                }
                if (line[0] != '#') {
                        continue;
                }
                std::string content = trim(line.substr(1));
                if (content.empty()) {
                        continue;
                }
                std::istringstream info_stream(content);
                std::string key;
                info_stream >> key;
                std::vector<std::string> values;
                std::string token;
                while (info_stream >> token) {
                        values.push_back(token);
                }
                writable::strings value_vector(values.size());
                for (size_t j = 0; j < values.size(); ++j) {
                        value_vector[j] = values[j];
                }
                info_values.push_back(value_vector);
                info_names.push_back(key);
        }
        writable::list info_strings(info_values.size());
        for (size_t i = 0; i < info_values.size(); ++i) {
                info_strings[i] = info_values[i];
        }
        if (!info_names.empty()) {
                info_strings.names() = info_names;
        }

        writable::list result;
        result.push_back(as_sexp(record_line));
        result.push_back(as_sexp(record_name));
        result.push_back(as_sexp(number_of_channels));
        result.push_back(as_sexp(frequency));
        result.push_back(as_sexp(samples));
        result.push_back(file_name);
        result.push_back(storage_format);
        result.push_back(adc_gain);
        result.push_back(adc_baseline);
        result.push_back(adc_units);
        result.push_back(adc_resolution);
        result.push_back(adc_zero);
        result.push_back(initial_value);
        result.push_back(checksum);
        result.push_back(blocksize);
        result.push_back(label);
        result.push_back(info_strings);

        writable::strings names = {"record_line",      "record_name",   "number_of_channels", "frequency",     "samples",
                                   "file_name",       "storage_format", "adc_gain",          "adc_baseline", "adc_units",
                                   "adc_resolution",  "adc_zero",      "initial_value",      "checksum",      "blocksize",
                                   "label",           "info_strings"};
        result.names() = names;

        return result;
}

[[cpp11::register]]
cpp11::writable::list read_signal_native_cpp(const std::string &data_path,
                                             int number_of_channels,
                                             int total_samples,
                                             cpp11::integers storage_format,
                                             int begin_sample,
                                             int end_sample,
                                             cpp11::integers channel_indices,
                                             cpp11::doubles adc_gain,
                                             cpp11::integers adc_baseline,
                                             bool physical,
                                             cpp11::strings channel_names) {
        std::ifstream stream(data_path, std::ios::binary);
        ensure_can_open(stream, data_path);

        // Determine how many bytes of raw data correspond to a single sample.
        // The computation accounts for the paired-channel 212 format by
        // expressing everything as fractions and then finding the common
        // denominator across the recording.
        std::vector<FormatSize> sizes(number_of_channels);
        long long denom_lcm = 1;
        for (int i = 0; i < number_of_channels; ++i) {
                sizes[i] = format_size(storage_format[i]);
                denom_lcm = lcm_ll(denom_lcm, sizes[i].denominator);
        }

        long long bytes_per_sample_num = 0;
        for (int i = 0; i < number_of_channels; ++i) {
                bytes_per_sample_num += static_cast<long long>(sizes[i].numerator) *
                                        (denom_lcm / sizes[i].denominator);
        }

        if (bytes_per_sample_num == 0) {
                stop("Unable to determine bytes per sample");
        }

        if (total_samples <= 0) {
                // Some headers omit the sample count; in that case we infer the
                // number of samples by dividing the file size by the bytes per
                // sample ratio computed above.
                stream.seekg(0, std::ios::end);
                std::streamoff bytes = stream.tellg();
                if ((bytes * denom_lcm) % bytes_per_sample_num != 0) {
                        stop("Signal file size is not compatible with declared storage formats");
                }
                total_samples = static_cast<int>((bytes * denom_lcm) / bytes_per_sample_num);
                stream.seekg(0, std::ios::beg);
        }

        if (begin_sample < 0) {
                begin_sample = 0;
        }
        if (end_sample <= 0 || end_sample > total_samples) {
                end_sample = total_samples;
        }
        if (end_sample < begin_sample) {
                end_sample = begin_sample;
        }

        int samples_to_read = end_sample - begin_sample;
        if (samples_to_read < 0) {
                samples_to_read = 0;
        }
        writable::integers sample_column(samples_to_read);
        for (int i = 0; i < samples_to_read; ++i) {
                sample_column[i] = begin_sample + i;
        }

        // Pre-compute the subset of channels requested by the caller.  channel
        // indices are zero-based by convention in the native code to match the
        // binary format, hence the subtraction in the caller.
        int output_channels = channel_indices.size();
        std::vector<int> requested_channels(output_channels);
        for (int i = 0; i < output_channels; ++i) {
                int index = channel_indices[i];
                if (index < 0 || index >= number_of_channels) {
                        stop("Requested channel index %d is out of range", index + 1);
                }
                requested_channels[i] = index;
        }

        std::vector<std::vector<size_t>> channel_map(number_of_channels);
        for (size_t i = 0; i < requested_channels.size(); ++i) {
                channel_map[requested_channels[i]].push_back(i);
        }

        std::vector<writable::doubles> output_columns;
        output_columns.reserve(output_channels);
        for (int i = 0; i < output_channels; ++i) {
                output_columns.emplace_back(samples_to_read);
        }

        stream.seekg(0, std::ios::beg);

        int output_index = 0;
        for (int sample_idx = 0; sample_idx < end_sample; ++sample_idx) {
                bool store_row = sample_idx >= begin_sample;

                for (int channel_idx = 0; channel_idx < number_of_channels;) {
                        int fmt = storage_format[channel_idx];
                        if (fmt == 212) {
                                if (channel_idx + 1 >= number_of_channels || storage_format[channel_idx + 1] != 212) {
                                        stop("WFDB storage format 212 requires pairs of channels");
                                }
                                auto values = read_format212_pair(stream);

                                // Each pair contains two consecutive channels.
                                // We copy the values into every column that requested the
                                // channel (channel_map handles duplicates).
                                // Conversion to physical units is applied if requested.
                                if (store_row && !channel_map[channel_idx].empty()) {
                                        double value = static_cast<double>(values.first);
                                        if (physical) {
                                                // Convert to physical units: (digital - baseline) / gain
                                                int baseline_value = adc_baseline[channel_idx];
                                                if (baseline_value != NA_INTEGER) {
                                                        value -= static_cast<double>(baseline_value);
                                                }
                                                double gain_value = adc_gain[channel_idx];
                                                if (gain_value != NA_REAL && gain_value != 0.0) {
                                                        value /= gain_value;
                                                }
                                        }
                                        // If not physical, value remains as raw ADC count (digital units)
                                        for (size_t output_idx : channel_map[channel_idx]) {
                                                output_columns[output_idx][output_index] = value;
                                        }
                                }

                                if (store_row && !channel_map[channel_idx + 1].empty()) {
                                        double value = static_cast<double>(values.second);
                                        if (physical) {
                                                // Convert to physical units: (digital - baseline) / gain
                                                int baseline_value = adc_baseline[channel_idx + 1];
                                                if (baseline_value != NA_INTEGER) {
                                                        value -= static_cast<double>(baseline_value);
                                                }
                                                double gain_value = adc_gain[channel_idx + 1];
                                                if (gain_value != NA_REAL && gain_value != 0.0) {
                                                        value /= gain_value;
                                                }
                                        }
                                        // If not physical, value remains as raw ADC count (digital units)
                                        for (size_t output_idx : channel_map[channel_idx + 1]) {
                                                output_columns[output_idx][output_index] = value;
                                        }
                                }

                                channel_idx += 2;
                                continue;
                        }

                        double raw_value = 0.0;
                        switch (fmt) {
                        case 8:
                        case 80:
                                raw_value = static_cast<double>(read_int8(stream));
                                break;
                        case 16:
                                raw_value = static_cast<double>(read_int16_little(stream));
                                break;
                        case 24:
                                raw_value = static_cast<double>(read_int24_little(stream));
                                break;
                        case 32:
                                raw_value = static_cast<double>(read_int32_little(stream));
                                break;
                        default:
                                stop("Unsupported WFDB storage format: %d", fmt);
                        }

                        if (store_row && !channel_map[channel_idx].empty()) {
                                // Formats other than 212 map directly to a single channel.
                                // For digital units, preserve raw ADC counts.
                                // For physical units, apply conversion: (digital - baseline) / gain
                                double value = raw_value;
                                if (physical) {
                                        int baseline_value = adc_baseline[channel_idx];
                                        if (baseline_value != NA_INTEGER) {
                                                value -= static_cast<double>(baseline_value);
                                        }
                                        double gain_value = adc_gain[channel_idx];
                                        if (gain_value != NA_REAL && gain_value != 0.0) {
                                                value /= gain_value;
                                        }
                                }
                                // If not physical, value remains as raw ADC count (digital units)
                                for (size_t output_idx : channel_map[channel_idx]) {
                                        output_columns[output_idx][output_index] = value;
                                }
                        }

                        ++channel_idx;
                }

                if (store_row) {
                        ++output_index;
                }
        }

        writable::list result;
        result.push_back(sample_column);
        for (int i = 0; i < output_channels; ++i) {
                result.push_back(output_columns[i]);
        }

        writable::strings names(output_channels + 1);
        names[0] = "sample";
        for (int i = 0; i < output_channels; ++i) {
                names[i + 1] = channel_names[i];
        }
        result.names() = names;

        return result;
}

[[cpp11::register]]
void write_wfdb_native_cpp(const std::string &data_path,
                           const std::string &header_path,
                           cpp11::sexp signal_matrix_sexp,
                           cpp11::strings channel_names,
                           cpp11::strings file_names,
                           cpp11::integers storage_format,
                           cpp11::doubles adc_gain,
                           cpp11::integers adc_baseline,
                           cpp11::strings adc_units,
                           cpp11::integers adc_resolution,
                           cpp11::integers adc_zero,
                           cpp11::integers initial_value,
                           cpp11::integers checksum,
                           cpp11::integers blocksize,
                           double frequency,
                           int samples,
                           const std::string &record_name,
                           const std::string &start_time,
                           cpp11::list info_strings,
                           bool physical = false) {
        // Accept both integer and double matrices since WFDB signals can be
        // provided as either digital units (integers) or physical units (doubles).
        // On disk, WFDB always stores signals as integers, but the in-memory
        // representation in R may be either type.
        bool is_integer_matrix = TYPEOF(signal_matrix_sexp) == INTSXP;
        bool is_double_matrix = TYPEOF(signal_matrix_sexp) == REALSXP;

        if (!is_integer_matrix && !is_double_matrix) {
                stop("Signal matrix must be either integer or double type");
        }

        // Extract dimensions using the appropriate accessor
        SEXP dims = Rf_getAttrib(signal_matrix_sexp, R_DimSymbol);
        if (Rf_length(dims) != 2) {
                stop("Signal matrix must be a 2-dimensional matrix");
        }
        int nrow = INTEGER(dims)[0];
        int ncol = INTEGER(dims)[1];
        int number_of_channels = ncol;

        if (samples <= 0) {
                samples = nrow;
        }
        if (nrow != samples) {
                stop("Signal matrix rows do not match the expected number of samples");
        }

        std::ofstream data_stream(data_path, std::ios::binary);
        if (!data_stream.is_open()) {
                stop("Unable to open '%s' for writing", data_path.c_str());
        }

        std::vector<int> format_vec(number_of_channels);
        for (int i = 0; i < number_of_channels; ++i) {
                format_vec[i] = storage_format[i];
        }

        std::vector<int32_t> digital_row(number_of_channels);

        // Pre-extract the raw data pointers for efficient access
        const int *int_data = is_integer_matrix ? INTEGER(signal_matrix_sexp) : nullptr;
        const double *dbl_data = is_double_matrix ? REAL(signal_matrix_sexp) : nullptr;

        for (int i = 0; i < samples; ++i) {
                for (int channel_idx = 0; channel_idx < number_of_channels; ++channel_idx) {
                        // Extract value from either integer or double matrix
                        // Matrix is stored in column-major order: index = row + col * nrow
                        int matrix_index = i + channel_idx * nrow;
                        double value;
                        if (is_integer_matrix) {
                                value = static_cast<double>(int_data[matrix_index]);
                        } else {
                                value = dbl_data[matrix_index];
                        }

                        // Convert from physical to digital units if needed
                        // Formula: digital = (physical * gain) + baseline
                        if (physical) {
                                double gain_value = adc_gain[channel_idx];
                                int baseline_value = adc_baseline[channel_idx];
                                if (gain_value != NA_REAL && gain_value != 0.0) {
                                        value *= gain_value;
                                }
                                if (baseline_value != NA_INTEGER) {
                                        value += static_cast<double>(baseline_value);
                                }
                        }
                        // If not physical, value is already in digital units (raw ADC counts)

                        long long scaled = static_cast<long long>(std::llround(value));
                        int fmt = format_vec[channel_idx];
                        // Clamp to the legal range of the target storage format
                        // before casting down to the native integer width.  The
                        // ranges here mirror the limits enforced by the reader.
                        switch (fmt) {
                        case 8:
                        case 80:
                                if (scaled > std::numeric_limits<int8_t>::max()) {
                                        scaled = std::numeric_limits<int8_t>::max();
                                }
                                if (scaled < std::numeric_limits<int8_t>::min()) {
                                        scaled = std::numeric_limits<int8_t>::min();
                                }
                                break;
                        case 16:
                                if (scaled > std::numeric_limits<int16_t>::max()) {
                                        scaled = std::numeric_limits<int16_t>::max();
                                }
                                if (scaled < std::numeric_limits<int16_t>::min()) {
                                        scaled = std::numeric_limits<int16_t>::min();
                                }
                                break;
                        case 24:
                                if (scaled > 8388607LL) {
                                        scaled = 8388607LL;
                                }
                                if (scaled < -8388608LL) {
                                        scaled = -8388608LL;
                                }
                                break;
                        case 32:
                                if (scaled > std::numeric_limits<int32_t>::max()) {
                                        scaled = std::numeric_limits<int32_t>::max();
                                }
                                if (scaled < std::numeric_limits<int32_t>::min()) {
                                        scaled = std::numeric_limits<int32_t>::min();
                                }
                                break;
                        case 212:
                                if (scaled > 2047LL) {
                                        scaled = 2047LL;
                                }
                                if (scaled < -2048LL) {
                                        scaled = -2048LL;
                                }
                                break;
                        default:
                                stop("Unsupported WFDB storage format: %d", fmt);
                        }
                        digital_row[channel_idx] = static_cast<int32_t>(scaled);
                }

                for (int channel_idx = 0; channel_idx < number_of_channels;) {
                        int fmt = format_vec[channel_idx];
                        // Serialise each channel using the appropriate helper.
                        // Format 212 consumes two channels at a time, hence the
                        // manual increment when writing those pairs.
                        switch (fmt) {
                        case 8:
                        case 80:
                                data_stream.put(static_cast<char>(digital_row[channel_idx] & 0xFF));
                                if (!data_stream) {
                                        stop("Failed to write signal data");
                                }
                                ++channel_idx;
                                break;
                        case 16:
                                write_int16_little(data_stream, static_cast<int16_t>(digital_row[channel_idx]));
                                ++channel_idx;
                                break;
                        case 24:
                                write_int24_little(data_stream, digital_row[channel_idx]);
                                ++channel_idx;
                                break;
                        case 32:
                                write_int32_little(data_stream, digital_row[channel_idx]);
                                ++channel_idx;
                                break;
                        case 212:
                                if (channel_idx + 1 >= number_of_channels || format_vec[channel_idx + 1] != 212) {
                                        stop("WFDB storage format 212 requires pairs of channels");
                                }
                                write_format212_pair(data_stream,
                                                     static_cast<int16_t>(digital_row[channel_idx]),
                                                     static_cast<int16_t>(digital_row[channel_idx + 1]));
                                channel_idx += 2;
                                break;
                        default:
                                stop("Unsupported WFDB storage format: %d", fmt);
                        }
                }
        }
        data_stream.close();

        std::ofstream header_stream(header_path, std::ios::out | std::ios::trunc);
        if (!header_stream.is_open()) {
                        stop("Unable to open '%s' for writing", header_path.c_str());
        }

        // Record line --------------------------------------------------------
        // Assemble the first line in the WFDB header.  Optional start time is
        // appended when supplied.
        header_stream << record_name << " " << number_of_channels << " " << frequency << " " << samples;
        if (!start_time.empty()) {
                header_stream << " " << start_time;
        }
        header_stream << "\n";

        // Channel specifications --------------------------------------------
        for (int channel_idx = 0; channel_idx < number_of_channels; ++channel_idx) {
                std::string adc_spec;
                double gain_value = adc_gain[channel_idx];
                if (gain_value == NA_REAL) {
                        gain_value = 200.0;
                }
                int baseline_value = adc_baseline[channel_idx];
                std::ostringstream adc_stream;
                adc_stream << gain_value;
                if (baseline_value != NA_INTEGER) {
                        adc_stream << "(" << baseline_value << ")";
                }
                std::string units;
                if (!is_na(adc_units[channel_idx])) {
                        units = static_cast<std::string>(adc_units[channel_idx]);
                }
                if (!units.empty()) {
                        adc_stream << "/" << units;
                }
                adc_spec = adc_stream.str();

                int resolution_value = adc_resolution[channel_idx];
                if (resolution_value == NA_INTEGER) {
                        resolution_value = 16;
                }
                int zero_value = adc_zero[channel_idx];
                if (zero_value == NA_INTEGER) {
                        zero_value = 0;
                }
                int initial = initial_value[channel_idx];
                if (initial == NA_INTEGER) {
                        initial = zero_value;
                }
                int checksum_value = checksum[channel_idx];
                if (checksum_value == NA_INTEGER) {
                        checksum_value = 0;
                }
                int blocksize_value = blocksize[channel_idx];
                if (blocksize_value == NA_INTEGER) {
                        blocksize_value = 0;
                }

                std::string file_name;
                if (!is_na(file_names[channel_idx])) {
                        file_name = static_cast<std::string>(file_names[channel_idx]);
                }
                if (file_name.empty()) {
                        file_name = record_name + ".dat";
                }

                std::string label_value;
                if (!is_na(channel_names[channel_idx])) {
                        label_value = static_cast<std::string>(channel_names[channel_idx]);
                }
                if (label_value.empty()) {
                        label_value = "CH" + std::to_string(channel_idx + 1);
                }

                header_stream << file_name << "\t" << storage_format[channel_idx] << "\t" << adc_spec << "\t" << resolution_value
                              << "\t" << zero_value << "\t" << initial << "\t" << checksum_value << "\t" << blocksize_value << "\t"
                              << label_value << "\n";
        }

        // Info strings -------------------------------------------------------
        // Additional metadata is written as `# key value` lines.  This mirrors
        // the behaviour of upstream WFDB tools and allows R callers to roundtrip
        // arbitrary channel annotations.
        cpp11::strings info_names(info_strings.names());
        for (int i = 0; i < info_strings.size(); ++i) {
                cpp11::r_string key_proxy(info_names[i]);
                std::string key = static_cast<std::string>(key_proxy);
                cpp11::sexp value_sexp = info_strings[i];
                cpp11::strings values(value_sexp);
                header_stream << "# " << key;
                for (int j = 0; j < values.size(); ++j) {
                        cpp11::r_string value_proxy(values[j]);
                        std::string val = static_cast<std::string>(value_proxy);
                        header_stream << " " << val;
                }
                header_stream << "\n";
        }

        header_stream.close();
}

[[cpp11::register]]
cpp11::writable::list read_annotation_native_cpp(const std::string &annotation_path) {
        std::ifstream stream(annotation_path, std::ios::binary);
        ensure_can_open(stream, annotation_path);

        // The annotation reader keeps track of the running sample position.
        // Special codes (59-63) mutate state rather than producing actual
        // annotations, so the logic below resembles a small state machine.
        std::vector<int> samples;
        std::vector<std::string> types;
        std::vector<int> subtypes;
        std::vector<int> channels;
        std::vector<int> numbers;

        int32_t current_sample = 0;
        int pending_num = 0;
        bool has_pending_num = false;
        int pending_subtype = 0;
        bool has_pending_subtype = false;
        int pending_channel = 0;
        bool has_pending_channel = false;

        while (true) {
                int first = stream.get();
                if (!stream) {
                        break;
                }
                int second = stream.get();
                if (!stream) {
                        stop("Unexpected end of annotation file");
                }

                int code = (second >> 2) & 0x3F;
                int interval = ((second & 0x03) << 8) | (first & 0xFF);

                if (code == 0 && interval == 0) {
                        break;
                }

                if (code == 59) {
                        int32_t skip = read_skip_value(stream);
                        current_sample += skip;
                        continue;
                }

                if (code == 60) {
                        if (!numbers.empty()) {
                                numbers.back() = interval;
                        } else {
                                pending_num = interval;
                                has_pending_num = true;
                        }
                        continue;
                }

                if (code == 61) {
                        if (!subtypes.empty()) {
                                subtypes.back() = interval;
                        } else {
                                pending_subtype = interval;
                                has_pending_subtype = true;
                        }
                        continue;
                }

                if (code == 62) {
                        if (!channels.empty()) {
                                channels.back() = interval;
                        } else {
                                pending_channel = interval;
                                has_pending_channel = true;
                        }
                        continue;
                }

                if (code == 63) {
                        skip_auxiliary(stream, interval);
                        continue;
                }

                current_sample += interval;
                std::string symbol = annotation_symbol(code);
                int subtype_value = has_pending_subtype ? pending_subtype : 0;
                int channel_value = has_pending_channel ? pending_channel : 0;
                int num_value = has_pending_num ? pending_num : 0;

                samples.push_back(current_sample);
                types.push_back(symbol);
                subtypes.push_back(subtype_value);
                channels.push_back(channel_value);
                numbers.push_back(num_value);

                has_pending_num = false;
                has_pending_subtype = false;
                has_pending_channel = false;
        }

        size_t n = samples.size();
        cpp11::writable::integers sample_vec(n);
        cpp11::writable::strings type_vec(n);
        cpp11::writable::integers subtype_vec(n);
        cpp11::writable::integers channel_vec(n);
        cpp11::writable::integers number_vec(n);

        for (size_t i = 0; i < n; ++i) {
                sample_vec[i] = samples[i];
                type_vec[i] = types[i];
                subtype_vec[i] = subtypes[i];
                channel_vec[i] = channels[i];
                number_vec[i] = numbers[i];
        }

        cpp11::writable::list result;
        result.push_back(sample_vec);
        result.push_back(type_vec);
        result.push_back(subtype_vec);
        result.push_back(channel_vec);
        result.push_back(number_vec);

        cpp11::writable::strings names = {"sample", "type", "subtype", "channel", "number"};
        result.names() = names;

        return result;
}

[[cpp11::register]]
void write_annotation_native_cpp(const std::string &annotation_path,
                                 cpp11::integers samples,
                                 cpp11::strings types,
                                 cpp11::integers subtypes,
                                 cpp11::integers channels,
                                 cpp11::integers numbers) {
        if (samples.size() != types.size()) {
                stop("Samples and types must have the same length");
        }

        std::ofstream stream(annotation_path, std::ios::binary | std::ios::trunc);
        if (!stream.is_open()) {
                stop("Unable to open '%s' for writing", annotation_path.c_str());
        }

        int prev_sample = 0;

        auto get_value = [](const cpp11::integers &vec, int index) {
                if (index >= vec.size()) {
                        return 0;
                }
                int value = vec[index];
                if (cpp11::is_na(value)) {
                        return 0;
                }
                return value;
        };

        // Emit annotations in order, encoding the sample delta relative to the
        // previous entry.  Optional subtype/channel/number fields are only
        // written when non-zero to keep the output compact.
        for (int i = 0; i < samples.size(); ++i) {
                int sample = samples[i];
                if (cpp11::is_na(sample)) {
                        stop("Sample values must be finite integers");
                }
                if (sample < prev_sample) {
                        stop("Annotation samples must be non-decreasing");
                }

                std::string symbol;
                if (cpp11::is_na(types[i])) {
                        symbol = "";
                } else {
                        symbol = static_cast<std::string>(types[i]);
                }
                int code = annotation_code(symbol);

                int diff = sample - prev_sample;
                int interval = encode_interval(stream, diff);
                write_annotation_pair(stream, code, interval);

                int subtype_value = get_value(subtypes, i);
                if (subtype_value < 0 || subtype_value > 1023) {
                        stop("Annotation subtype must be between 0 and 1023");
                }
                if (subtype_value != 0) {
                        write_annotation_pair(stream, 61, subtype_value);
                }

                int channel_value = get_value(channels, i);
                if (channel_value < 0 || channel_value > 1023) {
                        stop("Annotation channel must be between 0 and 1023");
                }
                if (channel_value != 0) {
                        write_annotation_pair(stream, 62, channel_value);
                }

                int num_value = get_value(numbers, i);
                if (num_value < 0 || num_value > 1023) {
                        stop("Annotation number must be between 0 and 1023");
                }
                if (num_value != 0) {
                        write_annotation_pair(stream, 60, num_value);
                }

                prev_sample = sample;
        }

        unsigned char terminator[2] = {0, 0};
        stream.write(reinterpret_cast<const char *>(terminator), 2);
        if (!stream) {
                stop("Failed to finalize annotation file");
        }
}
