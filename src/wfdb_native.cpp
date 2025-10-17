#include <cpp11.hpp>
#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdint>
#include <fstream>
#include <limits>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

using namespace cpp11;

namespace {
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

double parse_double(const std::string &value) {
        try {
                return std::stod(value);
        } catch (...) {
                return NA_REAL;
        }
}

int parse_int(const std::string &value) {
        try {
                return std::stoi(value);
        } catch (...) {
                return NA_INTEGER;
        }
}

void ensure_can_open(const std::ifstream &stream, const std::string &path) {
        if (!stream.is_open()) {
                stop("Unable to open '%s'", path.c_str());
        }
}

struct FormatSize {
        int numerator;
        int denominator;
};

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

} // namespace

[[cpp11::register]]
cpp11::writable::list read_header_native_cpp(const std::string &header_path) {
        std::ifstream stream(header_path);
        ensure_can_open(stream, header_path);

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

        writable::list info_strings;
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
                info_strings.push_back(value_vector);
                info_names.push_back(key);
        }
        info_strings.names() = info_names;

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

                                if (store_row && !channel_map[channel_idx].empty()) {
                                        double value = static_cast<double>(values.first);
                                        int baseline_value = adc_baseline[channel_idx];
                                        if (baseline_value != NA_INTEGER) {
                                                value -= static_cast<double>(baseline_value);
                                        }
                                        if (physical) {
                                                double gain_value = adc_gain[channel_idx];
                                                if (gain_value != NA_REAL && gain_value != 0.0) {
                                                        value /= gain_value;
                                                }
                                        }
                                        for (size_t output_idx : channel_map[channel_idx]) {
                                                output_columns[output_idx][output_index] = value;
                                        }
                                }

                                if (store_row && !channel_map[channel_idx + 1].empty()) {
                                        double value = static_cast<double>(values.second);
                                        int baseline_value = adc_baseline[channel_idx + 1];
                                        if (baseline_value != NA_INTEGER) {
                                                value -= static_cast<double>(baseline_value);
                                        }
                                        if (physical) {
                                                double gain_value = adc_gain[channel_idx + 1];
                                                if (gain_value != NA_REAL && gain_value != 0.0) {
                                                        value /= gain_value;
                                                }
                                        }
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
                                double value = raw_value;
                                int baseline_value = adc_baseline[channel_idx];
                                if (baseline_value != NA_INTEGER) {
                                        value -= static_cast<double>(baseline_value);
                                }
                                if (physical) {
                                        double gain_value = adc_gain[channel_idx];
                                        if (gain_value != NA_REAL && gain_value != 0.0) {
                                                value /= gain_value;
                                        }
                                }
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
                           cpp11::doubles_matrix<> signal_matrix,
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
                           cpp11::list info_strings) {
        int number_of_channels = signal_matrix.ncol();
        if (samples <= 0) {
                samples = signal_matrix.nrow();
        }
        if (signal_matrix.nrow() != samples) {
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

        for (int i = 0; i < samples; ++i) {
                for (int channel_idx = 0; channel_idx < number_of_channels; ++channel_idx) {
                        double value = signal_matrix(i, channel_idx);
                        long long scaled = static_cast<long long>(std::llround(value));
                        int fmt = format_vec[channel_idx];
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

        header_stream << record_name << " " << number_of_channels << " " << frequency << " " << samples;
        if (!start_time.empty()) {
                header_stream << " " << start_time;
        }
        header_stream << "\n";

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

        writable::strings info_names = info_strings.names();
        for (int i = 0; i < info_strings.size(); ++i) {
                std::string key = info_names[i];
                cpp11::sexp value_sexp = info_strings[i];
                writable::strings values(value_sexp);
                header_stream << "# " << key;
                for (int j = 0; j < values.size(); ++j) {
                        std::string val = values[j];
                        header_stream << " " << val;
                }
                header_stream << "\n";
        }

        header_stream.close();
}
