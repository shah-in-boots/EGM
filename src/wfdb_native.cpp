#include "cpp11.hpp"
#include <cstdint>
#include <fstream>
#include <string>
#include <vector>

using cpp11::writable::doubles_matrix;
using cpp11::stop;

// The cpp11 bridge returns a dense matrix of WFDB signal samples. The caller
// passes the path to the binary signal file along with metadata describing how
// many channels are interleaved in the file and which sample range should be
// read. Only 16-bit interleaved records are currently supported and are
// returned as doubles to match R's native numeric type.
[[cpp11::register]]
doubles_matrix<> read_wfdb_dat_cpp(const std::string& path,
                                   int n_channels,
                                   int start_sample,
                                   int n_samples,
                                   int bytes_per_sample) {
  if (n_channels <= 0) {
    stop("`n_channels` must be positive");
  }
  if (start_sample < 0) {
    stop("`start_sample` must be non-negative");
  }
  if (n_samples <= 0) {
    stop("`n_samples` must be positive");
  }
  if (bytes_per_sample != 2) {
    stop("Only 16-bit WFDB records are currently supported");
  }

  // Open the WFDB signal file in binary mode so we can stream the bytes
  // without any platform-specific newline conversions.
  std::ifstream input(path, std::ios::binary);
  if (!input.is_open()) {
    stop("Unable to open WFDB signal file '%s'", path.c_str());
  }

  // Seek to the requested starting sample. WFDB signal files interleave the
  // channel samples, so we compute an absolute byte offset by multiplying the
  // sample index by the number of channels and the number of bytes per sample.
  const std::size_t offset = static_cast<std::size_t>(start_sample) *
                             static_cast<std::size_t>(n_channels) *
                             static_cast<std::size_t>(bytes_per_sample);
  input.seekg(static_cast<std::streamoff>(offset), std::ios::beg);
  if (!input.good()) {
    stop("Failed to seek to requested offset in '%s'", path.c_str());
  }

  // Read the requested block of samples into a temporary buffer. Each value is
  // stored as a signed 16-bit integer in the WFDB format we currently support.
  const std::size_t total_values = static_cast<std::size_t>(n_samples) *
                                   static_cast<std::size_t>(n_channels);
  std::vector<int16_t> buffer(total_values);
  input.read(reinterpret_cast<char*>(buffer.data()),
             static_cast<std::streamsize>(total_values * bytes_per_sample));
  if (input.gcount() < static_cast<std::streamsize>(total_values * bytes_per_sample)) {
    stop("WFDB signal file '%s' ended before the requested number of samples could be read", path.c_str());
  }

  // Copy the raw values into the output matrix. We return doubles so that R can
  // work with the data without additional type coercion; callers cast back to
  // integers when they request digital units.
  doubles_matrix<> result(n_samples, n_channels);
  for (int i = 0; i < n_samples; ++i) {
    for (int j = 0; j < n_channels; ++j) {
      const std::size_t idx = static_cast<std::size_t>(i) *
                              static_cast<std::size_t>(n_channels) +
                              static_cast<std::size_t>(j);
      result(i, j) = static_cast<double>(buffer[idx]);
    }
  }

  return result;
}
