#include <cpp11.hpp>
#include <cmath>
using namespace cpp11;

// Sample entropy (Richman & Moorman 2000).
//
// SampEn = -log(A / B), where B counts template-vector pairs of length m whose
// Chebyshev distance is within r, and A counts the same pairs extended to
// length m + 1. Unlike approximate entropy, self-matches are excluded, which
// removes the bias toward regularity and the dependence on record length that
// ApEn carries.
//
// Both counts run over the same N - m templates so that A and B are directly
// comparable. The m and m + 1 distances are accumulated in one pass, since the
// longer distance is just the shorter one maximised against the next sample.
[[cpp11::register]]
double calculate_sample_entropy_cpp(cpp11::writable::doubles x, int m = 2, double r = -1.0) {
	int N = x.size();

	if (m < 1) {
		stop("Embedding dimension `m` must be at least 1.");
	}
	if (N < m + 2) {
		stop("Time series is too short for the given embedding dimension.");
	}

	// A negative `r` is the flag for "compute the conventional tolerance", which
	// is 0.2 times the standard deviation of the series.
	if (r < 0) {
		double sum = 0.0;
		for (int i = 0; i < N; i++) {
			sum += x[i];
		}
		double mean = sum / N;

		double var = 0.0;
		for (int i = 0; i < N; i++) {
			double diff = x[i] - mean;
			var += diff * diff;
		}
		var /= (N - 1);
		r = 0.2 * std::sqrt(var);
	}

	// Number of templates. Using N - m for both dimensions keeps A and B over
	// the same index set, which is what makes the ratio well defined.
	int num_vectors = N - m;

	// Counts can exceed the range of int on long records, so accumulate wide.
	double B = 0.0;  // pairs matching at length m
	double A = 0.0;  // pairs matching at length m + 1

	// Only j > i is walked; each unordered pair is counted once and the ratio
	// A / B is unaffected by the constant factor of two.
	for (int i = 0; i < num_vectors; i++) {
		for (int j = i + 1; j < num_vectors; j++) {
			double max_diff = 0.0;

			for (int k = 0; k < m; k++) {
				double diff = std::abs(x[i + k] - x[j + k]);
				if (diff > max_diff) {
					max_diff = diff;
				}
				if (max_diff > r) break;
			}

			if (max_diff <= r) {
				B += 1.0;

				// Extending to m + 1 only needs the one additional sample.
				double diff = std::abs(x[i + m] - x[j + m]);
				if (diff > max_diff) {
					max_diff = diff;
				}
				if (max_diff <= r) {
					A += 1.0;
				}
			}
		}
	}

	// With no matches at either length the statistic is undefined rather than
	// infinite; the caller is told so instead of being handed a number.
	if (B == 0.0 || A == 0.0) {
		return NA_REAL;
	}

	return -std::log(A / B);
}
