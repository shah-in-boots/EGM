#include <cpp11.hpp>
#include <cmath>
using namespace cpp11;

[[cpp11::register]]
double calculate_approximate_entropy_cpp(cpp11::writable::doubles x, int m = 3, double r = -1.0) {
 	// Determine the length of the input time series.
 	int N = x.size();

 	// Check if the time series is long enough for the specified embedding dimension.
 	if (N < m + 1) {
 		stop("Time series is too short for the given embedding dimension.");
 	}

 	// If r is negative, compute the tolerance as 3.5 times the standard deviation of x.
 	if (r < 0) {
 		double sum = 0.0;
 		// Calculate the sum of all elements to compute the mean.
 		for (int i = 0; i < N; i++) {
 			sum += x[i];
 		}
 		double mean = sum / N; // Compute the mean of the time series.

 		double var = 0.0;
 		// Compute the variance by summing squared differences from the mean.
 		for (int i = 0; i < N; i++) {
 			double diff = x[i] - mean;
 			var += diff * diff;
 		}
 		var /= (N - 1); // Use sample variance (dividing by N-1).
 		// Calculate r as 3.5 times the standard deviation.
 		r = 3.5 * std::sqrt(var);
 	}

 	// Define a lambda function to compute the phi statistic for a given embedding dimension.
 	// The phi statistic is the average over all embedded vectors of the logarithm of the fraction of vectors
 	// that are within tolerance r.
 	auto compute_phi = [&](int dim) -> double {
 		// The number of embedded vectors that can be formed.
 		int num_vectors = N - dim + 1;
 		double sum_log = 0.0;  // This will accumulate the log values for each vector.

 		// Loop over each "template" vector in the embedded space.
 		for (int i = 0; i < num_vectors; i++) {
 			int count = 0;  // Initialize the count of vectors similar to the template vector.

 			// Compare the template vector starting at index i with all other embedded vectors.
 			for (int j = 0; j < num_vectors; j++) {
 				double max_diff = 0.0; // Variable to hold the Chebyshev distance between two vectors.

 				// For each element in the embedded vector, compute the absolute difference.
 				for (int k = 0; k < dim; k++) {
 					double diff = std::abs(x[i + k] - x[j + k]);
 					// Update the maximum difference observed.
 					if (diff > max_diff) {
 						max_diff = diff;
 					}
 					// If the current maximum difference exceeds the tolerance, exit the loop early.
 					if (max_diff > r) break;
 				}

 				// If the Chebyshev distance is within the tolerance, count this vector as similar.
 				if (max_diff <= r) {
 					count++;
 				}
 			}
 			// Normalize the count by dividing by the total number of vectors, then add the log of this ratio to the sum.
 			sum_log += std::log(static_cast<double>(count) / num_vectors);
 		}
 		// Return the average log value over all template vectors.
 		return sum_log / num_vectors;
 	};

 	// Compute the phi value for the original embedding dimension m.
 	double phi_m = compute_phi(m);
 	// Compute the phi value for the embedding dimension m+1.
 	double phi_m1 = compute_phi(m + 1);

 	// The approximate entropy (ApEn) is the difference between the two phi values.
 	double apen = phi_m - phi_m1;
 	return apen;
 }
