#include <Rcpp.h>
#include <random>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace Rcpp;


// ------------------------------------------------------------
// Circular difference
// ------------------------------------------------------------

inline double circ_diff(double d)
{
  const double half = 365.25 / 2.0;
  
  if (d > half)
    d -= 365.25;
  
  else if (d < -half)
    d += 365.25;
  
  return d;
}


// ------------------------------------------------------------
// Calculate circular Theil-Sen slope
// Skips NA DOY values
// ------------------------------------------------------------

inline double sen_slope_internal(
    const double* doy,
    const double* year,
    int n,
    std::vector<double>& slopes
)
{
  int k = 0;
  
  for (int i = 0; i < n - 1; ++i)
  {
    // Skip missing DOY
    if (NumericVector::is_na(doy[i]))
      continue;
    
    for (int j = i + 1; j < n; ++j)
    {
      // Skip missing DOY
      if (NumericVector::is_na(doy[j]))
        continue;
      
      double dy = year[j] - year[i];
      
      if (dy == 0)
        continue;
      
      double dd = circ_diff(
        doy[j] - doy[i]
      );
      
      slopes[k++] = dd / dy;
    }
  }
  
  if (k == 0)
    return NA_REAL;
  
  int mid = k / 2;
  
  std::nth_element(
    slopes.begin(),
    slopes.begin() + mid,
    slopes.begin() + k
  );
  
  if (k % 2 == 1)
    return slopes[mid];
  
  double upper = slopes[mid];
  
  std::nth_element(
    slopes.begin(),
    slopes.begin() + mid - 1,
    slopes.begin() + mid
  );
  
  double lower = slopes[mid - 1];
  
  return 0.5 * (lower + upper);
}


// ------------------------------------------------------------
// Monte Carlo test
//
// [[Rcpp::export]]
// ------------------------------------------------------------

NumericVector mc_circular_sen(
    NumericVector doy,
    NumericVector year,
    int B = 10000,
    int min_perm = 500,
    int early_stop = 2000,
    double early_p = 0.20,
    int seed = 1
)
{
  const int n = doy.size();
  
  NumericVector out(4);
  
  // --------------------------------------------------------
  // Need at least 3 non-NA observations
  // --------------------------------------------------------
  
  int n_valid = 0;
  
  for (int i = 0; i < n; ++i)
  {
    if (!NumericVector::is_na(doy[i]))
      ++n_valid;
  }
  
  if (n_valid < 3)
  {
    out[0] = NA_REAL;
    out[1] = 0;
    out[2] = 0;
    out[3] = NA_REAL;
    
    return out;
  }
  
  
  // --------------------------------------------------------
  // Number of possible pairwise slopes
  //
  // Maximum is 27 choose 2 = 351
  // --------------------------------------------------------
  
  const int npair = n * (n - 1) / 2;
  
  std::vector<double> slopes(npair);
  
  
  // --------------------------------------------------------
  // Observed slope
  // --------------------------------------------------------
  
  double observed =
    sen_slope_internal(
      doy.begin(),
      year.begin(),
      n,
      slopes
    );
  
  if (!R_finite(observed))
  {
    out[0] = NA_REAL;
    out[1] = 0;
    out[2] = 0;
    out[3] = NA_REAL;
    
    return out;
  }
  
  
  const double abs_observed =
    std::abs(observed);
  
  
  // --------------------------------------------------------
  // Permuted DOY
  //
  // Only permute the non-NA values.
  // --------------------------------------------------------
  
  std::vector<double> valid_doy;
  
  valid_doy.reserve(n_valid);
  
  for (int i = 0; i < n; ++i)
  {
    if (!NumericVector::is_na(doy[i]))
      valid_doy.push_back(doy[i]);
  }
  
  
  std::vector<double> permuted_doy(
      n,
      NA_REAL
  );
  
  
  // --------------------------------------------------------
  // Random number generator
  // --------------------------------------------------------
  
  std::mt19937 rng(seed);
  
  int exceed = 0;
  int performed = 0;
  
  
  // --------------------------------------------------------
  // Monte Carlo loop
  // --------------------------------------------------------
  
  for (int b = 0; b < B; ++b)
  {
    // Start with all NA
    std::fill(
      permuted_doy.begin(),
      permuted_doy.end(),
      NA_REAL
    );
    
    // Shuffle the valid observations
    std::shuffle(
      valid_doy.begin(),
      valid_doy.end(),
      rng
    );
    
    // Put shuffled values into the positions
    // that originally contained observations
    int k = 0;
    
    for (int i = 0; i < n; ++i)
    {
      if (!NumericVector::is_na(doy[i]))
      {
        permuted_doy[i] = valid_doy[k];
        ++k;
      }
    }
    
    
    // ----------------------------------------------------
    // Calculate permutation slope
    // ----------------------------------------------------
    
    double s =
      sen_slope_internal(
        permuted_doy.data(),
        year.begin(),
        n,
        slopes
      );
    
    
    if (R_finite(s) &&
        std::abs(s) >= abs_observed)
    {
      ++exceed;
    }
    
    ++performed;
    
    
    // ----------------------------------------------------
    // Early stopping
    // ----------------------------------------------------
    
    if (performed >= min_perm)
    {
      double p =
        (1.0 + exceed) /
          (performed + 1.0);
      
      if (performed >= early_stop &&
          p > early_p)
      {
        break;
      }
    }
  }
  
  
  // --------------------------------------------------------
  // Final Monte Carlo p-value
  // --------------------------------------------------------
  
  double p =
    (1.0 + exceed) /
      (performed + 1.0);
  
  
  out[0] = observed;
  out[1] = exceed;
  out[2] = performed;
  out[3] = p;
  
  return out;
}