#include <Rcpp.h>
#include <algorithm>
#include <cmath>

#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

// [[Rcpp::plugins(openmp)]]

// [[Rcpp::export]]
NumericMatrix sen_mk_parallel(
    NumericVector prec,
    int n_groups,
    int n_threads = 1
) {
  
  const int NY = 27;
  const int MAX_PAIRS = 351;
  
  NumericMatrix out(n_groups, 2);
  
  // Column 1 = Sen slope
  // Column 2 = MK p-value
  
#ifdef _OPENMP
  omp_set_num_threads(n_threads);
#endif
  
#pragma omp parallel for schedule(static)
  for (int g = 0; g < n_groups; ++g) {
    
    int start = g * NY;
    
    // -------------------------------------------------
    // Extract non-missing observations
    // -------------------------------------------------
    
    double y[NY];
    int yr[NY];
    int n = 0;
    
    for (int i = 0; i < NY; ++i) {
      
      double v = prec[start + i];
      
      if (!Rcpp::NumericVector::is_na(v)) {
        
        y[n] = v;
        
        // Since the years are 1998,...,2024,
        // i is sufficient to represent the year.
        yr[n] = i;
        
        ++n;
      }
    }
    
    // Fewer than 2 observations -> NA
    if (n < 2) {
      out(g, 0) = NA_REAL;
      out(g, 1) = NA_REAL;
      continue;
    }
    
    
    // =================================================
    // THEIL-SEN SLOPE
    // =================================================
    
    double slopes[MAX_PAIRS];
    int ns = 0;
    
    for (int i = 0; i < n - 1; ++i) {
      
      for (int j = i + 1; j < n; ++j) {
        
        slopes[ns++] =
          (y[j] - y[i]) /
            (double)(yr[j] - yr[i]);
      }
    }
    
    // Median
    int mid = ns / 2;
    
    std::nth_element(
      slopes,
      slopes + mid,
      slopes + ns
    );
    
    double sen = slopes[mid];
    
    // Even number of slopes
    if ((ns % 2) == 0) {
      
      double lower = *std::max_element(
        slopes,
        slopes + mid
      );
      
      sen = 0.5 * (lower + sen);
    }
    
    
    // =================================================
    // MANN-KENDALL S
    // =================================================
    
    long long S = 0;
    
    for (int i = 0; i < n - 1; ++i) {
      
      for (int j = i + 1; j < n; ++j) {
        
        if (y[j] > y[i]) {
          ++S;
        }
        else if (y[j] < y[i]) {
          --S;
        }
      }
    }
    
    
    // =================================================
    // TIE CORRECTION
    // =================================================
    
    double sorted_y[NY];
    
    for (int i = 0; i < n; ++i) {
      sorted_y[i] = y[i];
    }
    
    std::sort(
      sorted_y,
      sorted_y + n
    );
    
    long long tie_sum = 0;
    
    int i = 0;
    
    while (i < n) {
      
      int j = i + 1;
      
      while (
          j < n &&
            sorted_y[j] == sorted_y[i]
      ) {
        ++j;
      }
      
      int t = j - i;
      
      if (t > 1) {
        
        tie_sum +=
          (long long)t *
          (t - 1) *
          (2 * t + 5);
      }
      
      i = j;
    }
    
    
    // =================================================
    // VAR(S)
    // =================================================
    
    double nn = (double)n;
    
    double varS =
      (
          nn * (nn - 1.0) * (2.0 * nn + 5.0)
      - (double)tie_sum
      ) / 18.0;
    
    
    // =================================================
    // MK Z
    // =================================================
    
    double z;
    
    if (S > 0) {
      
      z = ((double)S - 1.0) /
        std::sqrt(varS);
      
    } else if (S < 0) {
      
      z = ((double)S + 1.0) /
        std::sqrt(varS);
      
    } else {
      
      z = 0.0;
    }
    
    
    // =================================================
    // TWO-SIDED P-VALUE
    // =================================================
    
    double p =
      std::erfc(
        std::fabs(z) / std::sqrt(2.0)
      );
    
    
    // =================================================
    // OUTPUT
    // =================================================
    
    out(g, 0) = sen;
    out(g, 1) = p;
  }
  
  return out;
}