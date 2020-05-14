#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector moving_mean_i(IntegerVector x, double window) {
  
  // Get length(x), calculate 1 / window, and initialize output vector
  int n = x.size();
  double inverse = 1 / window;
  NumericVector out(n - window + 1);
  
  // First moving average
  double sum = 0;
  for (int a = 0; a < window; ++a) {
    sum += x[a];
  }
  out[0] = sum * inverse;
  
  // All other moving averages
  int index = 0;
  for (int a = window; a < n; ++a) {
    sum = sum + x[a] - x[a - window];
    index += 1;
    out[index] = sum * inverse;
  }
  return out;
  
}

// [[Rcpp::export]]
double moving_mean_i_max(IntegerVector x, double window) {
  
  // Get length(x)
  int n = x.size();
  
  // First moving sum
  NumericVector current(window);
  double sum = 0;
  for (int a = 0; a < window; ++a) {
    current[a] = x[a];
    sum += x[a];
  }
  
  // Loop through and find max moving sum
  double max = sum;
  for (int a = window; a < n; ++a) {
    sum = sum + x[a] - x[a - window];
    if (sum > max) max = sum;
  }
  
  // Return max moving average
  return max / window;
  
}
  
// [[Rcpp::export]]
NumericVector moving_mean_n(NumericVector x, double window) {
  
  // Get length(x), calculate 1 / window, and initialize output vector
  int n = x.size();
  double inverse = 1 / window;
  NumericVector out(n - window + 1);
  
  // First moving average
  double sum = 0;
  for (int a = 0; a < window; ++a) {
    sum += x[a];
  }
  out[0] = sum * inverse;
  
  // All other moving averages
  int index = 0;
  for (int a = window; a < n; ++a) {
    sum = sum + x[a] - x[a - window];
    index += 1;
    out[index] = sum * inverse;
  }
  return out;
  
}

// [[Rcpp::export]]
double moving_mean_n_max(NumericVector x, double window) {
  
  // Get length(x)
  int n = x.size();
  
  // First moving sum
  NumericVector current(window);
  double sum = 0;
  for (int a = 0; a < window; ++a) {
    current[a] = x[a];
    sum += x[a];
  }
  
  // Loop through and find max moving sum
  double max = sum;
  for (int a = window; a < n; ++a) {
    sum = sum + x[a] - x[a - window];
    if (sum > max) max = sum;
  }
  
  // Return max moving average
  return max / window;
  
}