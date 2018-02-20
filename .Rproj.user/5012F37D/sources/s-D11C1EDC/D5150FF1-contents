#include <Rcpp.h>
using namespace Rcpp;

//' Lagged Differences (Alternate Implementation)
//'
//' Calculates differences between subsequent (or lagged) elements of a vector. 
//' Very similar to \code{\link[base]{diff}}, but written in C++.
//'
//' @param x Numeric vector.
//' @param lag Numeric value (e.g. 2 for differences between 1st and 3rd
//' element, 2nd and 4th, ...).
//'
//'
//' @return Numeric vector.
//'
//'
//' @examples
//' # Generate 1 million values from Poisson(3) distribution
//' x <- rpois(100000, 3)
//'
//' # Calculate vector of differences between subsequent values
//' y <- diffs(x)
//'
//' # Could get same result from base R function diff
//' z <- diff(x)
//' all.equal(y, z)
//'
//' # But diffs is faster
//' benchmark(diffs(x), diff(x), replications = 100)
//'
//'
//'@export
// [[Rcpp::export]]
NumericVector diffs(NumericVector x, int lag = 1) {
  int n = x.size();
  NumericVector y(n - lag);
  if (lag == 1) {
    double current = 0;
    double previous = x(0);
    for (int a = 1; a < n; ++a) {
      current = x(a);
      y(a - 1) = current - previous;
      previous = current;
    }
  }
  else for (int a = lag; a < n; ++a) y(a - lag) = x(a) - x(a - lag);
  return(y);
}
