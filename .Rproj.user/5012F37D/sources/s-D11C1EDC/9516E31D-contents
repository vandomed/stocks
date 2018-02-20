#include <Rcpp.h>
using namespace Rcpp;

//' Ratios of Subsequent Elements in a Vector
//' 
//' Calculates vector of ratios of a vector, i.e. ratio of \code{x[2]} to 
//' \code{x[1]}, ratio of \code{x[3]} to \code{x[2]}, and so forth.
//' 
//'
//' @param x Numeric vector.
//'
//'
//' @return Numeric vector.
//'
//'
//' @examples
//' # Generate 10 values from N(0, 1)
//' x <- rnorm(10)
//' 
//' # Calculate vector of ratios
//' (y <- ratios(x))
//' 
//' # Slower base R computation
//' len <- length(x)
//' y2 <- x[2: len] / x[1: (len - 1)]
//' all.equal(y, y2)
//' 
//'
//'@export
// [[Rcpp::export]]
NumericVector ratios(NumericVector x) {
  int n = x.size();
  NumericVector y(n - 1);
  double current = 0;
  double previous = x(0);
  for (int a = 1; a < n; ++a) {
    current = x(a);
    y(a - 1) = current / previous;
    previous = current;
  }
  return(y);
}