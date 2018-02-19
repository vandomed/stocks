#include <Rcpp.h>
using namespace Rcpp;

//' Lagged Proportion Differences
//'
//' Calculates proportion differences between subsequent (or lagged) elements of 
//' a numeric vector.
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
//' # Generate 10 values from N(0, 1)
//' x <- rnorm(10)
//' 
//' # Calculate vector of proportion differences between subsequent values
//' y <- pdiffs(x)
//' 
//' # Equivalent base R computation
//' len <- length(x) 
//' p1 <- x[2: len]
//' p2 <- x[1: (len - 1)]
//' y2 <- (p1 - p2) / (0.5 * (p1 + p2))
//' all.equal(y, y2)
//' 
//'
//'@export
// [[Rcpp::export]]
NumericVector pdiffs(NumericVector x, int lag = 1) {
  int n = x.size();
  double current = 0;
  double previous = 0;
  NumericVector y(n - lag);
  if (lag == 1) {
    previous = x(0);
    for (int a = 1; a < n; ++a) {
      current = x(a);
      y(a - 1) = 2 * (current - previous) / (current + previous);
      previous = current;
    }
  }
  else {
    for (int a = lag; a < n; ++a) {
      current = x(a);
      previous = x(a - lag);
      y(a  -lag) = 2 * (current - previous) / (current + previous);
    }
    
  }
  return(y);
}