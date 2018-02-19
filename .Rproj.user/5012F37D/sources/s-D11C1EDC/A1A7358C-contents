#include <Rcpp.h>
using namespace Rcpp;

//' Sample Covariance for Integer Vectors
//' 
//' Written in C++, this function should always run faster than 
//' \code{\link[stats]{cov}} for integer vectors. Will give incorrect result for 
//' non-integer vectors, and does not check that \code{x} and \code{y} are 
//' same length or that they contain no missing values.
//' 
//' @param x,y Integer vector.
//' 
//' @return Numeric value.
//' 
//' @examples
//' # For integer vectors, cov_i is typically much faster than cov.
//' x <- rpois(1000, lambda = 5)
//' y <- rpois(1000, lambda = 5)
//' all.equal(cov(x, y), cov_i(x, y))
//' benchmark(cov(x, y), cov_i(x, y), replications = 5000)
//' 
//' @export
// [[Rcpp::export]]
double cov_i(IntegerVector x, IntegerVector y) {
  double n = x.size();
  double nless1 = n - 1;
  double sumx = 0;
  double sumy = 0;
  double sumxy = 0;
  double xa = 0;
  double ya = 0;
  for (int a = 0; a < n; ++a) {
    xa = x[a];
    ya = y[a];
    sumx += xa;
    sumy += ya;
    sumxy += xa * ya;
  }
  double covxy = 1 / nless1 * (sumxy - sumx * sumy / n);
  return(covxy);
}
