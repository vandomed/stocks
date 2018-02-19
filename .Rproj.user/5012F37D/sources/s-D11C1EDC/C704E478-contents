#include <Rcpp.h>
using namespace Rcpp;

//' True Range of Integer Values
//' 
//' Defined as the difference between the maximum and the minimum. Equivalent to 
//' base R code \code{diff(range(x))}, but much faster.
//' 
//' @param x Integer vector or matrix.
//' 
//' @return Integer value.
//' 
//' @examples 
//' # In general, true_range_i is much faster than diff(range(x))
//' x <- rpois(1000, lambda = 5)
//' all.equal(diff(range(x)), true_range_i(x))
//' benchmark(diff(range(x)), true_range_i(x), replications = 5000)
//' 
//' @export
// [[Rcpp::export]]
int true_range_i(IntegerVector x) {
  int n = x.size();
  int currentx = x[0];
  int minx = currentx;
  int maxx = currentx;
  for (int a = 1; a < n; ++a) {
    currentx = x[a];
    if (currentx < minx) minx = currentx;
    if (currentx > maxx) maxx = currentx;
  }
  int out = maxx - minx;
  return(out);
}