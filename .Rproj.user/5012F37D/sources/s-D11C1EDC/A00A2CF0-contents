#include <Rcpp.h>
using namespace Rcpp;

//' Weighted Arithmetic Mean for Integer Values and Integer Weights
//' 
//' Written in C++, this function should always run faster than 
//' \code{\link{weighted.mean}}.
//' 
//' For optimal speed, choose the version of this function that matches the 
//' class of your \code{x} and \code{w}: 
//' \code{\link{weighted_mean_nn}} for numeric \code{x}, numeric \code{w} \cr 
//' \code{\link{weighted_mean_ni}} for numeric \code{x}, integer \code{w} \cr 
//' \code{\link{weighted_mean_in}} for integer \code{x}, numeric \code{w} \cr 
//' \code{\link{weighted_mean_ii}} for integer \code{x}, integer \code{w} \cr
//' 
//' These functions typically execute several times faster than the base R 
//' function \code{\link[stats]{weighted.mean}} and weighted average functions 
//' in other packages (e.g. \code{wtd.mean} in \pkg{Hmisc} and \code{wt.mean} in 
//' \pkg{SDMTools}).
//' 
//' @param x Integer vector of values.
//' @param w Integer vector of weights.
//' 
//' @return Numeric value.
//' 
//' @examples 
//' # weighted_mean_ii is typically much faster than weighted.mean
//' x <- rpois(1000, lambda = 5)
//' w <- rpois(1000, lambda = 5)
//' all.equal(weighted.mean(x, w), weighted_mean_ii(x, w))
//' benchmark(weighted.mean(x, w), weighted_mean_ii(x, w), replications = 2000)
//' 
//' @export
// [[Rcpp::export]]
double weighted_mean_ii(IntegerVector x, IntegerVector w) {
  int n = x.size();
  double sumx = 0;
  double sumw = 0;
  double weight = 0;
  for (int a = 0; a < n; ++a) {
    weight = w[a];
    sumx += x[a] * weight;
    sumw += weight;
  }
  double meanx = sumx / sumw;
  return(meanx);
}