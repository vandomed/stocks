#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double mdd_p(NumericVector x) {
  int n = x.size();
  double maximum = x(0);
  double mdd = 1;
  double dd = 0;
  double current = 0;
  for (int a = 1; a < n; ++a) {
    current = x(a);
    dd = current / maximum;
    if (dd < mdd) mdd = dd;
    if (current > maximum) maximum = current;
  }
  mdd = 1 - mdd;
  return(mdd);
}
// [[Rcpp::export]]
NumericVector mdd_p_indices(NumericVector x) {
  int n = x.size();
  int pos1 = 0;
  int pos2 = 0;
  int whichmax = 0;
  double maximum = x(0);
  double mdd = 1;
  double dd = 0;
  double current = 0;
  for (int a = 1; a < n; ++a) {
    current = x(a);
    dd = current / maximum;
    if (dd < mdd) {
      mdd = dd;
      pos1 = whichmax;
      pos2 = a;
    }
    if (current > maximum) {
      whichmax = a;
      maximum = current;
    }
  }
  mdd = 1 - mdd;
  NumericVector out(3);
  out(0) = mdd;
  out(1) = pos1 + 1;
  out(2) = pos2 + 1;
  return(out);
}
// [[Rcpp::export]]
double mdd_hl(NumericVector highs, NumericVector lows) {
  int n = highs.size();
  double maximum = highs(0);
  double mdd = 1;
  double dd = 0;
  double currenthigh;
  double currentlow;
  for (int a = 1; a < n; ++a) {
    currenthigh = highs(a);
    currentlow = lows(a);
    dd = currentlow / maximum;
    if (dd < mdd) mdd = dd;
    if (currenthigh > maximum) maximum = currenthigh;
  }
  mdd = 1 - mdd;
  return(mdd);
}
// [[Rcpp::export]]
NumericVector mdd_hl_indices(NumericVector highs, NumericVector lows) {
  int n = highs.size();
  int pos1 = 0;
  int pos2 = 0;
  int whichmax = 0;
  double maximum = highs(0);
  double mdd = 1;
  double dd = 0;
  double currenthigh = 0;
  double currentlow = 0;
  for (int a = 1; a < n; ++a) {
    currenthigh = highs(a);
    currentlow = lows(a);
    dd = currentlow / maximum;
    if (dd < mdd) {
      mdd = dd;
      pos1 = whichmax;
      pos2 = a;
    }
    if (currenthigh > maximum) {
      whichmax = a;
      maximum = currenthigh;
    }
  }
  mdd = 1 - mdd;
  NumericVector out(3);
  out(0) = mdd;
  out(1) = pos1 + 1;
  out(2) = pos2 + 1;
  return(out);
}