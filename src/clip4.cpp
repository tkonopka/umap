
#include <Rcpp.h>
using namespace Rcpp;


//' perform a compound transformation on a vector, including clipping
//'
//' @keywords internal
//' @param x numeric vector
//' @param inner numeric constant
//' @param outer numeric constan
//'
//' @return numeric vector of same length as x, containing outer*clip4(inner*x)
// [[Rcpp::export]]
NumericVector clip4(NumericVector x, double inner, double outer) {
  int xlen = x.size();
  for (int i=0; i<xlen; i++) {
    x[i] *= inner;
    if (x[i]>4) {
      x[i] = 4;
    } else if (x[i]<-4) {
      x[i] = -4;
    }
    x[i] *= outer;
  }
  return x;
}

