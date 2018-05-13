
#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;


//' compute Euclidean distance between two vectors
//'
//' @param x numeric vector
//' @param y numeric vector
//'
//' @return euclidean norm of x-y
//'
// [[Rcpp::export]]
double dEuclidean(NumericVector x, NumericVector y) {
  int xlen = x.size();
  double sumsquares = 0.0;
  for (int i=0; i<xlen; i++) {
    sumsquares += pow(x[i]-y[i], 2);
  }
  return sqrt(sumsquares);
}


