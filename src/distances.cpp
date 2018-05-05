
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



//' compute squared Euclidean distance between two vectors
//'
//' @param x numeric vector
//' @param y numeric vector
//'
//' @return square of euclidean distance 
//'
// [[Rcpp::export]]
double dEuc2(NumericVector x, NumericVector y) {
  // for low dimensions, hardcode the formulae
  int xlen = x.size();
  if (xlen==2) {
    return pow(x[0]-y[0], 2) + pow(x[1]-y[1], 2);
  }
  if (xlen==3) {
    return pow(x[0]-y[0], 2) + pow(x[1]-y[1], 2) + pow(x[2]-y[2], 2);
  }
  // handle generic case too
  double sumsquares = 0.0;
  for (int i=0; i<xlen; i++) {
    double diff = x[i]-y[i];
    sumsquares += diff*diff;
  }
  return sumsquares;
}



