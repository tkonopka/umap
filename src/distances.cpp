
#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;


//' compute Euclidean distance between two vectors
//'
//' @param x numeric vector
//' @param y numeric vector
//'
//' @return euclidean norm of x-y
// [[Rcpp::export]]
double dEuclidean(NumericVector x, NumericVector y) {
  int xlen = x.size();
  double sumsquares = 0.0;
  for (int i=0; i<xlen; i++) {
    sumsquares += pow(x[i]-y[i], 2);
  }
  return sqrt(sumsquares);
}




//' compute Manhattan distance between two vectors
//'
//' @param x numeric vector
//' @param y numeric vector
//'
//' @return manhattan norm of x-y
// [[Rcpp::export]]
double dManhattan(NumericVector x, NumericVector y) {
  int xlen = x.size();
  double sumabs = 0.0;
  double diff = 0.0;
  for (int i=0; i<xlen; i++) {
    diff = x[i]-y[i];
    if (diff<0) {
      diff *= -1;
    }
    sumabs += diff;
  }
  return sumabs;
}




//' compute pearson correlation distance between two vectors
//'
//' Pearson distance is (1-r^2)
//'
//' Important: this function assumes that data has been centered
//' i.e. that mean(x) = mean(y) = 0
//'
//' @param x numeric vector
//' @param y numeric vector
//'
//' @return pearson distance between x and y
// [[Rcpp::export]]
double dCenteredPearson(NumericVector x, NumericVector y) {
  double xy = 0.0;
  double xx = 0.0;
  double yy = 0.0;
  int xlen = x.size();
  for (int i=0; i<xlen; i++) {
    xy += x[i]*y[i];
    xx += x[i]*x[i];
    yy += y[i]*y[i];
  }
  return 1-((xy*xy)/(xx*yy));
}




//' compute cosine dissimilarity between two vectors
//'
//' Note: values output from this function do not satisfy the triangle inequality
//'
//' @param x numeric vector
//' @param y numeric vector
//'
//' @return cosine dissimilarity between x and y
// [[Rcpp::export]]
double dCosine(NumericVector x, NumericVector y) {
  double xy = 0.0;
  double xx = 0.0;
  double yy = 0.0;
  int xlen = x.size();
  for (int i=0; i<xlen; i++) {
    xy += x[i]*y[i];
    xx += x[i]*x[i];
    yy += y[i]*y[i];
  }
  return 1-(xy/sqrt(xx*yy));
}

