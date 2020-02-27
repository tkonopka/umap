// package umap
// functions to compute distances
// it would be great to simplify the "matrix" distance functions into a
// single function 

#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;


//' compute Euclidean distance between two vectors
//'
//' @keywords internal
//' @noRd
//' @param x numeric vector
//' @param y numeric vector
//'
//' @return euclidean norm of x-y
// [[Rcpp::export]]
double dEuclidean(const NumericVector &x, const NumericVector &y) {
  int xlen = x.size();
  double sumsquares = 0.0;
  for (int i=0; i<xlen; i++) {
    sumsquares += pow(x[i]-y[i], 2);
  }
  return sqrt(sumsquares);
}


//' compute Euclidean distances
//'
//' @keywords internal
//' @noRd
//' @param m matrix with raw data
//' @param origin index (1-based) of origin element
//' @param targets indexes (1-based) of target elements
//'
//' @return dEuclidean norms between origin and target elements
// [[Rcpp::export]]
NumericVector mdEuclidean(const NumericMatrix &m, int origin, const NumericVector &targets) {
  int numtargets = targets.size();
  NumericVector result(numtargets);
  NumericVector odata = m(_, origin-1);
  for (int i=0; i<numtargets; i++) {
    result[i] = dEuclidean(odata, m(_, targets[i]-1));
  }
  return result;
}


//' compute Manhattan distance between two vectors
//'
//' @keywords internal
//' @noRd
//' @param x numeric vector
//' @param y numeric vector
//'
//' @return manhattan norm of x-y
// [[Rcpp::export]]
double dManhattan(const NumericVector &x, const NumericVector &y) {
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


//' compute Manhattan distances
//'
//' @keywords internal
//' @noRd
//' @param m matrix with raw data
//' @param origin index (1-based) of origin element
//' @param targets indexes (1-based) of target elements
//'
//' @return dManhattan norms between origin and targets
// [[Rcpp::export]]
NumericVector mdManhattan(const NumericMatrix &m, int origin, const NumericVector &targets) {
  int numtargets = targets.size();
  NumericVector result(numtargets);
  NumericVector odata = m(_, origin-1);
  for (int i=0; i<numtargets; i++) {
    result[i] = dManhattan(odata, m(_, targets[i]-1));
  }
  return result;
}


//' compute pearson correlation distance between two vectors
//'
//' Pearson distance is (1-r^2)
//'
//' Important: this function assumes that data has been centered
//' i.e. that mean(x) = mean(y) = 0
//'
//' @keywords internal
//' @noRd
//' @param x numeric vector
//' @param y numeric vector
//'
//' @return pearson distance between x and y
// [[Rcpp::export]]
double dCenteredPearson(const NumericVector &x, const NumericVector &y) {
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


//' compute pearson correlation distances 
//'
//' @keywords internal
//' @noRd
//' @param m matrix with raw data
//' @param origin index (1-based) of origin element
//' @param targets indexes (1-based) of target elements
//'
//' @return dCenteredPearson norms between origin and targets
// [[Rcpp::export]]
NumericVector mdCenteredPearson(const NumericMatrix &m, int origin, const NumericVector &targets) {
  int numtargets = targets.size();
  NumericVector result(numtargets);
  NumericVector odata = m(_, origin-1);
  for (int i=0; i<numtargets; i++) {
    result[i] = dCenteredPearson(odata, m(_, targets[i]-1));
  }
  return result;
}


//' compute cosine dissimilarity between two vectors
//'
//' Note: values output from this function do not satisfy the
//' triangle inequality
//'
//' @keywords internal
//' @noRd
//' @param x numeric vector
//' @param y numeric vector
//'
//' @return cosine dissimilarity between x and y
// [[Rcpp::export]]
double dCosine(const NumericVector &x, const NumericVector &y) {
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


//' compute cosine distances
//'
//' @keywords internal
//' @noRd
//' @param m matrix with raw data
//' @param origin index (1-based) of origin element
//' @param targets indexes (1-based) of target elements
//'
//' @return dCosine norms between origin and targets
// [[Rcpp::export]]
NumericVector mdCosine(const NumericMatrix &m, int origin, const NumericVector &targets) {
  int numtargets = targets.size();
  NumericVector result(numtargets);
  NumericVector odata = m(_, origin-1);
  for (int i=0; i<numtargets; i++) {
    result[i] = dCosine(odata, m(_, targets[i]-1));
  }
  return result;
}

