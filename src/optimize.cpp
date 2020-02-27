// package umap
// functions to optimize embedding

#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;



//' perform a compound transformation on a vector, including clipping
//'
//' @keywords internal
//' @noRd
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


//' run one epoch of the umap optimization
//'
//' @keywords internal
//' @noRd
//' @param embedding matrix with embedding coordinates
//' (This should be transpose of final embedding, with coordinates along
//' columns, and items in rows)
//' @param pairs matrix with two columns; rows should link to (from to);
//' indexes 0-based
//' @param adjust vector with 0/1 whether to adjust or not
//' @param nns vector with negative-neighbors-set size
//' @param abg vector with configuration parameters, a, b, gamma, move_other
//' @param alpha numeric learning rate for this epoch
//'
// [[Rcpp::export]]
NumericMatrix optimize_epoch (NumericMatrix &embedding,
			      IntegerMatrix &pairs,
			      IntegerVector &adjust,
			      IntegerVector &nns,
			      NumericVector &abg,
			      double alpha) {

  // precompute some constants from the configuration
  double a = abg[0];
  double b = abg[1];
  double gamma = abg[2];
  double bm1 = b-1;
  double m2ab = -2*a*b;
  double p2gb = 2*gamma*b;
  int V = embedding.ncol();
  bool move_other = !(abg[3]>0);
  
  int numpairs = pairs.nrow();
  for (int i=0; i<numpairs; i++) {
    if (adjust[i]>0) {
      
      // identify current vertex to process
      int j = pairs(i, 0);
      NumericMatrix::Column current = embedding(_, j);
      
      // identify primary link
      int k = pairs(i, 1);
      NumericMatrix::Column other = embedding(_, k);
      
      // adjust primary link
      NumericVector codiff = current-other;	    
      double codist2 = sum(codiff*codiff);
      double gradcoeff = (m2ab*pow(codist2, bm1)) / (a*pow(codist2, b)+1);
      NumericVector gradd = clip4(codiff, gradcoeff, alpha);
      current = current + gradd;
      if (move_other) {
	other = other - gradd;
      }
      
      // adjust a set of other randomly selected links
      int nnsi = nns[i];
      NumericVector krandom = runif(nnsi, 0, V);
      for (int kindex = 0; kindex<nnsi; kindex++) {
	k = floor(krandom[kindex]);
	NumericMatrix::Column other2 = embedding(_, k);
	codiff = current-other2;
	codist2 = sum(codiff*codiff);
	gradcoeff = p2gb / ((0.001+codist2)*(a*pow(codist2, b)+1));
	gradd = clip4(codiff, gradcoeff, alpha);
	current = current + gradd;
      }

    }
  }
  
  return embedding;
}
				  

//' run a series of epochs of the umap optimization
//'
//' @keywords internal
//' @noRd
//' @param embedding matrix with embedding coordinates 
//' (This should be transpose of final embedding, with coordinates along
//' columns, and items in rows)
//' @param pairs matrix with two columns; rows should linke to (from, to);
//' indexes 0-based
//' @param eps numeric vector, epochs for next sample
//' @param epns numeric vector, epochs for next negative sample
//' @param abg vector with configuration parameters, a, b, gamma, move_other
//' @param alpha0 numeric, initial learning rate 
//'
// [[Rcpp::export]]
NumericMatrix optimize_embedding (NumericMatrix &embedding,
				  IntegerMatrix &pairs,		      
				  NumericVector &eps,
				  NumericVector &epns,
				  NumericVector &abg,
				  double alpha0,
				  int num_epochs) {

  int N = eps.size();
  
  // next negative sample (initially all zero)
  IntegerVector nns(N);
  // epochs of next sample (initially equal to eps)
  NumericVector eons(N);
  // epochs of next negative sample (initially equal to epns)
  NumericVector eon2s(N);
  for (int i=0; i<N; i++) {
    eons[i] = eps[i];
    eon2s[i] = epns[i];
  }
  IntegerVector adjust(N);
  double alpha = 0.0;
  
  for (int n = 0; n < num_epochs; n++) {
    alpha = alpha0 * (1 - (double(n)/double(num_epochs)));
    int np1 = n + 1;
    for (int i=0; i<N; i++) {
      adjust[i] = (eons[i] <= np1);
      if (adjust[i]) {
    	nns[i] = floor((1.0+double(n)-eon2s[i])/epns[i]);
      }
    }
    embedding = optimize_epoch(embedding, pairs, adjust, nns, abg, alpha);
    for (int i=0; i<N; i++) {
      if (adjust[i]) {
    	eons[i] += eps[i];
    	eon2s[i] += nns[i]*epns[i];
      }
    }
  }
  
  return embedding;
}

