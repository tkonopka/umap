## package umap
## functions of generic nature used in umap_naive 




##' Estimate a/b parameters
##'
##' This performs a brute-force search in parameter space.
##' The algorithm assumes a narrowing-down search will produce a decent fit.
##'
##' @keywords internal
##' @param spread numeric
##' @param min.dist numeric
##' @param alim numeric vector of length 2, initial search range for parameter a
##' @param blim numeric vector of length 2, initial search range for parameter b
##' @param tolerance numeric, determines how deeply to search 
##'
##' @return vector with componets "a" and "b" that are most appropriate
##' for the given spread and min.dist.
find.ab.params = function(spread, min.dist,
                          alim=c(0, 20), blim=c(0, 20),
                          tolerance=1e-8) {
  
  ## compute square error between two vectors
  sum.err.2 = function(x, y) {
    xy = (x-y)
    sum(xy*xy)
  }

  ## compute y values given parameters a, b
  abcurve = function(x, a, b) {
    xb2 = x^(2*b)
    1.0/(1+(a*xb2))
  }

  ## create x values and target y values
  xv = seq(0, spread*3, length=300)
  xv.high = xv[xv>=min.dist]
  yv = rep(0, length(xv))
  yv[xv<min.dist] = 1
  yv[xv>min.dist] = exp((min.dist-xv.high)/spread)

  ## internal recursive helper. Tries different combinations of a/b.
  find.ab.recursive = function(alim, blim) {
    avals = seq(alim[1], alim[2], length=10)
    bvals = seq(blim[1], blim[2], length=10)
    ## compute square error of curve for all combinations of a/b
    ab = expand.grid(avals, bvals) 
    errors = apply(ab, 1, function(x) {
      yvals = abcurve(xv, x[1], x[2])
      sum.err.2(yvals, yv)
    })
    ## identify combination with smallest error
    best = as.numeric(ab[which.min(errors)[1],])
    ## determine if exit or keep looking in narrower interval
    mid = c(mean(alim), mean(blim))
    if (sum(abs(best-mid))>tolerance) {
      alim = best[1] + (avals[2]-avals[1])*c(-1.5, 1.5)
      blim = best[2] + (bvals[2]-bvals[1])*c(-1.5, 1.5)
      best = find.ab.recursive(alim, blim)
    }
    best
  }
  
  result = find.ab.recursive(alim, blim)
  names(result) = c("a", "b")
  result  
}



##' Compute a value to capture how often each item contributes to layout optimization
##'
##' @keywords internal
##' @param w numeric vector or matrix
##' @param epochs integer
##'
##' @return numeric vector of same length as w
make.epochs.per.sample = function(w, epochs) {
  result = w
  result[1:length(w)] = rep(-1, length(w))
  n.samples = epochs*(w/max(w))
  n.positive = n.samples>0
  result[n.positive] = epochs / n.samples[n.positive]
  result
}




##' Force (clip) a value into a finite range
##'
##' This implementation exists mainly to
##' facilitate testing the Rcpp version.
##'
##' @keywords internal
##' @param x numeric; single value or a vector
##' @param xmax maximum value for x
##'
##' @return numeric values in range [-xmax, xmax]
clip = function(x, xmax=4) {
  x[x>xmax] = xmax
  x[x<(-xmax)] = -xmax
  x
}



##' Adjust a matrix so that each column is centered around zero
##'
##' @keywords internal
##' @param x matrix
##'
##' @return matrix of same shape as x
center.embedding = function(x) {
  colcenters = colMeans(x)
  V = nrow(x)
  x - matrix(rep(colcenters, each=V), ncol=ncol(x), nrow=V)
}

