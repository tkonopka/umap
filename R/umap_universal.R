## package umap
## functions used in umap that can be expected to be used by multiple implementations




##' Compute knn information
##'
##' This function determines whether to obtain knn information using an exact
##' brute force approach or using an approximate algorithm
##'
##' @param d data matrix
##' @param config list with settings; relevant settings are as follows:
##' input - "data" or "dist"
##' n.neighbors - number of neighbors k
##' metric.function - function with signature f(a, b)
##'
##' @return list with at least two components, indexes and distances
knn.info = function(d, config) {

  if (config$input=="dist") {
    return(knn.from.dist(d, config$n.neighbors))
  } 
  
  distfun = config$metric.function
  
  if (nrow(d)*config$n.neighbors<4096) {
    ## compute a distance matrix
    V = nrow(d)
    d.dist = matrix(0, ncol=V, nrow=V)
    for (i in 1:(V-1)) {
      for (j in (i+1):V) {
        d.dist[i,j] = distfun(d[i,], d[j,])
      }
    }
    d.dist = d.dist + t(d.dist)
    rownames(d.dist) = colnames(d.dist) = rownames(d)
    result = knn.from.dist(d.dist, config$n.neighbors)
  } else {
    result = knn.from.data.reps(d, config$n.neighbors, distfun, reps=config$knn.repeats)
  }

  class(result) = "umap.knn"
  result
}




##' Make an initial embedding with random coordinates
##'
##' @param V integer, number of vertices (rows)
##' @param d integer, number of diemsions (columns)
##' @param lims numeric vector with lower and upper bounds
##'
##' @return matrix (V,d) with random numbers
make.random.embedding = function(V, d, lims=c(-10, 10)) {
  matrix(stats::runif(V*d, lims[1], lims[2]), nrow=V, ncol=d)
}




##' Create a spectral embedding for a connectivity graph
##'
##' @param V integer, number of vertices in embedding
##' @param d integer, number of dimensions
##' @param g coo object
##'
##' @return embedding matrix
make.spectral.embedding = function(V, d, g) {

  ## identify connected components in graph coo
  gcomp = concomp.coo(g)
  
  if (gcomp$n.components==1) {
    lanczos.m = min(V-1, max(11, 2*d+1))
    result = spectral.coo(g, d, m=lanczos.m)
  } else {
    compsizes = sort(table(gcomp$components), decreasing=T)
    largestcomp = names(compsizes)[1]
    glarge = subset.coo(g, (1:V)[gcomp$components==largestcomp])
    ## choose a large number of lanczos vectors
    lanczos.m = min(V-1, max(11, 2*d+1))
    gspectral = spectral.coo(glarge, d, m=lanczos.m)
    ## make a random embedding, then fill in
    result = make.random.embedding(V, d, range(gspectral))
    result[gcomp$components==largestcomp,] = gspectral
  }
  
  result = result[, 1:d, drop=FALSE]
  result = center.embedding(result)
  ## rescale result so that most points are in range [-10, 10]
  range1 = stats::quantile(result[,1], p=c(0.01, 0.99))
  expansion = 10/(range1[2]-range1[1])
  result = expansion*result + matrix(stats::rnorm(V*d, 0, 0.001), nrow=V, ncol=d)
  result = center.embedding(result)
  
  result
}




##' Create an initial embedding for a graph
##'
##' This either takes a set embedding from config, or sets a random state
##'
##' @param V integer, number of vertices
##' @param config list with settings
##' @param g coo object with graph connectivity
##'
##' @return matrix with an embedding
make.initial.embedding = function(V, config, g=NULL) {

  numcomp = config$n.components
  
  ## make am ebedding, either using a premade matrix, or with random numbers
  if (class(config$init) == "matrix") {
    result = config$init
  } else {
    if (config$init=="spectral") {
      if (V > (2*(numcomp+1)+1)) {
        result = make.spectral.embedding(V, numcomp, g)
      } else {
        warning("spectral embedding requires n.components << (V-1)/2; using init='random'",
                call.=FALSE)
        result = make.random.embedding(V, numcomp)
      }
    } else {
      result = make.random.embedding(V, numcomp)
    }
  }
  
  if (nrow(result)!=V) {
    umap.error("initial embedding is incompatible")
  }
  if (ncol(result)!=numcomp) {
    umap.error("initial embedding is incompatible (dimension)")
  }
  
  result 
}




##' Estimate a/b parameters
##'
##' This performs a brute-force search in parameter space.
##' The algorithm assumes a narrowing-down search will produce a decent fit.
##'
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



##' Compute something
##'
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
##' This R implementation can be made slightly faster by hard-coding
##' the xmax value inside the function. However, performance is
##' even better with Rcpp. Hence, this implementation exists mainly
##' for reference and to facilitate testing the Rcpp version.
##'
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
##' @param x matrix
##'
##' @return matrix of same shape as x
center.embedding = function(x) {
  colcenters = apply(x, 2, mean)
  V = nrow(x)
  x - matrix(rep(colcenters, each=V), ncol=ncol(x), nrow=V)
}




##' Compute vector norm
##'
##' @param z numeric vector
##'
##' @return numeric, vector norm
vector.norm = function(z) {
  sqrt(sum(z*z))
}


