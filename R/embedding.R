## package umap
## functions related to creating an initial embedding




##' Make an initial embedding with random coordinates
##'
##' @keywords internal
##' @param d integer, number of diemsions (columns)
##' @param V integer, number of vertices (rows)
##' @param lims numeric vector with lower and upper bounds
##'
##' @return matrix (V,d) with random numbers
make.random.embedding = function(d, V, lims=c(-10, 10)) {
  matrix(stats::runif(V*d, lims[1], lims[2]), nrow=V, ncol=d)
}




##' get a set of k eigenvectors for the laplacian of x
##'
##' This implementation uses package RSpectra to compute eigenvectors.
##' Use of RSpectra as provider for sparse-matrix eigenvectors
##' credited to https://github.com/jlmelville/uwot
##'
##' @keywords internal
##' @param x coo object
##' @param k integer
##'
##' @return list with
spectral.eigenvectors = function(x, k) {
  x.laplacian = laplacian.coo(x)
  x.sparse = methods::new("dgTMatrix",
                          i = as.integer(x.laplacian$coo[,"from"]-1),
                          j = as.integer(x.laplacian$coo[, "to"]-1),
                          Dim = as.integer(rep(x.laplacian$n.elements, 2)),
                          x = as.numeric(x.laplacian$coo[, "value"]))
  x.sparse = methods::as(x.sparse, "dgCMatrix")
  result = RSpectra::eigs(x.sparse, k, which="SM")$vectors
  rownames(result) = x$names
  result
}




##' Create a spectral embedding for a connectivity graph
##'
##' @keywords internal
##' @param d integer, number of dimensions
##' @param g coo object
##'
##' @return embedding matrix. Might return NULL if spectral embedding fails
make.spectral.embedding = function(d, g) {

  ## identify connected components in graph coo
  gcomp = concomp.coo(g)

  ## helper to decide if combinations of parameters are legal for spectral.coo
  execute.spectral = function(g2) {
    result = NULL
    ## try to create spectral eigenvectors, abort quietly if not possible
    tryCatch({
      result = spectral.eigenvectors(g2, d+1)[, 1:d, drop=FALSE]
    }, error=function(e) {}, warning=function(e) {} )
    result
  }
  
  result = NULL
  V = g$n.elements
  
  if (gcomp$n.components==1) {
    result = execute.spectral(g)
  } else {
    compsizes = sort(table(gcomp$components), decreasing=T)
    largestcomp = names(compsizes)[1]
    glarge = subset.coo(g, (1:V)[gcomp$components==largestcomp])
    gspectral = execute.spectral(glarge)
    if (!is.null(gspectral)) {
      ## make a random embedding, then fill in
      result = make.random.embedding(d, V, range(gspectral))
      result[gcomp$components==largestcomp,] = gspectral
    }
  }

  if (is.null(result)) {
    return (result)
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
##' @keywords internal
##' @param V integer, number of vertices
##' @param config list with settings
##' @param g coo object with graph connectivity
##'
##' @return matrix with an embedding
make.initial.embedding = function(V, config, g=NULL) {

  numcomp = config$n_components
  
  ## make am ebedding, either using a premade matrix, or with random numbers
  if (class(config$init) == "matrix") {
    result = config$init
  } else {
    result = NULL
    if (config$init=="spectral") {
      result = make.spectral.embedding(numcomp, g)
    } else if (config$init=="random") {
      result = make.random.embedding(numcomp, V)
    }
    if (is.null(result)) {    
      warning("failed creating initial embedding; using init='random'",
              call.=FALSE)
      result = make.random.embedding(numcomp, V)
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




##' Create an initial embedding for a set of spectators
##'
##' @keywords internal
##' @param embedding matrix with an existing (primary) embedding
##' @param knn.indexes matrix with indexes from spectators to components primary embedding
##'
##' @return matrix with an embedding for the spectator elements
make.initial.spectator.embedding = function(embedding, knn.indexes) {

  ## crete an empty embedding for the spectators
  result = matrix(0, nrow=nrow(knn.indexes), ncol=ncol(embedding))
  rownames(result) = rownames(knn.indexes)

  ## avoid the first column in indexes (references a self-index)
  knn.indexes = knn.indexes[,2:ncol(knn.indexes), drop=FALSE]
  
  ## fill in coordinates by simple averaging 
  for (i in 1:nrow(result)) {
    result[i,] = colMeans(embedding[knn.indexes[i,], ])
  }
  
  result
}


