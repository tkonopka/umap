## package umap
## functions related to creating an initial embedding




#' Make an initial embedding with random coordinates
#'
#' @keywords internal
#' @param d integer, number of diemsions (columns)
#' @param V integer, number of vertices (rows)
#' @param lims numeric vector with lower and upper bounds
#'
#' @return matrix (V,d) with random numbers
make.random.embedding = function(d, V, lims=c(-10, 10)) {
  matrix(stats::runif(V*d, lims[1], lims[2]), nrow=V, ncol=d)
}




#' get a set of k eigenvectors for the laplacian of x
#'
#' This implementation uses package RSpectra to compute eigenvectors.
#' Use of RSpectra as provider for sparse-matrix eigenvectors
#' credited to https://github.com/jlmelville/uwot
#'
#' @keywords internal
#' @param x coo object
#' @param k integer
#'
#' @return list with
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




#' Create a spectral embedding for a connectivity graph
#'
#' @keywords internal
#' @param d integer, number of dimensions
#' @param g coo object
#'
#' @return embedding matrix. Might fallback on random embedding if spectral embedding fails
make.spectral.embedding = function(d, g) {

  # identify connected components in graph
  gcomp = concomp.coo(g)
  V = g$n.elements
  
  # try to create spectral eigenvectors, abort quietly if not possible
  execute.spectral = function(g2) {
    result = NULL
    tryCatch({
      result = spectral.eigenvectors(g2, d+1)[, 1:d, drop=FALSE]
    }, error=function(e) {}, warning=function(e) {} )
    result
  }
  
  # create one embedding, or use a random embedding if not possible
  one.embedding = function(g2) {
    result = execute.spectral(g2)
    if (is.null(result)) {
      warning("failed creating initial embedding; using random embedding insteadx",
              call.=FALSE)
      result = make.random.embedding(d, g2$n.elements)
    }
    # center and rescale so that most points are in range [-10, 10]
    result = center.embedding(result)
    range1 = stats::quantile(result[,1], p=c(0.01, 0.99))
    expansion = 10/(range1[2]-range1[1])
    expansion*result
  }
  
  if (gcomp$n.components==1) {
    result = one.embedding(g)
  } else {
    compsizes = sort(table(gcomp$components), decreasing=T)
    # create a grid of offset vectors
    offset = lapply(as.list(1:d), function(x) { seq(0, ceiling(sqrt(length(compsizes)))) }) 
    offset = as.matrix(expand.grid(offset))
    offset = offset[order(apply(offset, 1, sum), apply(offset, 1, max)),,drop=FALSE]
    # create layout for each component in turn
    result = matrix(0, ncol=d, nrow=V)
    for (i in seq_along(compsizes)) {
      iname = names(compsizes)[i]
      ioffset = offset[i,]
      ihits = gcomp$components==iname
      ig = subset.coo(g, (1:V)[ihits])
      result[ihits, ] = one.embedding(ig) + 20*matrix(rep(ioffset, each=ig$n.elements), ncol=d)
    }
  }
  
  # add some small noise
  result + matrix(stats::rnorm(V*d, 0, 0.001), nrow=V, ncol=d)
}




#' Create an initial embedding for a graph
#'
#' This either takes a set embedding from config, or sets a random state
#'
#' @keywords internal
#' @param V integer, number of vertices
#' @param config list with settings
#' @param g coo object with graph connectivity
#'
#' @return matrix with an embedding
make.initial.embedding = function(V, config, g=NULL) {

  numcomp = config$n_components
  
  # make am ebedding, either using a premade matrix, or with random numbers
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
      warning("failed creating initial embedding; using random embedding instead",
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




#' Create an initial embedding for a set of spectators
#'
#' @keywords internal
#' @param embedding matrix with an existing (primary) embedding
#' @param knn.indexes matrix with indexes from spectators to components primary embedding
#'
#' @return matrix with an embedding for the spectator elements
make.initial.spectator.embedding = function(embedding, knn.indexes) {

  # crete an empty embedding for the spectators
  result = matrix(0, nrow=nrow(knn.indexes), ncol=ncol(embedding))
  rownames(result) = rownames(knn.indexes)

  # avoid the first column in indexes (references a self-index)
  knn.indexes = knn.indexes[,2:ncol(knn.indexes), drop=FALSE]
  
  # fill in coordinates by simple averaging 
  for (i in 1:nrow(result)) {
    result[i,] = colMeans(embedding[knn.indexes[i,], , drop=FALSE])
  }
  
  result
}


