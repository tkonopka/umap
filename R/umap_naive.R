## package umap
## a from-scratch implementation of UMAP algorithm


##' create an embedding 
##'
##' @param d distance object
##' @param config list with settings
##'
##' @export
umap.naive = function(d, config) {

  ##simplicial.set = fuzzy.simplicial.set(d, config)
  
  c(1,2,3)
}




##' create a simplicial set from a distance object
##'
##' @param d distance object
##' @param config list with settings
##'
##' 
fuzzy.simplicial.set = function(d, config=umap.defaults) {

  if (class(d)!="matrix") {
    d = as.matrix(d)
  }
  
  ## extract neighbor information
  nei.info = knn.info(d, config$n.neighbors)
   
}




##' get information about k nearest neighbors
##'
##' @param dm matrix with distances
##' @param neighbors integer, number of neighbors
##'
##' @return list with neighbor indeces and distances
knn.info = function(dm, neighbors) {

  if (neighbors > nrow(dm)) {
    stop("number of neighbors must be smaller than number of items\n")
  }
  
  ## ensure that only self-distances have zero values
  dmtemp = dm+1
  diag(dmtemp) = 0
  
  ## get indexes of nearest neighbors (look for one more, which will be self)
  items = 1:nrow(dm)
  neiplus = neighbors+1
  indeces = apply(dmtemp, 1, function(x) {
    items[order(x)][1:neiplus]
  })
  ## make vertical, ignore the index of self
  indexes = t(indeces)[,2:nrow(indeces)]

  ## copy distances from initial dm
  distances = matrix(0, ncol=neighbors, nrow=nrow(dm))
  for (i in 1:nrow(dm)) {
    distances[i, ] = dm[i, indexes[i, ]]
  }
  
  list(indexes=indexes, distances=distances)
}

