## package umap
## functions to produce umap objects for extremely small datasets (0, 1, 2 items)



##' Create an embedding object compatible with package umap for very small inputs
##'
##' @param d matrix
##' @param config list with settings
##'
##' @return list, one element of which is matrix with embedding coordinates
umap.small = function(d, config) {
  
  warning("constructing layout for a very small input dataset", call.=FALSE)

  embedding = matrix(0, ncol=config$n.components, nrow=nrow(d))
  if (nrow(d)==2) {
    ## create two well-separate points
    embedding[1,] = 5
    embedding[2,] = -5
  }
  rownames(embedding) = rownames(d)

  ## in constrast to other umap.X implementations, this does not give knn data
  
  list(layout=embedding)
}

