# package umap
# functions to produce umap objects for extremely small datasets
# i.e. datasets with 0, 1, or 2 items



#' Create an embedding object compatible for very small inputs
#'
#' @keywords internal
#' @noRd
#' @param d matrix
#' @param config list with settings
#'
#' @return list, one element of which is matrix with embedding coordinates
umap.small = function(d, config) {
  
  warning("constructing layout for a very small input dataset", call.=FALSE)

  embedding = matrix(0, ncol=config$n_components, nrow=nrow(d))
  if (nrow(d)==2) {
    embedding[1,] = 5
    embedding[2,] = -5
  }
  rownames(embedding) = rownames(d)
  
  list(layout=embedding, config=config)
}

