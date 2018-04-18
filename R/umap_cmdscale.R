## package umap
## an implementation of embedding following the umap interface, but using cmdscale


##' create an embedding using cmdscale
##'
##' @param d distance object
##' @param config list with settings; ignored in this implementation
##'
##' @export
umap.cmdscale = function(d, config) {
  cmdscale(d)
}

