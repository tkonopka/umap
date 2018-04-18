## package umap
##
## This is the entry code to umap in R
##
## UMAP stands for "Uniform Manifold Approximation and Projection"
## UMAP is a method proposed by Leland McInnes and .
## The original implementation was written in python by Leland McInnes (github.com/lmcinnes/umap)
##
## This package contains a "translation" of the original code into R
##




##' default configuration for umap 
##'
##' @export
umap.defaults = list(
  n.neighbors=15,
  n.components=2,
  n.epochs=5,
  init="spectral",
  spread=1,
  min.dist=0.1,
  bandwidth=1.0,
  gamma=1.0,
  a=NA,
  b=NA,
  verbose=FALSE
)
class(umap.defaults) = "umap.config"




##' compute a manifold approximation and projection
##'
##' @param d matrix
##' @param config object of class umap.config
##' @param method character, implementation
##' @param ... list of settings; overwrite settings in config
##'
##' @export
umap = function(d, config=umap.defaults, method=c("naive", "cmdscale"),
                ...) {
  
  method = match.arg(method)
  
  ## prepare a single object with all settings
  config = umap.check.config(config, ...)

  if (class(d)!="dist") {
    stop("object d must of type dist\n")
  }
  
  if (method=="naive") {
    result = umap.naive(d, config)
  } else if (method=="cmdscale") {
    result = umap.cmdscale(d, config)
  }
  
  result
}

