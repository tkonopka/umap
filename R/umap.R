## package umap
##
## This is the entry code to umap in R
##
## UMAP stands for "Uniform Manifold Approximation and Projection"
## UMAP is a method proposed by Leland McInnes and John Healy.
## The original implementation was written in python by Leland McInnes (github.com/lmcinnes/umap)
##
## This package contains a "translation" of the original code into R
##


## These three lines required to use Rcpp; do not remove
#' @useDynLib umap
#' @importFrom Rcpp sourceCpp
NULL




##' Default configuration for umap 
##'
##' Default
##'
##' @param n.neighbors integer
##' @param n.components integer
##'
##' @export
umap.defaults = list(
  n.neighbors=15,
  n.components=2,
  metric.function="euclidean",
  n.epochs=20,
  input="data",
  init="random",
  spread=1,
  min.dist=0.1,
  set.op.mix.ratio=1,
  local.connectivity=1,
  bandwidth=1.0,
  alpha=1,
  gamma=1.0,
  negative.sample.rate=5,
  a=NA,
  b=NA,
  seed=NA,
  verbose=FALSE
)
class(umap.defaults) = "umap.config"




##' Computes a manifold approximation and projection
##'
##' @param d matrix, input data
##' @param config object of class umap.config
##' @param method character, implementation
##' @param ... list of settings; overwrite settings in config
##'
##' @export
umap = function(d, config=umap.defaults, method=c("naive", "cmdscale"),
                ...) {
  
  ## prep - check inputs, configuration settings
  method = match.arg(method)
  config = umap.check.config(config, ...)  
  
  ## handle random number generators (save existing seed, set internal seed)
  if (exists(".Random.seed", envir=.GlobalEnv)) {
    old.seed = .Random.seed
  } else {
    old.seed = NA
  }
  if (!is.na(config$seed)) {
    set.seed(config$seed)
  }

  ## perform the actual work with a specific umap implementation
  if (method=="naive") {
    result = umap.naive(d, config)
  } else if (method=="cmdscale") {
    result = umap.cmdscale(d, config)
  }
  
  ## restore old seed
  if (length(old.seed)>1) {
    assign(".Random.seed", old.seed, envir=.GlobalEnv)
  }
  
  result
}

