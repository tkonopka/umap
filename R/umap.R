## package umap
##
## UMAP stands for "Uniform Manifold Approximation and Projection"
## UMAP is a method proposed by Leland McInnes and John Healy.
##
## The original implementation was written in python by Leland McInnes.
## The original implementation is available at https://github.com/lmcinnes/umap
##
## This package contains a "translation" of the original code into R, with some
## modifications.
##


## These three lines required to use Rcpp; do not remove
#' @useDynLib umap
#' @importFrom Rcpp sourceCpp
NULL




##' Default configuration for umap 
##'
##' A list with parameters customizing a UMAP projection. Each component of the
##' list is an effective argument for umap().
##'
##' n.neighbors: integer; number of nearest neighbors
##'
##' n.components: integer; dimension of target (output) space
##'
##' metric.function: character or functions; determines how distances between
##' data points are computed.
##'
##' n.epochs: integer; number of iterations performed during
##' layout optimization
##'
##' input: character, use either "data" or "dist"; determines whether the primary
##' input argument to umap() is treated as a data matrix or as a distance matrix
##'
##' init: character or matrix. The default string "spectral" computes an initial
##' embedding using eigenvectors of the connectivity graph matrix. An alternative is
##' the string "random", which creates an initial layout based on random coordinates. 
##' This setting.can also be set to a matrix, in which case layout optimization
##' begins from the provided coordinates.
##'
##' min.dist: numeric; determines how close points appear in the final layout
##'
##' set.op.mix.ratio: numeric in range [0,1]; determines who the knn-graph
##' is used to create a fuzzy simplicial graph
##'
##' local.connectivity: numeric; used during construction of fuzzy simplicail set
##'
##' bandwidth: numeric; used during construction of fuzzy simplicial set
##'
##' alpha: numeric; initial value of "learning rate" of layout optimization
##'
##' beta: numeric; determines, together with alpha, the learning rate of layout optimization
##'
##' negative.sample.rate: integer; determines how many non-neighbor points are
##' used per point and per iteration during layout optimization
##'
##' a: numeric; contributes to gradient calculations during layout optimization.
##' When left at NA, a suitable value will be estimated automatically.
##'
##' b: numeric; contributes to gradient calculationss during layout optimization.
##  When left at NA, a suitable value will be estimated automatically.
##'
##' spread: numeric; used during automatic estimation of a/b parameters.
##'
##' seed: integer; seed for random number generation
##'
##' verbose: logical or integer; determines whether to show progress messages
##'
##' @export
umap.defaults = list(
  n.neighbors=15,
  n.components=2,
  metric.function="euclidean",
  n.epochs=200,
  input="data",
  init="spectral",
  min.dist=0.1,
  set.op.mix.ratio=1,
  local.connectivity=1,
  bandwidth=1.0,
  alpha=1,
  gamma=1.0,
  negative.sample.rate=5,
  a=NA,
  b=NA,
  spread=1,
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
umap = function(d, config=umap.defaults, method=c("naive"), ...) {
  
  ## prep - check inputs, configuration settings
  method = match.arg(method)
  config = umap.check.config(config, ...)  
  
  ## save existing RNG seed, set "internal" seed
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
  } 
  
  ## restore old seed
  if (length(old.seed)>1) {
    assign(".Random.seed", old.seed, envir=.GlobalEnv)
  }
  
  result
}

