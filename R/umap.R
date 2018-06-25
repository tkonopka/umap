## package umap
##
## UMAP stands for "Uniform Manifold Approximation and Projection"
## UMAP is a method proposed by Leland McInnes and John Healy.
##
## The original implementation was written in python by Leland McInnes.
## The original implementation is available at https://github.com/lmcinnes/umap
##
## This package is an interface to using the UMAP algorithm in R. This file
## is the entrypoint to the package. It defines a configuration object
## and a umap() function. 
##


## These three lines required to use Rcpp; do not remove
#' @useDynLib umap
#' @importFrom Rcpp sourceCpp
NULL




## These lines are required to control access to the umap python module
##
## This implements a "soft" requirement for python and the umap module
## i.e. the package should work when those components are absent
## but gain additional functionality when those components are present
python.umap = NULL
.onLoad = function(libname, pkgname) {
  has.reticulate = suppressWarnings(suppressMessages(requireNamespace("reticulate")))
  if (has.reticulate) {
    has.pkg.umap = reticulate::py_module_available("umap")
    if (has.pkg.umap) {
      ## assignment in parent environment!
      python.umap <<- reticulate::import("umap", delay_load=TRUE)
    }
  }
}




##' Default configuration for umap 
##'
##' A list with parameters customizing a UMAP embedding. Each component of the
##' list is an effective argument for umap().
##'
##' n.neighbors: integer; number of nearest neighbors
##'
##' n.components: integer; dimension of target (output) space
##'
##' metric.function: character or function; determines how distances between
##' data points are computed. When using a string, available metrics are:
##' euclidean, manhattan. Other availble generalized metrics are: cosine,
##' pearson, pearson2. Note the triangle inequality may not be satisfied by
##' some generalized metrics, hence knn search may not be optimal.
##' When using metric.function as a function, the signature must be
##' function(matrix) and should compute a distance between the first row
##' and all subsequent rows.
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
##' local.connectivity: numeric; used during construction of fuzzy simplicial set
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
##' knn.repeat: number of times to restart knn search
##'
##' verbose: logical or integer; determines whether to show progress messages
##'
##' @examples
##' # display all default settings
##' umap.defaults
##'
##' # create a new settings object with n.neighbors set to 5
##' custom.settings = umap.defaults
##' custom.settings$n.neighbors = 5
##' custom.settings
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
  knn.repeats=1,
  verbose=FALSE
)
class(umap.defaults) = "umap.config"




##' Computes a manifold approximation and projection
##'
##' @param d matrix, input data
##' @param config object of class umap.config
##' @param method character, implementation. Available methods are 'naive'
##' (an implementation written in pure R) and 'python' (requires python package
##' 'umap-learn')
##' @param ... list of settings; overwrite default values from config
##'
##' @return object of class umap, containing at least a component
##' with an embedding and a component with configuration settings
##'
##' @examples
##' # embedd iris dataset
##' # (using default settings, but with reduced number of epochs)
##' iris.umap = umap(iris[,1:4], n.epochs=20)
##'
##' # display object summary
##' iris.umap
##'
##' # display embedding coordinates
##' head(iris.umap$layout)
##'
##' @export
umap = function(d, config=umap.defaults, method=c("naive", "python"), ...) {
  
  ## prep - check inputs, configuration settings
  method = config$method = match.arg(method)
  config = umap.check.config(config, ...)  
  d = umap.prep.input(d, config)

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
  if (nrow(d)<=2) {
    result = umap.small(d, config)
  } else {
    implementations = c(naive=umap.naive, python=umap.python)
    if (method %in% names(implementations)) {
      result = implementations[[method]](d, config)
    } 
  }
  
  ## add a record of configuration into the result
  result[["config"]] = config
  class(result) = "umap"
  
  ## restore old seed
  if (length(old.seed)>1) {
    assign(".Random.seed", old.seed, envir=.GlobalEnv)
  }
  
  result
}

