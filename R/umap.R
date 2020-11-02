# package umap
#
# UMAP stands for "Uniform Manifold Approximation and Projection"
# UMAP is a method proposed by Leland McInnes and John Healy.
#
# The original implementation was written in python by Leland McInnes.
# The original implementation is available at https://github.com/lmcinnes/umap
#
# This package is an interface to using the UMAP algorithm in R. This file
# is the entrypoint to the package. It defines a configuration object
# and a umap() function. 
#


# These three lines required to use Rcpp; do not remove
#' @useDynLib umap
#' @importFrom Rcpp sourceCpp
NULL


# For interfacing with python and "umap-learn"
#' @importFrom reticulate py_module_available import
NULL


# This implements a "soft" requirement for python and the umap module
# i.e. the package should work when those components are absent
# but gain additional functionality when those components are present
#' interface to umap-learn via reticulate
#'
#' @keywords internal
#' @noRd
python.umap = NULL
.onLoad = function(libname, pkgname) {
  # this "try" block is necessary because:
  # a system that has python but not umap-learn stops during the test suite
  # with the following sequence of commands (devtools)
  # document(); test(); test()
  # note that test() only fails at second round
  try({
    python.umap <<- reticulate::import("umap", delay_load=TRUE)
  }, silent=TRUE)
}

  
#' Default configuration for umap 
#'
#' A list with parameters customizing a UMAP embedding. Each component of the
#' list is an effective argument for umap().
#'
#' n_neighbors: integer; number of nearest neighbors
#'
#' n_components: integer; dimension of target (output) space
#'
#' metric: character or function; determines how distances between
#' data points are computed. When using a string, available metrics are:
#' euclidean, manhattan. Other available generalized metrics are: cosine,
#' pearson, pearson2. Note the triangle inequality may not be satisfied by
#' some generalized metrics, hence knn search may not be optimal.
#' When using metric.function as a function, the signature must be
#' function(matrix, origin, target) and should compute a distance between
#' the origin column and the target columns
#'
#' n_epochs: integer; number of iterations performed during
#' layout optimization
#'
#' input: character, use either "data" or "dist"; determines whether the
#' primary input argument to umap() is treated as a data matrix or as a
#' distance matrix
#'
#' init: character or matrix. The default string "spectral" computes an initial
#' embedding using eigenvectors of the connectivity graph matrix. An
#' alternative is the string "random", which creates an initial layout based on
#' random coordinates. This setting.can also be set to a matrix, in which case
#' layout optimization begins from the provided coordinates.
#'
#' min_dist: numeric; determines how close points appear in the final layout
#'
#' set_op_ratio_mix_ratio: numeric in range [0,1]; determines who the knn-graph
#' is used to create a fuzzy simplicial graph
#'
#' local_connectivity: numeric; used during construction of fuzzy simplicial
#' set
#'
#' bandwidth: numeric; used during construction of fuzzy simplicial set
#'
#' alpha: numeric; initial value of "learning rate" of layout optimization
#'
#' gamma: numeric; determines, together with alpha, the learning rate of
#' layout optimization
#'
#' negative_sample_rate: integer; determines how many non-neighbor points are
#' used per point and per iteration during layout optimization
#'
#' a: numeric; contributes to gradient calculations during layout optimization.
#' When left at NA, a suitable value will be estimated automatically.
#'
#' b: numeric; contributes to gradient calculations during layout optimization.
#  When left at NA, a suitable value will be estimated automatically.
#'
#' spread: numeric; used during automatic estimation of a/b parameters.
#'
#' random_state: integer; seed for random number generation used during umap()
#'
#' transform_state: integer; seed for random number generation used during
#' predict()
#'
#' knn: object of class umap.knn; precomputed nearest neighbors
#'
#' knn.repeat: number of times to restart knn search
#'
#' verbose: logical or integer; determines whether to show progress messages
#'
#' umap_learn_args: vector of arguments to python package umap-learn
#'
#' @examples
#' # display all default settings
#' umap.defaults
#'
#' # create a new settings object with n_neighbors set to 5
#' custom.settings = umap.defaults
#' custom.settings$n_neighbors = 5
#' custom.settings
#' 
#' @export
umap.defaults = list(
  n_neighbors=15,
  n_components=2,
  metric="euclidean",
  n_epochs=200,
  input="data",
  init="spectral",
  min_dist=0.1,
  set_op_mix_ratio=1,
  local_connectivity=1,
  bandwidth=1.0,
  alpha=1,
  gamma=1.0,
  negative_sample_rate=5,
  a=NA,
  b=NA,
  spread=1,
  random_state=NA,
  transform_state=NA,
  knn=NA,
  knn_repeats=1,
  verbose=FALSE,
  umap_learn_args = NA
)
class(umap.defaults) = "umap.config"


#' Computes a manifold approximation and projection
#'
#' @export
#' @param d matrix, input data
#' @param config object of class umap.config
#' @param method character, implementation. Available methods are 'naive'
#' (an implementation written in pure R) and 'umap-learn' (requires python
#' package 'umap-learn')
#' @param preserve.seed logical, leave TRUE to insulate external code from
#' randomness within the umap algorithms; set FALSE to allow randomness used
#' in umap algorithms to alter the external random-number generator
#' @param ... list of settings; values overwrite defaults from config;
#' see documentation of umap.default for details about available settings
#'
#' @return object of class umap, containing at least a component
#' with an embedding and a component with configuration settings
#'
#' @examples
#' # embedd iris dataset using default settings
#' iris.umap = umap(iris[,1:4])
#'
#' # display object summary
#' iris.umap
#'
#' # display embedding coordinates
#' head(iris.umap$layout)
#'
umap = function(d, config=umap.defaults,
                method=c("naive", "umap-learn"),
                preserve.seed=TRUE,
                ...) {
  
  # prep - check inputs, configuration settings
  method = config$method = match.arg(method)
  config = umap.check.config(config, ...)  
  d = umap.prep.input(d, config)

  # save existing RNG seed, set "internal" seed
  old.seed = get.global.seed()
  if (!is.na(config$random_state)) {
    set.seed(config$random_state)
  }
  
  # perform the actual work with a specific umap implementation
  if (nrow(d)<=2) {
    result = umap.small(d, config)
  } else {
    implementations = c(naive=umap.naive,
                        "umap-learn"=umap.learn)
    if (method %in% names(implementations)) {
      result = implementations[[method]](d, config)
    } 
  }  
  class(result) = "umap"
  
  # restore state and finish
  if (preserve.seed) {
    set.global.seed(old.seed)
  }
  result
}


#' project data points onto an existing umap embedding
#'
#' @export
#' @param object trained object of class umap
#' @param data matrix with data
#' @param ... additional arguments (not used)
#'
#' @return new matrix
#'
#' @examples
#' # embedd iris dataset using default settings
#' iris.umap = umap(iris[,1:4])
#'
#' # create a dataset with structure like iris, but with perturbation
#' iris.perturbed = iris[,1:4] + matrix(rnorm(nrow(iris)*4, 0, 0.1), ncol=4)
#'
#' # project perturbed dataset
#' perturbed.embedding = predict(iris.umap, iris.perturbed)
#'
#' # output is a matrix with embedding coordinates
#' head(perturbed.embedding)
#'
predict.umap = function(object, data, ...) {
  
  umap.check.config.class(object$config)
  if (object$config$input == "dist") {
    umap.error("predict cannot work from object fitted by input='dist'")
  }
  if (nrow(object$layout)<=2) {
    umap.error("predict cannot work when too-small initial training set")
  }

  old.seed = get.global.seed()
  if (!is.na(object$config$transform_state)) {
    set.seed(object$config$transform_state)
  }
  
  # extract method from the umap object
  method = object$config$method
  implementations = c(naive=umap.naive.predict,
                      "umap-learn"=umap.learn.predict)
  if (!method %in% names(implementations)) {
    umap.error("unknown prediction method")
  }
  
  # carry out the predictions
  result = implementations[[method]](object, data)
  
  # restore state and finish
  set.global.seed(old.seed)
  result
}
