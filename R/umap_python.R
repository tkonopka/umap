## package umap
## calculations via the umap python package
##
## Some of this implementation is similar to https://github.com/ropenscilabs/umapr




##' Create a umap embedding using a python package
##'
##' @param d data object
##' @param config list with settings
##'
##' @return list, one element of which is matrix with embedding coordinates
umap.python = function(d, config) {

  ## abort if python package is not available
  if (is.null(python.umap)) {
    umap.error("python package umap is not available")
  }

  ## adjust some values in config to please python type checkers
  if (is.na(config$seed)) {
    config$seed = as.integer(stats::runif(1, 0, 2^30))
  }
  config$verbose = 0+as.logical(config$verbose)
  
  ## construct python object 
  UMAP = python.umap$UMAP(
    n_neighbors = config$n.neighbors,
    n_components = config$n.component,
    metric = config$metric.name,
    n_epochs = config$n.epochs,
    alpha = config$alpha,
    init = config$init,
    spread = config$spread,
    min_dist = config$min.dist,
    set_op_mix_ratio = config$set.op.mix.ratio,  
    local_connectivity = config$local.connectivity,
    bandwidth = config$bandwidth,
    gamma = config$gamma,
    negative_sample_rate = config$negative.sample.rate,
    a = config$a,
    b = config$b,  
    random_state = config$seed,
    verbose = config$verbose
  )
  
  ##create embedding
  embedding = UMAP$fit_transform(d)
  
  list(layout=embedding)
}

