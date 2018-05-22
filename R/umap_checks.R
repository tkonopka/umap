## package umap
## functions for argument checking


##' Validator functions for umap settings
##'
##' @param config list with umap arguments
##' @param ... other arguments
##'
##' @return config object, may contain some different components from config in input
umap.check.config = function(config=umap.defaults, ...) {
  
  ## transfer values from arguments into the config object
  arguments = list(...)
  for (onearg in names(arguments)) {
    config[[onearg]] = arguments[[onearg]]
  }

  ## manual adjustments on some settings
  config$n.neighbors = ceiling(config$n.neighbors)

  ## checks on individual parameters
  if (config$n.neighbors<2) {
    umap.error("number of neighbors must be greater than 1")
  }

  if (!is.finite(config$n.epochs) | config$n.epochs<0) {
    umap.error("number of epochs must be positive")
  }

  if (!config$input %in% c("data", "dist")) {
    umap.error("setting 'data' must be either 'data' or 'dist'")
  }

  if (!is.finite(config$local.connectivity) | config$local.connectivity <= 0) {
    umap.error("setting 'local.connectivity' must be >= 0")
  }
  
  ## replace a distance description by a function
  if (class(config$metric.function)!="function") {
    if (config$metric.function=="euclidean") {
      config$metric.function = dEuclidean
    } else {
      stop("unrecognized distance description: ", config$metric.function, "\n", .call=FALSE)
    }
  }

  ## return prepared configuration
  config
}




##' stop execution with a custom error message
##'
##' @param x string for error message
umap.error = function(x) {
  stop(paste0("umap: ", x, "\n"), .call=FALSE)
}

