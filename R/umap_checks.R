## package umap
## functions for argument checking



##' Validator functions for umap settings
##'
##' @param config list with umap arguments
##' @param ... other arguments
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
    umap.error("number of neighbors must be greater than 1\n")
  }

  if (!config$input %in% c("data", "dist")) {
    umap.error("setting 'data' must be either 'data' or 'dist'")
  }
  
  ## replace a distance description by a function
  if (class(config$metric.function)!="function") {
    if (config$metric.function=="euclidean") {
      config$metric.function = eucd
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
  stop(paste0("umap config: ", x, "\n"), .call=FALSE)
}

