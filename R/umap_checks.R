## package umap
## functions for argument checking


##' Validator functions for umap settings
##'
##' @keywords internal
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

  ## force some data types
  for (x in c("n.epochs", "n.neighbors", "n.components",
              "seed", "negative.sample.rate")) {
    config[[x]] = as.integer(config[[x]])
  }
  
  ## always give a metric name
  config$metric.name = "custom"
  ## replace a distance description by a function
  if (class(config$metric.function)!="function") {
    config$metric.name = config$metric.function
    available.metrics = c(manhattan=mdManhattan,
                          pearson2=mdCenteredPearson, ## relies on centering during prep
                          pearson=mdCosine, ## relies on centering during prep
                          cosine=mdCosine,
                          euclidean=mdEuclidean)
    if (config$metric.function %in% names(available.metrics)) {
      config$metric.function = available.metrics[[config$metric.function]]
    } else {
      if (config$method != "python") {
        umap.error("unrecognized distance description: ", config$metric.function)
      }
    }
  }
  
  ## return prepared configuration
  config
}




##' Prep primary input as a data matrix
##'
##' @keywords internal
##' @param d matrix or compatible
##' @param config list with settings
##'
##' @return d as matrix
umap.prep.input = function(d, config) {
  ## for d into a matrix
  if (class(d)=="matrix") {
    d = d
  } else if (sum(class(d) %in% c("data.frame", "data.table"))) {
    d = as.matrix(d)
  } else {
    umap.error("input must be a matrix or matrix-compatible\n")
  }
  ## ensure data is numeric (not integer or other data type)
  d[,1] = as.numeric(d[,1])
  
  ## perhaps adjust the data matrix
  if (config$metric.name %in% c("pearson", "pearson2")) {
    ## for pearson correlation distance, center by-sample
    ## (this avoids computing means during correlations)
    d = t(d)
    d = t(d) - apply(d, 2, mean)
  }
  
  d
}




##' stop execution with a custom error message
##'
##' @keywords internal
##' @param ... strings for error message
umap.error = function(...) {
  x = paste(..., collapse=" ")
  stop(paste0("umap: ", x, "\n"), call.=FALSE)
}

