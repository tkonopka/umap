# package umap
# functions for argument checking


#' Validator functions for umap settings
#'
#' @keywords internal
#' @noRd
#' @param config list with umap arguments
#' @param ... other arguments
#'
#' @return config object, may contain different components from config in input
umap.check.config = function(config=umap.defaults, ...) {

  umap.check.config.class(config)
  
  # transfer values from arguments into the config object
  arguments = list(...)
  for (onearg in names(arguments)) {
    config[[onearg]] = arguments[[onearg]]
  }

  missing = setdiff(names(umap.defaults), names(config))
  if (length(missing)>0) {
    umap.error(paste0("missing arguments: ", paste(missing, collapse=", ")))
  }

  # manual adjustments on some settings
  config$n_neighbors = ceiling(config$n_neighbors)

  # checks on individual parameters
  if (config$n_neighbors<2) {
    umap.error("number of neighbors must be greater than 1")
  }

  if (!is.finite(config$n_epochs) | config$n_epochs<0) {
    umap.error("number of epochs must be positive")
  }

  if (!config$input %in% c("data", "dist")) {
    umap.error("setting 'data' must be either 'data' or 'dist'")
  }

  if (!is.finite(config$local_connectivity) | config$local_connectivity <= 0) {
    umap.error("setting 'local_connectivity' must be >= 0")
  }

  # checks on embedding control via (a, b) vs. (min_dist, spread)
  if (!identical(config$a, NA) & identical(config$b, NA)) {
    umap.warning(
      "parameter 'a' is set but 'b' is not;\n",
      "value of 'a' will be ignored.\n",
      "(Embedding will be controlled via 'min_dist' and 'spread')")
  }
  if (!identical(config$b, NA) & identical(config$a, NA)) {
    umap.warning(
      "parameter 'b' is set but 'a' is not;\n",
      "value of 'b' will be ignored.\n",
      "(Embedding will be controlled via 'min_dist' and 'spread')") 
  }
  if (!identical(config$a, NA) & !identical(config$b, NA)) {
    abcontrol = "(Embedding will be controlled via 'a' and 'b')"
    if (!identical(config$min_dist, umap.defaults$min_dist)) {
      umap.warning(
        "parameters 'min_dist', 'a', 'b' are set to non-default values;\n",
        "parameter 'min_dist' will be ignored.\n", abcontrol)
    }
    if (!identical(config$spread, umap.defaults$spread)) {
      umap.warning(
        "parameters 'spread', 'a', 'b' are set to non-default values;\n",
        "parameter 'spread' will be ignored.\n", abcontrol)
    }
  }
  if (config$min_dist >= config$spread) {
    umap.error("setting 'min_dist' must be smaller than 'spread'")
  }
  if (config$min_dist <=0) {
    umap.error("setting 'min_dist' must be > 0")
  }
  
  # force some data types
  for (x in c("n_epochs", "n_neighbors", "n_components",
              "random_state", "negative_sample_rate", "transform_state")) {
    config[[x]] = as.integer(config[[x]])
  }
  
  # always give a metric name
  if (is(config$metric, "function")) {
    config$metric.function = config$metric
    config$metric = "custom"
  } else {
    # replace a distance description by a function
    # Notes:
    # mdCenteredPearson - relies on centering during prep
    # mdCosine - relies on centering during prep
    available.metrics = c(manhattan=mdManhattan,
                          pearson2=mdCenteredPearson, 
                          pearson=mdCosine, 
                          cosine=mdCosine,
                          euclidean=mdEuclidean)
    if (config$metric %in% names(available.metrics)) {
      config$metric.function = available.metrics[[config$metric]]
    } else {
      if (config$method != "umap-learn") {
        umap.error("unrecognized distance description: ", config$metric)
      }
    }
  }
  
  config
}




#' Prep primary input as a data matrix
#'
#' @keywords internal
#' @noRd
#' @param d matrix or compatible
#' @param config list with settings
#' @importFrom methods is
#'
#' @return d as matrix
umap.prep.input = function(d, config) {
  # for d into a matrix
  if (is(d, "matrix")) {
    d = d
  } else if (is(d, "data.frame")) {
    d = as.matrix(d)
  } else {
    umap.error("input must be a matrix or matrix-compatible\n")
  }
  # ensure data is numeric (not integer or other data type)
  d[,1] = as.numeric(d[,1])
  
  # perhaps adjust the data matrix
  if (config$metric %in% c("pearson", "pearson2")) {
    # for pearson correlation distance, center by-sample
    # (this avoids computing means during correlations)
    d = t(d)
    d = t(d) - apply(d, 2, mean)
  }
  
  d
}


#' stop execution with a custom error message
#'
#' @keywords internal
#' @noRd
#' @param ... strings for error message
umap.error = function(...) {
  x = paste(..., collapse=" ")
  stop(paste0("umap: ", x, "\n"), call.=FALSE)
}


#' create a warning message
#'
#' @keywords internal
#' @noRd
#' @param ... strings for error message
umap.warning = function(...) {
  x = paste(..., collapse=" ")
  warning(paste0("umap: ", x, "\n"), call.=FALSE)
}


#' Validator for config class component
#'
#' @keywords internal
#' @noRd
#' @param config list with arguments (object of class umap.config)
umap.check.config.class = function(config) {
  if (!is(config, "umap.config")) {
    umap.error("config is absent or corrupt")
  }
}

