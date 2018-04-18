## package umap
## functions for argument checking



##' Validator functions for umap settings
##'
##' @param config list with umap arguments
##' @param ... other arguments
umap.check.config = function(config=umap.defaults, ...) {
  
  arguments = list(...)
  for (onearg in names(arguments)) {
    config[[onearg]] = arguments[[onearg]]
  }

  ## return prepared configuration
  config
}

