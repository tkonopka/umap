## package umap
## some handy and generic functions, including distance 


## ############################################################################
## Logging


##' Send a message() with a prefix with a data
##'
##' @keywords internal
##' @param x character
##' @param verbose logical
message.w.date = function(x, verbose=FALSE) {
  if (verbose) {
    message(paste0("[",Sys.time(), "]  ", x))
  }
}


