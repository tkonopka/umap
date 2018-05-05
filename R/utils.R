## package umap
## some handy and generic functions, including distance 


## ############################################################################
## Logging


##' Send a message with a prefix with a data
##'
##' @param x character
message.w.date = function(x) {
  message(paste("[",Sys.time(), "]  ", x))
}



