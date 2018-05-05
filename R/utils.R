## package umap
## some handy and generic functions, including distance 


## ############################################################################
## Some distance related functions


## Compute squared distance between two vectors
##
## (For performance reasons, this function does not perform any checks)
## (Tried Rcpp here, but does not provide much speedup)
##
## @param x numeric vector
## @param y numeric vector
##
## @return numeric, squared distance
#eucd2 = function(x, y) {
#  z = x-y
#  sum(z*z)
#}





## Compute squared euclidean magnitude of a vector
##
## @param x numeric vector
##
## @return numeric, squared magnitude
#eucmag2 = function(x) {
#  sum(x*x)
#}




## Compute euclidean distances between two vectors (in any dimension)
##
## (For performance reasons, this function does not perform any checks)
## (Tried Rcpp here, but does not provide much speedup)
##
## @param x numeric vector
## @param y numeric vector
##
## @return numeric, euclidean distance
##
## @export
#eucd = function(x, y) {
#  z = x-y
#  sqrt(sum(z*z))
#}




## ############################################################################
## Logging


##' Send a message with a prefix with a data
##'
##' @param x character
message.w.date = function(x) {
  message(paste("[",Sys.time(), "]  ", x))
}



