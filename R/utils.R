# package umap
# some handy and generic functions, including distance 


# ############################################################################
# Logging


#' Send a message() with a prefix with a data
#'
#' @keywords internal
#' @noRd
#' @param x character
#' @param verbose logical
message.w.date = function(x, verbose=FALSE) {
  if (verbose) {
    message(paste0("[",Sys.time(), "]  ", x))
  }
}




# ############################################################################
# Random number generation seeds


#' lookup .Random.seed in global environment
#'
#' @keywords internal
#' @noRd
get.global.seed = function() {
  current.seed = NA
  if (exists(".Random.seed", envir=.GlobalEnv)) {
    current.seed = .Random.seed
  }
  current.seed
}


#' set .Random.seed to a pre-saved value
#'
#' @keywords internal
#' @noRd
#' @param x integer vector
set.global.seed = function(x) {
  if (length(x)>1) {
    assign(".Random.seed", x, envir=.GlobalEnv)
  }
}

