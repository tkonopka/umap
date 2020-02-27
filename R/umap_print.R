# package umap
# functions for pretty-printing umap objects with print()


#' Display a summary of a umap object
#'
#' @keywords internal
#' @noRd
#' @param x umap object
#' @param ... other parameters (not used)
#'
#' @export
print.umap = function(x, ...) {
  if (!is(x, "umap")) {
    umap.error("x is not a umap object")
  }
  
  V = nrow(x$layout)
  d = ncol(x$layout)

  # create output string
  out = c(paste0("umap embedding of ", V, " items in ", d, " dimensions"),
          paste0("object components: ", paste(names(x), collapse=", ")))
  
  message(paste(out, collapse="\n"))
  
  invisible(x)
}


#' Display contents of a umap configuration
#'
#' @keywords internal
#' @noRd
#' @param x object of class umap.config
#' @param ... ignored
#'
#' @export
print.umap.config = function(x, ...) {
  if (!is(x, "umap.config")) {
    umap.error("x is not a umap configuration object")
  }
  
  # produce a string of form "  z:  " of total length width
  padspaces = function(z, width=24) {
    padleft = max(0, width-nchar(z)-2)
    paste(c(rep(" ", padleft), z, ": "), collapse="")
  }
  
  message("umap configuration parameters")
  primitives = c("numeric", "integer", "character", "logical")
  vapply(names(x), function(z) {
    zval = x[[z]]
    if (sum(class(zval) %in% primitives)) {
      message(padspaces(z), paste(zval, collapse=" "))
    } else {
      message(padspaces(z), "[", paste(class(zval), collapse=","), "]")
    }
    z
  }, character(1))

  invisible(x)
}


#' Display summary of knn.info
#'
#' @keywords internal
#' @noRd
#' @param x object of class umap.knn
#' @param ... ignored
#'
#' @export
print.umap.knn = function(x, ...) {
  if (!is(x, "umap.knn")) {
    umap.error("x is not a umap knn object")
  }
  
  V = nrow(x$indexes)
  k = ncol(x$indexes)
  
  # create output string
  out = c(paste0("k-nearest neighbors for ", V, " items with k=", k, ""),
          paste0("object components: ", paste(names(x), collapse=", ")),
          paste0("Note: nearest neighbors may be approximate"))
  
  message(paste(out, collapse="\n"))
  
  invisible(x)
}

