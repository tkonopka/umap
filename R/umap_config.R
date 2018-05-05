## package umap
## configuration for a umap analysis


##' Display contents of a umap configuration
##'
##' @param x object of class umapconfig
##' @param ... ignored
##'
##' @export
print.umap.config = function(x, ...) {
  if (class(x)!="umap.config") {
    umap.error("not a umap configuration object")
  }

  ## produce a string of form "  z:  " of total length width
  padspaces = function(z, width=24) {
    padleft = max(0, width-nchar(z)-2)
    paste(c(rep(" ", padleft), z, ": "), collapse="")
  }
  
  message("umap configuration parameters")
  sapply(names(x), function(z) {
    zval = x[[z]]
    if (class(zval) %in% c("numeric", "integer", "character", "logical")) {
      message(padspaces(z), paste(zval, collapse=" "))
    } else {
      message(padspaces(z), "[", class(zval), "]")
    }
  })

  invisible(x)
}

