# package umap
# functions using coo graphs related to spectral analysis




## ############################################################################
## Utility functions relevant for spectral decomposition of coo objects


#' Construct an identity matrix
#'
#' @keywords internal
#' @noRd
#' @param n.elements integer, number of elements
#' @param names character vector, names associated with the elements
#'
#' @return new coo object
identity.coo = function(n.elements, names=NULL) {
  if (!is.null(names)) {
    if (length(names)!=n.elements) {
      stop.coo("identify", "n.elements and names don't match")
    }
  }

  # construct coo matrix encoding diagonal elements
  coo = matrix(1, ncol=3, nrow=n.elements)
  coo[,1] = coo[,2] = 1:n.elements

  make.coo(coo, names, n.elements)  
}


#' Construct a normalized Laplacian for a graph
#'
#' This implementation constructs the laplacian  element-by-element.
#' Diagonals: 1, Element_ij = -1/sqrt(deg_i deg_j)
#'
#' @keywords internal
#' @noRd
#' @param x coo object encoding a graph
#'
#' @return new coo object 
laplacian.coo = function(x) {
  check.coo(x, "laplacian")
  
  # rely on a reduced representation of coo
  x = reduce.coo(x)

  num.from = length(unique(x$coo[, "from"]))
  if (num.from != x$n.elements) {
    stop.coo("laplacian", "singular degrees")
  }

  # get degrees
  degrees = x$coo[order(x$coo[, "from"]), ]
  degrees = vapply(split(degrees[,"value"], degrees[, "from"]), sum,
                   numeric(1))
  
  # construct diagonal part of the laplacian
  result = identity.coo(x$n.elements, x$names)
  
  # construct matrix with non-diagonal parts
  nondiag = x$coo
  nondiag = nondiag[nondiag[,1]!=nondiag[,2], ,drop=FALSE]
  # fill values of nondiagonal parts
  oldvalues = nondiag[, "value"]
  nondiag[, "value"] = degrees[nondiag[,"from"]] * degrees[nondiag[, "to"]]
  nondiag[, "value"] = -oldvalues/sqrt(nondiag[, "value"])
  
  # combine diagonal and non-diagonal parts
  result$coo = rbind(result$coo, nondiag)
  result$coo = result$coo[order(result$coo[, "from"], result$coo[, "to"]), ]
  
  result
}


#' Count the number of connected components in a coo graph
#'
#' @keywords internal
#' @noRd
#' @param x coo object
#'
#' @return list with number of connected components and a vector
#' assigning rows in object to components
concomp.coo = function(x) {
  check.coo(x, "submat")
  
  count = 0
  components = rep(NA, x$n.elements)
  visited = rep(FALSE, x$n.elements)
  
  pick.next = function() {
    which(!visited)[1]
  }
  
  # make sure only looking at non-zero components
  x = reduce.coo(x)
  # get lists of neighbors
  # (this will be a list with keys e.g. "1", "2", etc.)
  neighbors = split(x$coo[, "to"], x$coo[, "from"])

  epoch = 0
  tovisit = pick.next()
  while (!any(is.na(tovisit))) {
    epoch = epoch + 1    
    unvisited = which(!visited[tovisit])
    unvisited = tovisit[unvisited]

    # mark as visited
    if (length(unvisited)>0) {
      visited[tovisit] = TRUE
      components[tovisit] = count
      # identify all neighbors
      tovisit = unique(unlist(neighbors[as.character(tovisit)]))
    }

    # perhaps start a new component if this one is done
    if (length(unvisited)==0 | length(tovisit)==0) {
      count = count + 1
      tovisit = pick.next()
    }
  }
  
  list(n.components = count, components=components, epoch=epoch)
}


#' Subset a coo
#'
#' @keywords internal
#' @noRd
#' @param x coo object
#' @param items items (indexes) to keep
#'
#' @return new smaller coo object
subset.coo = function(x, items) {
  check.coo(x, "subset")
  
  # check for early exit
  if (length(items)==x$n.elements) {
    return (x)
  }
  
  # get logical vector for x indexes to keep
  x.keep = rep(FALSE, x$n.elements)
  x.keep[items] = TRUE

  # create result by trimming the input coo table
  result = x
  result$coo = x$coo[x$coo[,"from"] %in% items & x$coo[, "to"] %in% items, ]
  if (!is.null(x$names)) {
    result$names = x$names[x.keep]
    names(result$names) = NULL
  }
  result$n.elements = length(items)
  
  # re-label from-to
  new.indexes = rep(NA, x$n.elements)
  new.indexes[which(x.keep)] = seq_along(items)
  
  result$coo[, "from"] = new.indexes[result$coo[, "from"]]
  result$coo[, "to"] = new.indexes[result$coo[, "to"]]
  
  result
}

