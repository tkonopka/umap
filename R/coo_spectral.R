## package umap
## functions using coo graphs related to spectral analysis



##' Get a set of k eigenvalues and eigenvectors
##'
##' @param x coo object
##' @param k integer, number of eigenvalues/eigenvectors
##'
##' @return list with two components
spectral.coo = function(x, k) {
  check.coo(x)
  
  ## get laplacian for the graph object
  x.laplacian = laplacian.coo(x)
  
  ## first get some Lanczos vectors for x
  lanczos = lanczos.coo(x, 2*k+1)
  
  ## decompose the transfer matrix
  T.U = svd(lanczos$T)$u
  
  ## use SVD decomposition of T to get eigenvectors 
  result = lanczos$V %*% T.U[,1:k]
  
  rownames(result) = x$names
  result
}




## ############################################################################
## Utility functions relevant for general use


##' Construct an identity matrix
##'
##' @param n.elements integer, number of elements
##' @param names character vector, names associated with the elements
##'
##' @return new coo object
identity.coo = function(n.elements, names=NULL) {
  if (!is.null(names)) {
    if (length(names)!=n.elements) {
      stop.coo("identify", "n.elements and names don't match")
    }
  }

  ## construct coo matrix encoding diagonal elements
  coo = matrix(1, ncol=3, nrow=n.elements)
  coo[,1] = coo[,2] = 1:n.elements

  make.coo(coo, names, n.elements)  
}



##' Subset a coo
##'
##' @param  coo object
##' @param items items (indexes) to keep
##'
##' @return new smaller coo object
subset.coo = function(x, items) {
  check.coo(x, "subset")
  
  ## check for early exit
  if (length(items)==x$n.elements) {
    return (x);
  }

  ## get logical vector for x indexes to keep
  x.keep = rep(FALSE, x$n.elements)
  x.keep[items] = TRUE

  ## create result by trimming the input coo table
  result = x
  result$coo = x$coo[x$coo[,"from"] %in% items & x$coo[, "to"] %in% items, ]
  if (!is.null(x$names)) {
    result$names = x$names[x.keep]
    names(result$names) = NULL
  }
  result$n.elements = length(items)

  ## re-label from-to
  new.indexes = rep(NA, x$n.elements)
  new.indexes[which(x.keep)] = 1:length(items)

  result$coo[, "from"] = new.indexes[result$coo[, "from"]]
  result$coo[, "to"] = new.indexes[result$coo[, "to"]]
  
  result
}




## ############################################################################
## Utility functions relevant for spectral decomposition of coo objects


##' Construct a normalized Laplacian for a graph
##'
##' This implementation constructs the laplacian  element-by-element.
##' Diagonals: 1, Element_ij = -1/sqrt(deg_i deg_j)
##'
##' @param x coo object encoding a graph
##'
##' @return new coo object 
laplacian.coo = function(x) {
  check.coo(x, "laplacian")
  
  ## rely on a reduced representation of coo
  x = reduce.coo(x)

  num.from = length(unique(x$coo[, "from"]))
  if (num.from != x$n.elements) {
    stop.coo("laplacian", "singular degrees")
  }

  ## get degrees
  degrees = x$coo[order(x$coo[, "from"]), ]
  degrees = sapply(split(degrees[,"value"], degrees[, "from"]), sum)
  
  ## construct diagonal part of the laplacian
  result = identity.coo(x$n.elements, x$names)
  
  ## construct matrix with non-diagonal parts
  nondiag = x$coo
  nondiag = nondiag[nondiag[,1]!=nondiag[,2], ,drop=FALSE]
  ## fill values of nondiagonal parts
  nondiag[, "value"] = degrees[nondiag[,"from"]] * degrees[nondiag[, "to"]]
  nondiag[, "value"] = -1/sqrt(nondiag[, "value"])

  ## combine diagonal and non-diagonal parts
  result$coo = rbind(result$coo, nondiag)
  result$coo = result$coo[order(result$coo[, "from"], result$coo[, "to"]), ]
  
  result
}




##' Count the number of connected components in a coo graph
##'
##' @param x coo object
##'
##' @return list with number of connected components and a vector
##' assigning rows in object to components
concomp.coo = function(x) {
  check.coo(x, "submat")
  
  count = 0
  components = rep(NA, x$n.elements)
  visited = rep(FALSE, x$n.elements)
  
  pick.next = function() {
    which(!visited)[1]
  }
  
  ## make sure only looking at non-zero compoennts
  x = reduce.coo(x)
  ## get lists of neighbors (this will be a list with keys e.g. "1", "2", etc.
  neighbors = split(x$coo[, "to"], x$coo[, "from"])

  epoch = 0
  tovisit = pick.next()
  while (!any(is.na(tovisit))) {
    epoch = epoch + 1    
    unvisited = which(!visited[tovisit])
    unvisited = tovisit[unvisited]

    ## mark as visited
    if (length(unvisited)>0) {
      visited[tovisit] = TRUE
      components[tovisit] = count
      ## identify all neighbors
      tovisit = unique(unlist(neighbors[as.character(tovisit)]))
    }

    ## perhaps start a new component if this one is done
    if (length(unvisited)==0 | length(tovisit)==0) {
      ## need to start a new components
      count = count + 1
      tovisit = pick.next()
    }
  }
  
  list(n.components = count, components=components, epoch=epoch)
}




##' Construct matrix with k Lanczos vectors from a real Hermitian matrix x
##'
##' @param x coo object
##' @param k number of vectors
##'
##'
lanczos.coo = function(x, k) {
  check.coo(x, "lanczos")
  
  if (k<2) {
    stop.coo("lanczos.coo", "k must be at least 2")
  }
  
  ## prep variables for iterative procedure
  vs = vector("list", k)
  ws = vector("list", k)
  
  ## create an initial vector of unit norm
  vector.norm = function(z) {
    sqrt(sum(z*z))
  }
  make.one.vector = function() {
    vec = rnorm(x$n.elements)
    vec/vector.norm(vec)
  }
  vs[[1]] = make.one.vector()
  
  ws[[1]] = vectormultiplication.coo(x, vs[[1]])
  alpha = sum(ws[[1]] * vs[[1]])
  ws[[1]] = ws[[1]] - alpha*vs[[1]]

  T = matrix(0, ncol=k, nrow=k)
  T[1,1] = alpha

  ## iterate to compute next vectors from previous ones
  for (j in 2:k) {
    beta = vector.norm(ws[[j-1]])

    vs[[j]] = ws[[j-1]]/beta
    
    ws[[j]] = vectormultiplication.coo(x, vs[[j]])
    alpha = sum(ws[[j]]*vs[[j]])
    ws[[j]] = ws[[j]] - alpha*vs[[j]] - beta*vs[[j-1]]

    T[j,j] = alpha
    T[j-1, j] = T[j, j-1] = beta
  }

  ## return a matrix with all the Lanczos vectors and the transfer matrix
  list(V=do.call(cbind, vs), T=T)
}



