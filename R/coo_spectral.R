## package umap
## functions using coo graphs related to spectral analysis



##' Get a set of k eigenvalues and eigenvectors
##'
##' @keywords internal
##' @param x coo object
##' @param k integer, number of eigenvalues/eigenvectors
##' @param m integer, number of lanczos vectors to use
##' (The higher the number the better, 2k+1 should be the minimum)
##'
##' @return list with two components
spectral.coo = function(x, k, m=2*k+1) {
  check.coo(x)
  
  ## get laplacian for the graph object
  x.laplacian = laplacian.coo(x)

  ## first get some Lanczos vectors for laplacian
  lanczos = lanczos.coo(x.laplacian, k, m)

  ## decompose the transfer matrix
  TU = svd(lanczos$T)$u
  TUsm = TU[, 1:k, drop=FALSE]
  
  ## use SVD decomposition of T to get eigenvectors 
  result = lanczos$V %*% TUsm
  
  rownames(result) = x$names
  result
}




## ############################################################################
## Utility functions relevant for general use


##' Construct an identity matrix
##'
##' @keywords internal
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
##' @keywords internal
##' @param x coo object
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
##' @keywords internal
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
  oldvalues = nondiag[, "value"]
  nondiag[, "value"] = degrees[nondiag[,"from"]] * degrees[nondiag[, "to"]]
  nondiag[, "value"] = -oldvalues/sqrt(nondiag[, "value"])
  
  ## combine diagonal and non-diagonal parts
  result$coo = rbind(result$coo, nondiag)
  result$coo = result$coo[order(result$coo[, "from"], result$coo[, "to"]), ]
  
  result
}




##' Count the number of connected components in a coo graph
##'
##' @keywords internal
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
##' Based on description on wikipedia:
##' https://en.wikipedia.org/wiki/Lanczos_algorithm
##' with restart implemented ad-hoc
##'
##' This does not work very well (eigenvectors don't match svd()). Help would be appreciated.
##'
##' @keywords internal
##' @param x coo object
##' @param k integer, number of vectors to optimize
##' @param m integer, number of vectors to use in procedure (set higher than k)
##'
##' @return list with vectors and transfer matrix
lanczos.coo = function(x, k, m) {
  check.coo(x, "lanczos")

  N = x$n.elements
  
  m = as.integer(m)
  k = as.integer(k)
  if (k<2 | k>=m) {
    stop.coo("lanczos.coo", "k must be at least 2, smaller than m")
  }
  if (k>=N | m >=N) {
    stop.coo("lanczos.coo", "k and m must be smaller than matrix")
  }

  xprep = multiplicationprep.coo(x)
  
  ## variables for iterative procedure
  V = matrix(0, ncol=m, nrow=N)
  W = matrix(0, ncol=m, nrow=N)
  T = matrix(0, ncol=m, nrow=m)
  
  ## create a new vector that is orthogonal to previous ones in V
  make.orthogonal.vector = function(j, Vj=NULL) {
    if (is.null(Vj)) {
      result = stats::rnorm(N)
      result = result/vector.norm(result)
    } else {
      result = as.numeric(Vj)
    }
    ## make orthogonal to previous 1:(j-1)
    for (n in seq_len(j-1)) {
      coeff = sum(V[,n]*result)
      result = result - coeff*V[,n]
    }
    result
  }
  
  ## helper to compute one lanczos iteration
  lanczos.iteration = function(j, Vj=NULL) {
    beta = vector.norm(W[,j-1])
    if (beta<1e-4 & !is.null(Vj)) {
      Vj = make.orthogonal.vector(j, Vj)
    } else {
      Vj = W[,j-1]/beta
    }
    Wj = vectormultiplication.coo(x, Vj, xprep)
    alpha = sum(Wj*Vj)
    ##Wj = Wj - alpha*Vj - beta*V[,j-1]
    Wj = make.orthogonal.vector(j, Wj-alpha*Vj)
    list(alpha=alpha, beta=beta, Vj=Vj, Wj=Wj)
  }

  ## initial round of lanczos algorithm
  V[,1] = make.orthogonal.vector(1)
  W[,1] = vectormultiplication.coo(x, V[,1], xprep)
  alpha = sum(W[,1] * V[,1])
  W[,1] = W[,1] - alpha*V[,1]
  T[1,1] = alpha
  for (j in 2:m) {
    j.result = lanczos.iteration(j)
    T[j,j] = j.result$alpha
    T[j-1,j] = T[j,j-1] = j.result$beta
    W[,j] = j.result$Wj
    V[,j] = j.result$Vj
  }
  
  ## helper: compute relative error within tolerance
  within.tol = function(z1, z2, tolerance=1e-6) {
    rel.err = abs(z1-z2)/abs(z1)
    max(rel.err)<tolerance
  }

  ## helper: compute svd decomposition of a matrix
  make.svd = function(mat) {
    result = svd(mat)
    result$d = rev(result$d)
    result$u = result$u[, ncol(result$u):1]
    result$v = NULL
    result
  }
  
  Teigen.previous = rep(0, nrow(T))
  Tsvd = make.svd(T)
  Teigen.current = Tsvd$d
  count = 0
  jstart = 1
  while (!within.tol(Teigen.current[1:k], Teigen.previous[1:k]) & count<m) {
    count = count + 1
    
    for (j in jstart:m) {
      if (j==1) {
        V[,1] = V %*% Tsvd$u[,1,drop=FALSE]
        W[,1] = vectormultiplication.coo(x, V[,1], xprep)
        alpha = sum(W[,1] * V[,1])
        W[,1] = W[,1] - alpha*V[,1]
        T[1,1] = alpha
      } else {
        if (j==jstart) {
          j.result = lanczos.iteration(j, Vj=V %*% Tsvd$u[,j,drop=FALSE])
        } else {
          j.result = lanczos.iteration(j)
        }
        T[j,j] = j.result$alpha
        T[j-1,j] = T[j,j-1] = j.result$beta
        W[,j] = j.result$Wj
        V[,j] = j.result$Vj
      }
    }
    
    ## identify how many components are consistent with Tsvd
    ## this allows restarting at higher j values in a future iteration
    jstart = 1
    while (jstart<m & within.tol(T[jstart, jstart], Tsvd$d[jstart])) {
      jstart = jstart +1
    }

    ## prep object for while() condition
    Tsvd = make.svd(T)
    Teigen.previous = Teigen.current
    Teigen.current = Tsvd$d
  }
  
  ## return a matrix with all the Lanczos vectors and the transfer matrix
  list(V=V, T=T)
}

