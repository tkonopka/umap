## package umap
## functions to compute k nearest neighbors


##' get information about k nearest neighbors from a distance object or from a matrix
##' with distances
##'
##' This implementation uses sorting of distances to identify the k elements 
##' that are nearest to each data point. The result is deterministic and exact.
##'
##' By definition, the first nearest neighbor to each point is the point itself.
##' Subsequent neighbors are "true" neighbors.
##'
##' @param d dist object or matrix with distances
##' @param k integer, number of neighbors
##'
##' @return list with two components;
##' indexes identifies, for each point in dataset, the set of k neighbors
##' distances provides distances from each point to those neighbors 
knn.from.dist = function(d, k) {

  ## internally, use d as a matrix
  if (class(d)=="dist") {
    d = as.matrix(d)
  }
  
  k = floor(k)
  if (k > nrow(d)) {
    stop("number of neighbors must be smaller than number of items\n")
  }
  if (k<=1) {
    stop("number of neighbors must be greater than 1\n")
  }
  
  ## ensure that only self-distances have zero values
  dtemp = d+1
  diag(dtemp) = 0
  
  ## get indexes of nearest neighbors (look for one more, which will be self)
  items = 1:nrow(d)
  indeces = apply(dtemp, 1, function(x) {
    items[order(x)][1:k]
  })
  ## make vertical, ignore the index of self
  indexes = t(indeces)

  ## copy distances from initial d
  distances = matrix(0, ncol=k, nrow=nrow(d))
  for (i in 1:nrow(d)) {
    distances[i, ] = d[i, indexes[i, ]]
  }
  
  rownames(indexes) = rownames(distances) = rownames(d)
  
  list(indexes=indexes, distances=distances)
}




##' get information about approximate k nearest neighbors from a data matrix
##'
##' This implementation uses a randomization scheme and thus produces
##' results that are nondeterministic and only approximately correct.
##'
##' @param d matrix with data
##' @param k integer, number of neighbors
##' @param metric.function function with signature f(a, b) that returns a metric distance
##' @param subsample.k numeric, used for internal tuning of implementation
##'
##' @return list with two components;
##' indexes - identifies, for each point in dataset, the set of k neighbors
##' distances - provides distances from each point to those neighbors
##' num.computed - for diagnostics only, gives the number of distances computed internally
##' avg.
knn.from.data = function(d, k, metric.function, subsample.k=0.5) {

  ## number of vertices, i.e. items in dataset
  V = nrow(d)
  ## number of neighbors
  k = floor(k)

  if (!is.finite(k) | k>V | k<=1) {
    stop("k must be greater than 1 and smaller than the number of items\n")
  }
  if (!is.finite(subsample.k) | subsample.k<0) {
    stop("subsample.k must be finite and greater than zero")
  }
  if (k<5) {
    subsample.k = k
  }
  if (subsample.k<1) {
    subsample.k = min(V-1, max(4, ceiling(k*subsample.k)))
  }
  if (subsample.k<1 | subsample.k>nrow(d)) {
    stop("subsample.k must be greater than 1 and smaller than the number of items\n")
  }

  ## some preset series
  Vseq = 1:V
  kseq = 1:k
  
  ## internal helper; creates a set of (neighbor-1) indexes that does not include x
  pick.random.k = function(x) {
    result = sample(Vseq, k, replace=F)
    result[result!=x][1:(k-1)]
  }

  ## compute distances between vertex i and a set of vertices j
  get.distances = function(i,j) {
    current = d[i,]
    ##sapply(j, function(x) { metric.function(current, d[x,]) })
    apply(d[j, ,drop=FALSE], 1, metric.function, current)
  }
  
  ## helper; create a matrix with col 1 -> indexes to neighbors, col 2 -> distances
  ## param i - index of source vertex
  ## neighbors - indeces of other vertices
  make.working.rep = function(i, neighbors) {
    current = d[i,]
    result = cbind(neighbors, get.distances(i, neighbors))
    colnames(result) = NULL
    result
  }
  
  ## create a working representation using a list
  ## each element will be a matrix. Col 1 -> indexes to neighbors, Col2 -> distances 
  work = lapply(as.list(1:V), function(i) {
    neighbors = c(i, pick.random.k(i))
    result = make.working.rep(i, neighbors)
    ## set distance to self as negative
    ## (this ensures that self is always in neighbor even when set is re-ordered)
    result[1,2]=-1
    result[order(result[,2]),]
    result
  })

  ## get indeces of neighbors for an index or a set of indeces
  get.neighbors = function(x) {
    x[,1]
  }
  get.neighbors.set = function(j) {
    unique(unlist(lapply(work[j], get.neighbors)))
  }
  
  ## helper; starts with a working matrix and suggests better neighbors
  get.better.neighbors = function(i, workmat) {
    ## identify neighbors from this matrix, pick a few
    i.neighbors = workmat[,1]
    selection = sample(i.neighbors[kseq], subsample.k, replace=F)
    ## identify neighbors not already in i.neighbors
    selection.neighbors = get.neighbors.set(selection)
    selection = setdiff(selection.neighbors, i.neighbors)
    if (length(selection)==0) {
      return (NULL)
    }
    ## compute distances to the candidates, but return only the good ones
    result = make.working.rep(i, selection)    
    result = result[result[,2] < workmat[k,2], ,drop=FALSE]
    if (nrow(result)==0) {
      return (NULL)
    }
    result
  }

  ## main iteration loop over dataset
  epoch = 0
  ## keep track of vertices that have found good neighbors and those that require work
  process = rep(TRUE, V)
  while (sum(process)>0) {
    epoch = epoch + 1
    work.new = work
    for (i in which(process)) {
      better = get.better.neighbors(i, work[[i]])
      if (!is.null(better)) {
        inew = rbind(work[[i]], better)
        ##work[[i]] = rbind(work[[i]], better)
        iorder = order(inew[,2])[kseq]
        inew = inew[iorder,]
        process[inew[kseq,1]] = TRUE
        work.new[[i]] = inew
      } else {
        process[i] = FALSE
      }
    }
    ## at beginning, process all nodes
    if (epoch<4) {
      process = rep(TRUE, V)
    }
    work = work.new
  }
  
  ## trim the work matrices to the few best candidates
  work.trimmed = lapply(work, function(w) {
    w[order(w[,2])[1:k],]
  })
  
  ## convert from working representation into output representation
  indexes = lapply(work.trimmed, function(x) { x[,1]} )
  indexes = do.call(rbind, indexes)
  distances = lapply(work.trimmed, function(x) { x[,2]})
  distances = do.call(rbind, distances)
  ## by definition, distances to self are zero
  distances[,1] = 0
  rownames(indexes) = rownames(distances) = rownames(d)
  
  list(indexes=indexes, distances=distances, epoch=epoch)
}

