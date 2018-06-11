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
    umap.error("number of neighbors must be smaller than number of items")
  }
  if (k<=1) {
    umap.error("number of neighbors must be greater than 1")
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
##' results that are nondeterministic and only approximately correct. The
##' algorithm is roughly inspired by Dong et al, but there are differences.
##' This is a rough implementation and improvements are possible.
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
    umap.error("k must be greater than 1 and smaller than the number of items")
  }
  if (!is.finite(subsample.k) | subsample.k<0 | subsample.k>k) {
    umap.error("subsample.k must be finite, >0, <k")
  }
  if (k<5) {
    subsample.k = k
  }
  if (subsample.k<1) {
    subsample.k = min(V-1, max(4, ceiling(k*subsample.k)))
  }
  if (subsample.k>nrow(d)) {
    umap.error("subsample.k is too large")
  }
  
  ## some preset series
  Vseq = 1:V
  kseq = 1:k
  Vkseq = rep(Vseq, each=k)
  subsample.ratio = subsample.k/k
  
  ## internal helper; creates a set of (neighbor-1) indexes that does not include x
  pick.random.k = function(x) {
    result = sample(Vseq, k, replace=F)
    result[result!=x][1:(k-1)]
  }

  ## trim matrices to k rows
  trim.to.k = function(x) {
    if (nrow(x)>k) {
      x = x[order(x[,2])[kseq],]
    }
    x
  }
  
  ## create a neighborhoods using a list
  ## each element will be a matrix. Col 1 -> indexes to neighbors, Col2 -> distances 
  B = lapply(as.list(1:V), function(i) {
    neighbors = c(i, pick.random.k(i))
    ## create matrix with indexes to neighbors and distances
    ## set distance to self as -1; this ensures that self is always rank 1 
    result = matrix(c(neighbors, -1, metric.function(d[neighbors,])), ncol=2)
    result[order(result[,2]),]
  })
  ## create reverse neighborhoods
  make.revB = function() {
    result = unlist(lapply(B, function(x) { x[kseq,1]}))
    split(Vkseq, result)
  }
  
  ## get indeces of neighbors for an index or a set of indeces
  get.neighbors = function(x) {
    x[,1]
  }
  get.Bbar.set = function(j) {
    b.set = unlist(lapply(B[j], get.neighbors))
    rev.set = unlist(revB[j])
    c(b.set, rev.set)
  }

  ## keep track of 
  checked = lapply(B, function(x) {x[,1]} )
  
  ## helper; starts with a working matrix and suggests better neighbors
  get.better.neighbors = function(i) {
    mat = B[[i]]
    ## identify neighbors from this matrix, pick a few
    i.neighbors = unique(c(mat[,1], revB[[i]]))
    i.pick = ceiling(length(i.neighbors)*subsample.ratio)
    selection = sample(i.neighbors, i.pick, replace=F)
    ## identify neighbors not already in i.neighbors
    selection.neighbors = get.Bbar.set(selection)
    already.checked = checked[[i]]
    selection = setdiff(selection.neighbors, already.checked)
    if (length(selection)==0) {
      return (matrix(0, ncol=2, nrow=0))
    }
    ## compute distances to the candidates, but return only the good ones
    checked[[i]] <<- c(already.checked, selection)
    result = matrix(c(selection, metric.function(d[c(i, selection),])), ncol=2)
    result[result[,2] < mat[k,2], , drop=FALSE]
  }

  ## main iteration loop over dataset
  epoch = 0
  ## keep track of vertices that have found good neighbors and those that require work
  process = rep(TRUE, V)
  while (sum(process)>0) {
    continue = 0
    epoch = epoch + 1
    newB = B
    revB = make.revB()
    for (i in which(process)) {
      better = get.better.neighbors(i)
      if (nrow(better)>0) {
        ## update the representation of the i'th neighborhood
        newB[[i]] = rbind(B[[i]], better)
        process[B[[i]][,1]] = TRUE
      } else {
        process[i] = FALSE
      }
    }
    B = lapply(newB, trim.to.k)
  }

  ## make sure neighbors are sorted (trim.to.k does not guarantee that)
  B = lapply(B, function(x) { x[order(x[,2])[kseq],] })
  
  ## convert from matrix representations 
  indexes = lapply(B, function(x) { x[,1]} )
  indexes = do.call(rbind, indexes)
  distances = lapply(B, function(x) { x[,2]})
  distances = do.call(rbind, distances)
  ## by definition, distances to self are zero
  distances[,1] = 0
  rownames(indexes) = rownames(distances) = rownames(d)
  
  list(indexes=indexes, distances=distances)
}



##' Repeat knn.from.data multiple times, pick the best neighbors
##'
##' @param d matrix with data
##' @param k integer, number of neighbors
##' @param metric.function function with signature f(a, b) that returns a metric distance
##' @param subsample.k numeric, used for internal tuning of implementation
##' @param reps integer, number of repeats to carry out
##'
##' @return list of same format as knn.from.data
knn.from.data.reps = function(d, k, metric.function, subsample.k=0.5, reps=2) {

  ## generate an initial knn
  result = knn.from.data(d, k, metric.function, subsample.k)

  reps = ceiling(reps)
  if (reps<2) {
    return(result)
  }
  
  ## perform more knn, keep best results
  V = nrow(d)
  for (rr in 2:reps) {
    counter = 0
    newresult = knn.from.data(d, k, metric.function, subsample.k)
    for (i in 1:V) {
      nowdist = sum(result$distances[i,])
      newdist = sum(newresult$distances[i,])
      if (newdist<nowdist) {
        counter = counter+1
        result$distances[i,] = newresult$distances[i,]
        result$indexes[i,] = newresult$indexes[i,]
      }
    }
  }

  result
}

