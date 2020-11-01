# package umap
# functions to compute k nearest neighbors


#' construct a umap.knn object describing nearest neighbors
#'
#' @export
#' @param indexes matrix, integers linking data points to nearest neighbors
#' @param distances matrix, distance values between pairs of points specified
#' in the matrix of indexes
#'
#' @return object of class umap.knn, which is a list with matrices with indexes
#' of nearest neighbors and distances to those neighbors
#'
#' @examples
#'
#' # this example describes a set of three data points (indexes 1,2,3)
#' # which are equidistant from each other. Hence the distance between
#' # pairs (i, j) is 0 for i=j and 1 otherwise.
#' three.indexes = matrix(c(1,2,3,
#'                          2,1,3,
#'                          3,1,2), nrow=3, ncol=3)
#' three.distances = matrix(c(0, 1, 1,
#'                            0, 1, 1,
#'                            0, 1, 1), nrow=3, ncol=3)
#'
#' umap.knn(three.indexes, three.distances)
#'
umap.knn = function(indexes, distances) {
  result = list(indexes=indexes, distances=distances)
  class(result) = "umap.knn"
  result
}


#' Compute knn information
#'
#' This function determines whether to obtain knn information using an exact
#' brute force approach or using an approximate algorithm
#'
#' @keywords internal
#' @noRd
#' @param d data matrix
#' @param config list with settings; relevant settings are as follows:
#' input - "data" or "dist"
#' n.neighbors - number of neighbors k
#' metric.function - function with signature f(a, b)
#' @param brute.force logical, during development, set FALSE to force
#' stochastic knn
#'
#' @return list with at least two components, indexes and distances
knn.info = function(d, config, brute.force=TRUE) {

  if (config$input=="dist") {
    return(knn.from.dist(d, config$n_neighbors))
  } 
  
  distfun = config$metric.function
  k = config$n_neighbors

  if (nrow(d)<2048 && brute.force) {
    # compute a distance matrix
    V = nrow(d)
    dT = t(d)
    d.dist = matrix(0, ncol=V, nrow=V)
    for (i in 1:(V-1)) {
      d.dist[(i+1):V, i] = distfun(dT, i, (i+1):V)
    }
    d.dist = abs(d.dist + t(d.dist))
    diag(d.dist) = -1
    rownames(d.dist) = rownames(d)
    # compute neighbors from the distance matrix
    result = knn.from.dist(d.dist, k)
    result$distances[,1] = 0
  } else {
    result = knn.from.data.reps(d, k, distfun, reps=config$knn_repeats)
  }

  result
}


#' compute knn information for spectators relative to data
#'
#' @keywords internal
#' @noRd
#' @param spectators matrix with data for spectators
#' @param d matrix with primary data
#' @param config list with settings
#' @param brute.force logical, during developemnt, set FALSE to force
#' stochastic knn
#'
#' @return list with at least two components, indexes and distances
spectator.knn.info = function(spectators, d, config, brute.force=TRUE) {
  
  distfun = config$metric.function
  Vd = nrow(d)
  Vdseq = seq_len(Vd)
  Vs = nrow(spectators)
  k = config$n_neighbors
  
  # create a joint dataset with both primary and spectator data
  alldata = cbind(t(d), t(spectators))
  
  if (nrow(spectators)<1024 && brute.force) {
    # compute distances from spectators to data
    d.dist = matrix(0, ncol=Vd, nrow=Vs)
    rownames(d.dist) = rownames(spectators)
    for (i in seq_len(nrow(spectators))) {
      # compute from the ith spectator to all the 
      d.dist[i, ] = distfun(alldata, Vd+i, Vdseq)
    }
    result = knn.from.dist(d.dist, k)
    # adjust result to let each item be closest to itself
    result$indexes[, 2:k] = result$indexes[,1:(k-1),drop=FALSE]
    result$distances[, 2:k] = result$distances[,1:(k-1),drop=FALSE]
    result$indexes[,1] = Vd+(1:Vs)
    result$distances[,1] = 0
  } else {
    # generate knn using a stochastic algorithm
    result = knn.from.data(alldata, k, distfun, subsample.k=0.3,
                           fix.observations=Vd)
    # reduce result to just the knn components
    result$indexes = result$indexes[Vd+(1:Vs), ]
    result$distances = result$distances[Vd+(1:Vs),]
  }

  result
}


# ############################################################################
# Specific implementations


#' get information about k nearest neighbors from a distance object or from
#' a matrix with distances
#'
#' This implementation uses sorting of distances to identify the k elements 
#' that are nearest to each data point. The result is deterministic and exact.
#'
#' By definition, the first nearest neighbor to each point is the point itself.
#' Subsequent neighbors are "true" neighbors.
#'
#' @keywords internal
#' @noRd
#' @param d dist object or matrix with distances
#' @param k integer, number of neighbors
#'
#' @return list with two components;
#' indexes identifies, for each point in dataset, the set of k neighbors
#' distances provides distances from each point to those neighbors 
knn.from.dist = function(d, k) {

  ## internally, use d as a matrix
  if (is(d, "dist")) {
    d = as.matrix(d)
  }
  
  k = floor(k)
  if (k > ncol(d)) {
    umap.error("number of neighbors must be smaller than number of items")
  }
  if (k<=1) {
    umap.error("number of neighbors must be greater than 1")
  }
  
  # get indexes of nearest neighbors 
  items = seq_len(ncol(d))
  indexes = t(apply(d, 1, function(x) {
    items[order(x)][1:k]
  }))
  
  # extract a subset of the distances
  distances = matrix(0, ncol=k, nrow=nrow(d))
  for (i in seq_len(nrow(d))) {
    distances[i, ] = d[i, indexes[i, ]]
  }
  
  rownames(indexes) = rownames(distances) = rownames(d)
  umap.knn(indexes, distances)
}


#' get information about approximate k nearest neighbors from a data matrix
#'
#' This implementation uses a randomization scheme and thus produces
#' results that are nondeterministic and only approximately correct. The
#' algorithm is roughly inspired by Dong et al, but there are differences.
#' This is a rough implementation and improvements are possible.
#'
#' @keywords internal
#' @noRd
#' @param dT matrix with data (observations in columns, features in rows)
#' @param k integer, number of neighbors
#' @param metric.function function that returns a metric distance
#' @param subsample.k numeric, used for internal tuning of implementation
#' @param fix.observations integer, number of observations in dT that will
#' appear in knn
#'
#' @return list with two components;
#' indexes - identifies, for each point in dataset, the set of k neighbors
#' distances - provides distances from each point to those neighbors
#' num.computed - for diagnostics only, number of distances computed internally
#' avg.
knn.from.data = function(dT, k, metric.function, subsample.k=0.5,
                         fix.observations=NULL) {
  
  # number of vertices, i.e. items in dataset
  V = ncol(dT)
  # number of neighbors
  k = floor(k)
  
  if (!is.finite(k) | k>V | k<=1) {
    umap.error(paste(c("k must be finite, greater than 1,",
                       "and smaller than the number of items"), collapse=" "))
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
  
  # Vseq is a set of observations to link to (targets)
  Vseq = seq_len(V)
  if (!is.null(fix.observations)) {
    Vseq = seq_len(fix.observations)
  }
  # kseq is to select first-k items in a list
  kseq = seq_len(k)
  Vkseq = rep(seq_len(V), each=k)
  subsample.ratio = subsample.k/k
  
  # create a set of (neighbor-1) indexes that does not include x
  pick.random.k = function(x) {
    result = sample(Vseq, k, replace=FALSE)
    result[result!=x][1:(k-1)]
  }

  # trim matrices to k rows
  trim.to.k = function(x) {
    if (nrow(x)>k) {
      x = x[order(x[,2])[kseq],]
    }
    x
  }
  
  # create a neighborhoods using a list
  # each element will be a matrix.
  # Col 1 -> indexes to neighbors, Col2 -> distances 
  B = lapply(seq_len(V), function(i) {
    neighbors = pick.random.k(i)
    neighbor.distances = metric.function(dT, i, neighbors)
    # create matrix with indexes to neighbors and distances
    # set distance to self as -1; this ensures that self is always rank 1 
    result = matrix(c(i, neighbors, -1, neighbor.distances), ncol=2)
    result[order(result[,2]),]
  })
  # create reverse neighborhoods
  make.revB = function() {
    result = unlist(lapply(B, function(x) { x[kseq,1]}))
    result = split(Vkseq, result)
    if (!is.null(fix.observations)) {
      result = lapply(result, function(x) { x[x<=fix.observations] })
    }
    result
  }
  
  # get indeces of neighbors for an index or a set of indeces
  get.neighbors = function(x) {
    x[,1]
  }
  get.Bbar.set = function(j) {
    b.set = unlist(lapply(B[j], get.neighbors))
    rev.set = unlist(revB[j])
    c(b.set, rev.set)
  }

  # keep track of what neighbors have been checked 
  checked = lapply(B, function(x) {x[,1]} )
  
  # helper; starts with a working matrix and suggests better neighbors
  get.better.neighbors = function(i) {
    mat = B[[i]]
    # identify neighbors from this matrix, pick a few
    i.neighbors = unique(c(mat[,1], revB[[i]]))
    i.pick = ceiling(length(i.neighbors)*subsample.ratio)
    selection = sample(i.neighbors, i.pick, replace=FALSE)
    # identify neighbors not already in i.neighbors
    selection.neighbors = get.Bbar.set(selection)
    already.checked = checked[[i]]
    selection = setdiff(selection.neighbors, already.checked)
    if (length(selection)==0) {
      return (matrix(0, ncol=2, nrow=0))
    }
    # compute distances to the candidates, but return only the good ones
    checked[[i]] <<- c(already.checked, selection)
    result = matrix(c(selection, metric.function(dT, i, selection)), ncol=2)
    result[result[,2] < mat[k,2], , drop=FALSE]
  }

  # main iteration loop over dataset
  epoch = 0
  # track of vertices that have found good neighbors and
  # those that still require work
  process = rep(TRUE, V)
  while (sum(process)>0) {
    continue = 0
    epoch = epoch + 1
    newB = B
    revB = make.revB()
    for (i in which(process)) {
      better = get.better.neighbors(i)
      if (nrow(better)>0) {
        # update the representation of the i'th neighborhood
        newB[[i]] = rbind(B[[i]], better)
        process[B[[i]][,1]] = TRUE
      } else {
        process[i] = FALSE
      }
    }
    B = lapply(newB, trim.to.k)
  }

  # make sure neighbors are sorted (trim.to.k does not guarantee that)
  B = lapply(B, function(x) { x[order(x[,2])[kseq],] })
  
  # convert from matrix representations 
  indexes = lapply(B, function(x) { x[,1]} )
  indexes = do.call(rbind, indexes)
  distances = lapply(B, function(x) { x[,2]})
  distances = do.call(rbind, distances)
  # by definition, distances to self are zero
  distances[,1] = 0
  rownames(indexes) = rownames(distances) = colnames(dT)
  umap.knn(indexes, distances)
}


#' Repeat knn.from.data multiple times, pick the best neighbors
#'
#' @keywords internal
#' @noRd
#' @param d matrix with data
#' @param k integer, number of neighbors
#' @param metric.function function that returns a metric distance
#' @param subsample.k numeric, used for internal tuning of implementation
#' @param reps integer, number of repeats to carry out
#'
#' @return list of same format as knn.from.data
knn.from.data.reps = function(d, k, metric.function, subsample.k=0.5, reps=2) {

  V = nrow(d)
  dT = t(d)
  
  # generate an initial knn
  result = knn.from.data(dT, k, metric.function, subsample.k)

  reps = ceiling(reps)
  if (reps<2) {
    return(result)
  }
  
  # perform more knn, keep best results
  for (rr in seq(2, reps)) {
    counter = 0
    newresult = knn.from.data(dT, k, metric.function, subsample.k)
    for (i in seq_len(V)) {
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

