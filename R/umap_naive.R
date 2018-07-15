## package umap
## a from-scratch implementation of the UMAP algorithm
##
## This implementation is called naive because it is a rather direct translation
## of the original implementation by Leland McInnes. This implementation does,
## however, have some slight modifications.
##
## The original implementation is available at https://github.com/lmcinnes/umap.
##
## The original implementation was published under a BSD license.
##


##' Create a umap embedding
##'
##' This implementation is called naive because it is a rather straightforward
##' translation of the original python code.
##'
##' @keywords internal
##' @param d data object
##' @param config list with settings
##'
##' @return list, one element of which is matrix with embedding coordinates
umap.naive = function(d, config) {
  
  verbose = config$verbose
  
  ## prep parameters
  message.w.date("starting umap", verbose)
  if (is.na(config$a) | is.na(config$b)) {
    config[c("a", "b")] = find.ab.params(config$spread, config$min.dist)
  }
  
  ## perhaps extract knn from input
  knn = NULL
  if ("knn" %in% names(config)) {
    if (class(config$knn) == "umap.knn") {
      message.w.date("using supplied nearest neighbors", verbose)
      knn = config$knn
    }
  }
  if (is.null(knn)) {
    message.w.date("creating graph of nearest neighbors", verbose)
    knn =  knn.info(d, config)
  }
  
  ## create a graph representation
  graph = naive.fuzzy.simplicial.set(knn, config)
  message.w.date("creating initial embedding", verbose)
  embedding = make.initial.embedding(graph$n.elements, config, graph)
  message.w.date("optimizing embedding", verbose)
  embedding = naive.simplicial.set.embedding(graph, embedding, config)
  embedding = center.embedding(embedding)
  message.w.date("done", verbose)
  
  list(layout=embedding, knn=knn)
}




## ############################################################################
## Details of the implementation specific to umap.naive


##' create an embedding of graph into a low-dimensional space
##'
##' @keywords internal
##' @param g matrix, graph connectivity as coo
##' @param embedding matrix, coordinates for an initial graph embedding
##' @param config list with settings
##'
##' @return matrix with embedding,
##' nrows is from g, ncols determined from config
naive.simplicial.set.embedding = function(g, embedding, config) {

  ## create an initial embedding
  result = embedding
  rownames(result) = g$names

  if (config$n.epochs==0) {
    return(result)
  }
  
  total.weight = sum(g$coo[, "value"])
  
  ## simplify graph a little bit
  gmax = max(g$coo[, "value"])
  g$coo[g$coo[, "value"] < gmax/config$n.epochs, "value"] = 0
  g = reduce.coo(g)
  
  ## create an epochs-per-sample. Keep track of it together with the graph coo
  eps = cbind(g$coo,
              eps=make.epochs.per.sample(g$coo[, "value"], config$n.epochs))
  
  result = naive.optimize.embedding(result, config, eps)
  rownames(result) = g$names
  
  result
}




##' modify an existing embedding 
##' 
##' @keywords internal
##' @param embedding matrix holding an initial embedding
##' @param config list with settings
##' @param eps matrix with connectivity coo graph and epochs per sample;
##' this must be a matrix with columns "from", "to", "value", and "eps"
##'
##' @return matrix of same dimension as initial embedding
naive.optimize.embedding = function(embedding, config, eps) {

  ## number of vertices in embedding
  V = nrow(embedding)
  ## transpose to get observations in columns
  embedding = t(embedding)
  ## extract some variables from config
  abg = c(config$a, config$b, config$gamma)

  ## define some vectors for book-keeping
  ## integer matrix with pairs of data
  eps.pairs = matrix(as.integer(eps[, c("from", "to")]), ncol=2)-1
  eps.val = eps[, "eps"]
  ## epns is short for "epochs per negative sample"
  epns = eps.val/config$negative.sample.rate
  ## eon2s is short for "epochs of next negative sample"
  eon2s = epns
  ## eons is short for "epochs of next sample"
  eons = eps.val
  ## nns is next negative sample
  nns = rep(0, nrow(eps))

  for (n in seq_len(config$n.epochs)) {
    ## set the learning rate for this epoch
    alpha = config$alpha * (1 - ((n-1)/config$n.epochs))
    
    ## occasional message or output
    if (config$verbose) {
      message.w.date(paste0("epoch: ", n), (n %% config$verbose) == 0)
      if (!is.null(config$save)) {
        embedding.in.progress = t(embedding)
        save(embedding.in.progress, file=paste0(config$save, ".", n, ".Rda"))
      }
    }
    
    ## identify links in graph that require attention, then process those in loop
    adjust = eons<=n
    ihits = which(adjust)
    nns[ihits] = floor((n-eon2s[ihits])/epns[ihits])
    embedding = optimize_epoch(embedding, eps.pairs,
                               as.integer(adjust), nns, abg, alpha)
    
    ## prepare for next epoch
    eons[ihits] = eons[ihits] + eps.val[ihits]
    eon2s[ihits] = eon2s[ihits] + (nns[ihits]*epns[ihits])    
  } 
  
  t(embedding)
}




##' create a simplicial set from a distance object
##'
##' @keywords internal
##' @param knn list with inform about nearest neighbors (output of knn.info)
##' @param config list with settings
##'
##' @return matrix 
naive.fuzzy.simplicial.set = function(knn, config) {
  
  ## prepare constants
  V = nrow(knn$indexes)
  mix.ratio = config$set.op.mix.ratio
  bandwidth = config$bandwidth
  nk = config$n.neighbors
  connectivity = config$local.connectivity

  ## construct a smooth map to non-integer neighbors
  n.smooth = smooth.knn.dist(knn$distance, nk,
                             local.connectivity=connectivity,
                             bandwidth=bandwidth)

  ## construct a weighted connectivity matrix (manually)
  conn = base::vector("list", V)
  for (i in 1:V) {
    conn.i = rep(0, nk)
    nearest.i = n.smooth$nearest[i]
    smooth.i = n.smooth$distances[i]
    coo.i = cbind(from=i, to=knn$indexes[i,], value=0)
    for (j in 1:nk) {
      distance.ij = knn$distances[i,j]
      if (knn$indexes[i,j]==i) {
        val = 0
      } else if (distance.ij-nearest.i<=0) {
        val = 1
      } else {
        val = exp(-(distance.ij-nearest.i)/(smooth.i*bandwidth))
      }
      coo.i[j, "value"] = val
    }
    conn[[i]] = coo.i
  }
  conn = do.call(rbind, conn)
  conn = list(coo=conn, names=rownames(knn$indexes), n.elements=V)
  class(conn) = "coo"
  
  connT = t.coo(conn)
  ## here product is indended as a element-by-element product
  connP = multiply.coo(conn, connT)
  
  ## formula with standard matrix objects:
  ##result = mix.ratio*(conn+connT-connP) + (1-mix.ratio)*(connP)
  result = add.coo(add.coo(conn, connT), connP, b=-1)
  result = add.coo(result, connP, a=mix.ratio, b=(1-mix.ratio))
  
  reduce.coo(result)
}




##' compute a "smooth" distance to the kth neighbor and approximate first neighbor
##'
##' @keywords internal
##' @param k.dist matrix with distances to k neighbors
##' @param neighbors numeric, number of neighbors to approximate for
##' @param iterations integers, number of iterations
##' @param local.connectivity iteger
##' @param bandwidth numeric
##' @param tolerance numeric, for numeric precision
##' @param min.dist.scale numeric, minimum distance to nearest neighbor (for display)
##'
##' @return list with two vectors, distances to the kth neighbor,
##' and distance to the first nearest neighbor 
smooth.knn.dist = function(k.dist, neighbors,
                           iterations=64,
                           local.connectivity=1,
                           bandwidth=1,
                           tolerance = 1e-5,
                           min.dist.scale=1e-3) {
  
  target = log2(neighbors)*bandwidth
  result = rho = rep(0, nrow(k.dist))
  
  ## for numeric interpolation
  local.int = floor(local.connectivity)
  interpolation = local.connectivity-local.int
  
  ## precompute constants used within loop
  k.dist.mean = mean(k.dist)
  k.dist.cols = 1:ncol(k.dist)
  
  for (i in 1:nrow(k.dist)) {
    ## identify distances to ith point
    i.dist = k.dist[i,]
    i.nonzero = i.dist[i.dist!=0]
    i.num.nonzero = length(i.nonzero)
    if (i.num.nonzero>local.connectivity) {
      if (local.int>0) {
        rho[i] = i.nonzero[local.int] +
          interpolation*(i.nonzero[local.int+1]-i.nonzero[local.int])
      } else {
        rho[i] = interpolation*i.nonzero[1]
      }
    } else if (i.num.nonzero>0 & i.num.nonzero<local.connectivity)  {
      rho[i] = max(i.nonzero)
    } 

    lo = 0
    hi = Inf
    mid = 1
    
    ## iterate to get a normalization factor
    for (n in 2:iterations) {
      val = sapply(k.dist[i,2:ncol(k.dist)]-rho[i], max, 0)
      val = sum(exp(-val/mid))
      if (abs(val-target)<tolerance) {
        break
      }
      if (val>target) {
        hi = mid
        mid = mean(c(lo, hi))
      } else {
        lo = mid
        if (!is.finite(hi)) {
          mid = mid*2
        } else {
          mid = mean(c(lo, hi))
        }
      }
    }
    result[i] = mid
    
    if (rho[i]>0) {
      result[i] = max(result[i], min.dist.scale*mean(i.dist))
    } else {
      result[i] = max(result[i], min.dist.scale*k.dist.mean)
    }
  }
  
  list(distances=result, nearest=rho)
}

