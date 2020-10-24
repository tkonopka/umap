# package umap
# a from-scratch implementation of the UMAP algorithm
#
# This implementation is called naive because it is a rather direct translation
# of the original implementation by Leland McInnes. This implementation does,
# however, have some slight modifications.
#
# The original implementation is available at https://github.com/lmcinnes/umap.
#
# The original implementation was published under a BSD license.
#


#' Create a umap embedding
#'
#' This implementation is called naive because it is a rather straightforward
#' translation of the original python code.
#'
#' @keywords internal
#' @noRd
#' @param d data object
#' @param config list with settings
#' @importFrom stats runif
#' 
#' @return list, one element of which is matrix with embedding coordinates
umap.naive = function(d, config) {
  
  verbose = config$verbose
  
  # prep parameters
  message.w.date("starting umap", verbose)
  if (is.na(config$a) | is.na(config$b)) {
    config[c("a", "b")] = find.ab.params(config$spread, config$min_dist)
  }
  if (is.na(config$random_state)) {
    config$random_state= as.integer(runif(1, 0, 2^30))
  }
  
  # perhaps extract knn from input
  knn = NULL
  if ("knn" %in% names(config)) {
    if (is(config$knn, "umap.knn")) {
      message.w.date("using supplied nearest neighbors", verbose)
      knn = config$knn
    }
  }
  if (is.null(knn)) {
    message.w.date("creating graph of nearest neighbors", verbose)
    knn =  knn.info(d, config)
  }
  
  # create a graph representation
  graph = naive.fuzzy.simplicial.set(knn, config)
  message.w.date("creating initial embedding", verbose)
  embedding = make.initial.embedding(graph$n.elements, config, graph)
  message.w.date("optimizing embedding", verbose)
  embedding = naive.simplicial.set.embedding(graph, embedding, config)
  embedding = center.embedding(embedding)
  message.w.date("done", verbose)
  
  list(layout=embedding, data=d, knn=knn, config=config)
}




#' predict embedding of new data given an existing umap object
#'
#' @keywords internal
#' @noRd
#' @param umap object of class umap
#' @param data matrix with new data
#'
#' @return matrix with embedding coordinates
umap.naive.predict = function(umap, data) {

  # check that data and knn are available
  missing = setdiff(c("knn", "data", "config"), names(umap))
  if (length(missing)>0) {
    umap.error("missing components: ", paste(missing, collapse=", "))
  }

  # create a configuration for the prediction
  config = umap$config
  config$n_epochs = floor(config$n_epochs/3)
  V = nrow(umap$layout)
  verbose = umap$config$verbose
  
  # obtain nearest neighbors
  message.w.date("creating graph of nearest neighbors", verbose)
  spectator.knn = spectator.knn.info(data, umap$data, config)
  knn = umap.knn(rbind(umap$knn$indexes, spectator.knn$indexes),
                 rbind(umap$knn$distances, spectator.knn$distances))

  # create graph representation of primary and spectator data together
  graph = naive.fuzzy.simplicial.set(knn, config)
  message.w.date("creating initial embedding", verbose)
  embedding = make.initial.spectator.embedding(umap$layout,
                                               spectator.knn$indexes)
  embedding = rbind(umap$layout, embedding)
  message.w.date("optimizing embedding", verbose)
  embedding = naive.simplicial.set.embedding(graph, embedding, config,
                                             fix.observations=V)  
  
  # extract coordinates for just the spectator data
  embedding = embedding[V + seq_len(nrow(data)), , drop=FALSE]
  message.w.date("done", verbose)
  
  embedding
}




# ############################################################################
# Details of the implementation specific to umap.naive


#' create an embedding of graph into a low-dimensional space
#'
#' @keywords internal
#' @noRd
#' @param g matrix, graph connectivity as coo
#' @param embedding matrix, coordinates for an initial graph embedding
#' @param config list with settings
#' @param fix.observations integer, number of points to avoid moving during
#' optimization
#'
#' @return matrix with embedding,
#' nrows is from g, ncols determined from config
naive.simplicial.set.embedding = function(g, embedding, config,
                                          fix.observations=NULL) {
  
  if (config$n_epochs==0) {
    return(embedding)
  }
  
  # create a new matrix with an optimized embedding
  # (here work in transpose mode)
  result = t(embedding)
  
  # simplify graph a little bit
  gmax = max(g$coo[, "value"])
  g$coo[g$coo[, "value"] < gmax/config$n_epochs, "value"] = 0
  g = reduce.coo(g)
  
  # create an epochs-per-sample. Track it together with the graph coo
  eps = cbind(g$coo,
              eps=make.epochs.per.sample(g$coo[, "value"], config$n_epochs))

  # adjust the initial embedding guess
  if (is.null(fix.observations)) {
    # this branch occurs during an initial embedding
    result = naive.optimize.embedding(result, config, eps)
  } else {
    # this branch occurs during prediction
    # (many fixed observations, a few to predict/optimize)
    eps = eps[eps[, "from"]>fix.observations, , drop=FALSE]
    # define the indeces that need optimizing (skips the fixed observations)
    indeces = seq(fix.observations+1, ncol(result))
    seeds = column.seeds(result[, indeces, drop=FALSE],
                         key=config$transform_state)
    # construct temporary embeddings that hold fix.observations plus one
    # "temp" item. The "temp" items will be optimized on its own
    temp.index = fix.observations + 1
    temp.embedding = result[, seq_len(fix.observations+1), drop=FALSE]
    temp.eps = split.data.frame(eps, eps[, "from"])
    for (i in seq_along(indeces)) {
      temp.embedding[, temp.index] = result[, indeces[i]]
      set.seed(seeds[i])
      i.eps = temp.eps[[as.character(indeces[i])]]
      if (!is.null(i.eps)) {
        i.eps[, "from"] = temp.index
        temp.result = naive.optimize.embedding(temp.embedding, config, i.eps)
      }
      result[, indeces[i]] = temp.result[, temp.index]
    }
  }
  
  colnames(result) = g$names
  t(result)
}




#' modify an existing embedding 
#' 
#' @keywords internal
#' @noRd
#' @param tembedding matrix, transpose of matrix with an initial embedding
#' @param config list with settings
#' @param eps matrix with connectivity coo graph and epochs per sample;
#' this must be a matrix with columns "from", "to", "value", and "eps"
#'
#' @return matrix of same dimension as initial tembedding
naive.optimize.embedding = function(tembedding, config, eps) {

  # number of vertices in embedding
  V = ncol(tembedding)
  
  # define some vectors for book-keeping
  # integer matrix with pairs of data
  eps.pairs = matrix(as.integer(eps[, c("from", "to")]), ncol=2)-1
  eps.val = eps[, "eps"]
  # epns is short for "epochs per negative sample"
  epns = eps.val/config$negative_sample_rate
  
  # infer if some points should remain fixed
  fix.observations = min(eps.pairs[,1])>1
  # extract some variables from config
  abg = c(config$a, config$b, config$gamma, as.numeric(fix.observations))

  optimize_embedding(tembedding, eps.pairs,
                     eps.val, epns,
                     abg, config$alpha, as.integer(config$n_epochs)) 
}




#' create a simplicial set from a distance object
#'
#' @keywords internal
#' @noRd
#' @param knn list with inform about nearest neighbors (output of knn.info)
#' @param config list with settings
#' 
#' @return matrix 
naive.fuzzy.simplicial.set = function(knn, config) {
  
  # prepare constants
  V = nrow(knn$indexes)
  mix.ratio = config$set_op_mix_ratio
  bandwidth = config$bandwidth
  nk = config$n_neighbors
  connectivity = config$local_connectivity

  # construct a smooth map to non-integer neighbors
  n.smooth = smooth.knn.dist(knn$distance, nk,
                             local.connectivity=connectivity,
                             bandwidth=bandwidth)

  # construct a weighted connectivity matrix (manually)
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
  # here product is indended as a element-by-element product
  connP = multiply.coo(conn, connT)
  
  # formula with standard matrix objects:
  #result = mix.ratio*(conn+connT-connP) + (1-mix.ratio)*(connP)
  result = add.coo(add.coo(conn, connT), connP, b=-1)
  result = add.coo(result, connP, a=mix.ratio, b=(1-mix.ratio))
  
  reduce.coo(result)
}


#' compute a "smooth" distance to the kth neighbor and first neighbor
#'
#' @keywords internal
#' @noRd
#' @param k.dist matrix with distances to k neighbors
#' @param neighbors numeric, number of neighbors to approximate for
#' @param iterations integers, number of iterations
#' @param local.connectivity iteger
#' @param bandwidth numeric
#' @param tolerance numeric, for numeric precision
#' @param min.dist.scale numeric, minimum distance to nearest neighbor
#' (for display)
#'
#' @return list with two vectors, distances to the kth neighbor,
#' and distance to the first nearest neighbor 
smooth.knn.dist = function(k.dist, neighbors,
                           iterations=64,
                           local.connectivity=1,
                           bandwidth=1,
                           tolerance = 1e-5,
                           min.dist.scale=1e-3) {
  
  target = log2(neighbors)*bandwidth
  result = rho = rep(0, nrow(k.dist))
  
  # for numeric interpolation
  local.int = floor(local.connectivity)
  interpolation = local.connectivity-local.int
  
  # precompute constants used within loop
  k.dist.mean = mean(k.dist)
  k.dist.cols = seq_len(ncol(k.dist))
  
  for (i in seq_len(nrow(k.dist))) {
    # identify distances to ith point
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
    
    # iterate to get a normalization factor
    for (n in 2:iterations) {
      val = pmax(k.dist[i, 2:ncol(k.dist)]-rho[i], 0)
      val = sum(exp(-val/mid))
      if (abs(val-target)<tolerance) {
        break
      }
      if (val>target) {
        hi = mid
        mid = (lo+hi)/2
      } else {
        lo = mid
        if (!is.finite(hi)) {
          mid = mid*2
        } else {
          mid = (lo + hi)/2
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

