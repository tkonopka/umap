## package umap
## a from-scratch implementation of UMAP algorithm


##' create an embedding 
##'
##' @param d distance object
##' @param config list with settings
##'
##' @export
umap.naive = function(d, config) {

  if (config$verbose) {
    cat(paste(Sys.time(), "\tcomputing a,b\n"))
  }
  if (is.na(config$a) | is.na(config$b)) {
    config[c("a", "b")] = find.ab.params(config$spread, config$min.dist)
  }
  
  ## create a graph representation
  if (config$verbose) {
    cat(paste(Sys.time(), "\tcreating graph\n"))
  }
  graph = naive.fuzzy.simplicial.set(d, config)
  if (config$verbose) {
    cat(paste(Sys.time(), "\tcreating embedding\n"))
  }
  embedding = naive.simplicial.set.embedding(graph, config)

  if (config$verbose) {
    cat(paste(Sys.time(), "\tdone\n"))
  }
  
  embedding
}




## ############################################################################
## Details of the implementation specific to umap.naive


##' create an embedding of graph into a low-dimensional space
##'
##' @param g matrix, graph connectivity as coo
##' @param config list with settings
##'
##' @return matrix with embedding,
##' nrows is from g, ncols determined from config
naive.simplicial.set.embedding = function(g, config) {

  ## create an initial embedding
  result = make.initial.embedding(g$n.elements, config)
  
  total.weight = sum(g$coo[, "value"])
  
  ## simplify graph a little bit
  gmax = max(g$coo[, "value"])
  g$coo[g$coo[, "value"] < gmax/config$n.epochs, "value"] = 0
  ## convert to a graph coo
  g = reduce.coo(g)
  
  ## create an epochs-per-sample. Keep track of it together with the graph coo
  eps = cbind(g$coo,
              eps=make.epochs.per.sample(g$coo[, "value"], config$n.epochs))
  
  result = naive.optimize.embedding(result, config, eps)
  
  result
}




##' modify an existing embedding 
##' 
##' @param embedding matrix holding an initial embedding
##' @param config list with settings
##' @param eps matrix with connectivity coo graph and epochs per sample;
##' this must be a matrix with columns "from", "to", "value", and "eps"
##'
##' @return matrix of same dimension as initial embedding
naive.optimize.embedding = function(embedding, config, eps) {
  
  ## embedding dimension
  V = nrow(embedding)
  
  ## extract some variables from config
  a = config$a
  b = config$b
  gamma = config$gamma
  ## precompute some combinations
  bm1 = b-1
  m2ab = -2*a*b
  p2gb = 2*gamma*b
  
  ## helpers, compute gradients using a squared-distance
  gradcoeff1 = function(d2) {
    ##(-2*a*b*(codist2^bm1)) / (a*(codist2^b)+1)
    (m2ab*(d2^bm1)) / (a*(d2^b)+1)
  }
  gradcoeff2 = function(d2) {
    ##(2*gamma*b) / ((0.001+codist2)*(a*(codist2^b)+1))
    (p2gb) / ((0.001+d2)*(a*(d2^b)+1))
  }
  
  ## epns is short for "epochs per negative sample"
  epns = eps[, "eps"]/config$negative.sample.rate
  ## eon2s is short for "epochs of next negative sample"
  eon2s = epns
  ## eons is short for "epochs of next sample"
  eons = eps[, "eps"]
  
  for (n in 1:config$n.epochs) {
    ## set the learning rate for this epoch
    alpha = config$alpha * (1 - ((n-1)/config$n.epochs))
    if (config$verbose) {
      cat(paste(Sys.time(), "\tepoch ", n, "\n"))
    }
    
    ## identify links in graph that require attention, then process those in loop
    ihits = which(eons<=n)
    for (i in ihits) {
      ## extract details of graph link
      j = eps[i, "from"]
      k = eps[i, "to"]
      ieps = eps[i, "eps"]
      
      ## extract two points to work with
      current = embedding[j,]
      other = embedding[k,]
      codiff = current-other

      ## adjust those points, based on their distance
      codist2 = eucmag2(codiff)
      gradcoeff = gradcoeff1(codist2)
      ##grad.d = alpha*clip4(gradcoeff*codiff)
      grad.d = c4(codiff, gradcoeff, alpha)
      ##embedding[j,] = current = current + grad.d
      current = current + grad.d
      embedding[k,] = other - grad.d
      
      ## prepare for next epoch
      eons[i] = eons[i]+ieps

      ## corrent the current point based on a set of other randomly selected points
      nns = floor((n-eon2s[i])/epns[i])
      nns.random = 1+floor(runif(nns, 1, V))
      for (k in nns.random) {
        other = embedding[k,]
        codiff = current-other
        codist2 = eucmag2(codiff)
        gradcoeff = gradcoeff2(codist2)
        ## original code had this check here, but is this really necessary?
        ##if (!is.finite(gradcoeff)) {
        ##  gradcoeff = 4
        ##}        
        ##grad.d = alpha*clip4(gradcoeff*codiff)
        grad.d = c4(codiff, gradcoeff, alpha)
        current = current + grad.d
      }
      embedding[j,] = current
      
      ## prepare for next epoch
      eon2s[i] = eon2s[i] + (nns*epns[i])
      
    } ## finished processing j/k connections
  } ## finished loop over epochs
  
  embedding
}




##' create a simplicial set from a distance object
##'
##' @param d distance object
##' @param config list with settings
##'
##' @return matrix 
naive.fuzzy.simplicial.set = function(d, config) {

  if (class(d)!="matrix") {
    d = as.matrix(d)
  }

  ## prepare constants
  V = nrow(d)
  mix.ratio = config$set.op.mix.ratio
  bandwidth = config$bandwidth
  nk = config$n.neighbors
  connectivity = config$local.connectivity

  if (config$verbose) {
    cat(paste(Sys.time(), "\tcomputing kinfo\n"))
  }
  
  ## extract neighbor information
  n.info = knn.info(d, config)
  ## construct a smooth map to non-integer neighbors
  n.smooth = smooth.knn.dist(n.info$distance, nk,
                             local.connectivity=connectivity,
                             bandwidth=bandwidth)

  if (config$verbose) {
    cat(paste(Sys.time(), "\tcomputing connectivity coo\n"))
  }

  ## construct a weighted connectivity matrix (manually)
  conn = base::vector("list", V)
  for (i in 1:V) {
    conn.i = rep(0, nk)
    nearest.i = n.smooth$nearest[i]
    smooth.i = n.smooth$distances[i]
    coo.i = cbind(from=i, to=n.info$indexes[i,], value=0)
    for (j in 1:nk) {
      distance.ij = n.info$distances[i,j]
      if (n.info$indexes[i,j]==i) {
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
  conn = list(coo=conn, names=rownames(d), n.elements=V)
  class(conn) = "coo"
  
  if (config$verbose) {
    cat(paste(Sys.time(), "\tmatrix computations\n"))
  }
  
  connT = t.coo(conn)
  ## here product is really indended as a element-by-element product
  ## not matrix multiplication
  connP = multiply.coo(conn, connT)
  
  result = add.coo(add.coo(conn, connT), connP, b=-1)
  result = add.coo(result, connP, a=mix.ratio, b=(1-mix.ratio))
  ##result = mix.ratio*(conn+connT-connP) + (1-mix.ratio)*(connP)
  
  reduce.coo(result)
}




##' compute a
##'
##' @param k.dist matrix with distances to k neighbors
##' @param neighbors numeric, number of neighbors to approximate for
##' @param iterations integers, number of iterations
##' @param local.connectivity iteger
##' @param bandwidth numeric
##' @param tolerance numeric, for numeric precision
##' @param min.dist.scale numeric, minimum distance to nearest neighbor (for display)
##'
##' @return list with two vectors, distances to the kth neighbor,
##' and distnace to the first nearest neighbor 
smooth.knn.dist = function(k.dist, neighbors,
                           iterations=64,
                           local.connectivity=1,
                           bandwidth=1,
                           tolerance = 1e-5,
                           min.dist.scale=1e-3) {
  
  if (class(k.dist)!="matrix") {
    stop("k.dist must be a matrix\n")
  }
  if (local.connectivity<=0){
    stop("local.connectivity must be positive\n")
  }
  
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


