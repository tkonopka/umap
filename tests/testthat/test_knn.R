## tests for universal functions (umap_universal.R)

cat("\ntest_knn\n")
source("synthetic.R")


## ############################################################################
## Tests for exact nearest neighbors extraction


test_that("k nearest neighbors complains when k is too large or too small", {
  ## da is a distance matrix (not necessarily metric)
  da = matrix(0, ncol=5, nrow=5)
  expect_error(knn.from.dist(da, 6))
  expect_error(knn.from.dist(da, 0))
  expect_silent(knn.from.dist(da, 3))
})


test_that("k nearest neighbors information", {
  ## da is a distance matrix (not necessarily metric)
  da = matrix(0, ncol=5, nrow=5)
  ## object 1 - nearest items are 2,3 
  da[1,2] = da[2,1] = 1
  da[1,3] = da[3,1] = 2
  da[1,4] = da[4,1] = 3
  da[1,5] = da[5,1] = 4
  ## object 2 - nearest items are 1,4
  da[2,3] = da[3,2] = 10
  da[2,4] = da[4,2] = 2
  da[2,5] = da[5,2] = 11
  ## object 3 - nearest items are 1,4
  da[3,4] = da[4,3] = 3
  da[3,5] = da[5,3] = 12
  ## object 4 - nearest items are 5,2
  da[4,5] = da[5,4] = 1
  da.dist = as.dist(da)
  
  ## compute knn from matrix and from dist
  result = knn.from.dist(da, 3)
  resultd = knn.from.dist(da.dist, 3)
  ## the results should be numerically equal, but (dist) introduces rownames
  ## check for numerical equality only (not attributes)
  expect_equal(result, resultd, check.attributes=FALSE)

  ## check content of the matrices
  ## these matrices constructed manually by visual inspection of da
  expected.indexes = matrix(c(1,2,3, 2,1,4, 3,1,4, 4,5,2, 5,4,1),
                            byrow=TRUE, nrow=5, ncol=3)
  expected.distances = matrix(c(0,1,2, 0,1,2, 0,2,3, 0,1,2, 0,1,4),
                              byrow=TRUE, nrow=5, ncol=3)
  expect_equal(result$indexes, expected.indexes)
  expect_equal(result$distances, expected.distances)
})




## ############################################################################
## Tests for approximate nearest neighbors extraction


test_that("knn from data complains when k is too large or too small", {
  ## da is a dummy matrix
  da = matrix(0, ncol=5, nrow=5)
  expect_error(knn.from.data(da, 6, dEuclidean))
  expect_error(knn.from.data(da, 0, dEuclidean))
  expect_silent(knn.from.data(da, 3, dEuclidean))
})


test_that("knn from data complains about k (large dataset)", {
  ## da is a dummy, but large, matrix 
  da = matrix(0, ncol=5, nrow=5000)
  expect_error(knn.from.data(da, 6000, dEuclidean))
  expect_error(knn.from.data(da, 0, dEuclidean))
  expect_error(knn.from.data(da, -0.5, dEuclidean))
})


test_that("knn from data complains about subsampling (large dataset)", {
  ## da is a dummy, but large, matrix 
  da = matrix(0, ncol=5, nrow=5000)
  expect_error(knn.from.data(da, 5, dEuclidean, subsample.k=NA))
  expect_error(knn.from.data(da, 5, dEuclidean, subsample.k=-0.2))
  expect_error(knn.from.data(da, 5, dEuclidean, subsample.k=6000))
})


test_that("knn returns a reasonable set of data", {

  ## dd is a matrix with points on a plane
  dd = matrix(0, ncol=2, nrow=10)
  ## cluster 1 with points near (1,1)
  dd[1,] = c(1, 1)
  dd[2,] = c(1, 2)
  dd[3,] = c(2, 1)
  ## cluster with points near (10, 10)
  dd[4,] = c(10,10)
  dd[5,] = c(10,11)
  dd[6,] = c(12,12)
  ## cluster with points near (20, 20)
  dd[7,] = c(20,20)
  dd[8,] = c(20,21)
  dd[9,] = c(21,20)
  dd[10,] = c(21,21)
  
  result = knn.from.data(dd, 3, dEuclidean)
  expect_equal(dim(result$indexes), c(10, 3))
  expect_equal(dim(result$distances), c(10, 3))
})


test_that("knn for large dataset queries a small number of distances", {
  dlarge = matrix(0, ncol=2, nrow=400)
  dlarge[,1] = runif(nrow(dlarge), -2, 2)
  dlarge[,2] = runif(nrow(dlarge), -2, 2)
  result = knn.from.data(dlarge, 4, dEuclidean, subsample=4)
  result.dist = knn.from.dist(dist(dlarge), 4)
  #c(mean(result.dist$distances[,2]), mean(result$distances[,2]))
  #c(mean(result.dist$distances[,3]), mean(result$distances[,3]))
  
  ## all self distances are zero
  expect_equal(sum(result$distances[,1]), 0)
  ## distance to nearest neighbor should be small
  expect_lt(mean(result$distances[,2]), 0.4)
})



## ############################################################################
## Tests with degenerate neighbors


test_that("knn works with degenerate neighbors", {
  ## syn1 has so many same-location points, that first nearest neighbor
  ## should always be at distance 0
  ## NOTE: because of the size of syn1, this test takes around 1s to run
  result = knn.from.data(syn1, 4, dEuclidean)
  expect_equal(mean(result$distances[,2]), 0)
})

