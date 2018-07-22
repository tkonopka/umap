## tests for universal functions (umap_universal.R)

cat("\ntest_knn\n")
source("synthetic.R")
source("train_test.R")


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
## Tests for well-formed output


test_that("knn.from.data should preserve rownames", {
  result = knn.from.data(t(syn0), 3, mdEuclidean)
  expect_equal(rownames(syn0), rownames(result$indexes))
  expect_equal(rownames(syn0), rownames(result$distances))
})


test_that("knn.from.data should preserve rownames", {
  syn0dist = as.matrix(syn0.dist)
  result = knn.from.dist(syn0dist, 3)
  expect_equal(rownames(syn0), rownames(result$indexes))
  expect_equal(rownames(syn0), rownames(result$distances))  
})


test_that("knn.info (brute force) should preserve rownames", {
  conf = umap.defaults
  conf$n_neighbors = 4
  conf$metric.function = mdEuclidean
  result = knn.info(syn0, conf, brute.force=TRUE)
  expected.rownames = c(rownames(result$indexes), rownames(result$distances))
  result.rownames = c(rownames(syn0), rownames(syn0))
  expect_equal(result.rownames, expected.rownames)
})


test_that("knn.info (from data) should preserve rownames", {
  conf = umap.defaults
  conf$n_neighbors = 4
  conf$metric.function = mdEuclidean
  result = knn.info(syn0, conf, brute.force=FALSE)
  expected.rownames = c(rownames(result$indexes), rownames(result$distances))
  result.rownames = c(rownames(syn0), rownames(syn0))
  expect_equal(result.rownames, expected.rownames)
})




## ############################################################################
## Tests for approximate nearest neighbors extraction


test_that("knn from data complains when k is too large or too small", {
  ## da is a dummy matrix
  da = matrix(0, ncol=5, nrow=5)
  expect_error(knn.from.data(da, 6, mdEuclidean))
  expect_error(knn.from.data(da, 0, mdEuclidean))
  expect_silent(knn.from.data(da, 3, mdEuclidean))
})


test_that("knn from data complains about k (large dataset)", {
  ## da is a dummy, but large, matrix 
  da = matrix(0, ncol=5, nrow=5000)
  expect_error(knn.from.data(da, 6000, mdEuclidean))
  expect_error(knn.from.data(da, 0, mdEuclidean))
  expect_error(knn.from.data(da, -0.5, mdEuclidean))
})


test_that("knn from data complains about subsampling (large dataset)", {
  ## da is a dummy, but large, matrix 
  da = matrix(0, nrow=20, ncol=300)
  expect_error(knn.from.data(da, 5, mdEuclidean, subsample.k=NA), "subsample")
  expect_error(knn.from.data(da, 5, mdEuclidean, subsample.k=-0.2), "subsample")
  expect_error(knn.from.data(da, 5, mdEuclidean, subsample.k=600), "subsample")
  expect_silent(knn.from.data(da, 6, mdEuclidean, subsample.k=0.3))
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
  
  result = knn.from.data(t(dd), 3, mdEuclidean)
  expect_equal(dim(result$indexes), c(10, 3))
  expect_equal(dim(result$distances), c(10, 3))
})


test_that("knn for large dataset queries a small number of distances", {
  dlarge = matrix(0, ncol=2, nrow=300)
  dlarge[,1] = runif(nrow(dlarge), -2, 2)
  dlarge[,2] = runif(nrow(dlarge), -2, 2)
  result.dist = knn.from.dist(dist(dlarge), 4)
  result = knn.from.data.reps(dlarge, 4, mdEuclidean, subsample=4, reps=1)
  result.reps = knn.from.data.reps(dlarge, 4, mdEuclidean, subsample=4, reps=2)
  
  ## all self distances are zero
  expect_equal(sum(result$distances[,1]), 0)
  ## distance to nearest neighbor should be small
  expect_lt(mean(result$distances[,2]), 0.4)
  ## distances in two-reps are smaller
  totdist1 = sum(apply(result$distances, 1, sum))
  totdist2 = sum(apply(result.reps$distances, 2, sum))
  expect_lt(totdist2, totdist1)
})




## ############################################################################
## Tests with degenerate neighbors


test_that("knn works with degenerate neighbors", {
  ## syn1 has so many same-location points, that first nearest neighbor
  ## should always be at distance 0
  result = knn.from.data(t(syn1[, 1:2]), 4, mdEuclidean)
  expect_lt(mean(result$distances[,2]), 0.01)
})




## ############################################################################
## Tests with spectators


test_that("knn from data links spectators to primary data", {
  ## create merged dataset with observations in columns
  dT = cbind(t(i.train), t(i.test))
  num.primary = nrow(i.train)
  result = knn.from.data(dT, 6, mdEuclidean, fix.observations=num.primary)
  ## result should have indexes and distance components
  expect_true(all(c("indexes", "distances") %in% names(result)))
  ## indexes should show all observation link to themselves
  expect_equivalent(result$indexes[,1], 1:nrow(result$indexes))
  ## indexes to further neighbors should only link to primary data
  expect_lt(max(result$indexes[, 2:6]), num.primary+1)
})


train.size = nrow(i.train)/3
test.size = nrow(i.test)/3


test_that("spectator.info by brute force links spectators to primary data", {
  conf = umap.check.config(umap.defaults)
  conf$n_neighbors = 5
  result = spectator.knn.info(i.test, i.train, conf, brute.force=TRUE)
  ## check output sizes
  expect_equal(dim(result$indexes), c(nrow(i.test), conf$n_neighbors))
  expect_equal(dim(result$distances), c(nrow(i.test), conf$n_neighbors))
  expect_equal(rownames(result$distances), rownames(i.test))
  expect_equal(rownames(result$indexes), rownames(i.test))
  ## ditances of first neighbor must be larger than zero
  expect_equal(mean(result$distances[,1]), 0)
  ## entries in i.test should connect to items in i4, which are more numerous than i.test
  indexes = result$indexes[, 2:ncol(result$indexes)]
  expect_gt(max(indexes), nrow(i.test))
  ## the first 5 items in i.test must be neighbors to the first 15 indexes in i4, etc.
  expect_lt(mean(indexes[1:test.size,]), train.size)
  expect_lt(mean(indexes[test.size+(1:test.size),]), 2*train.size)
  expect_gt(mean(indexes[2*test.size+(1:test.size),]), 2*train.size)
})


test_that("spectator.info by stochastic method links spectators to primary data", {
  conf = umap.check.config(umap.defaults)
  conf$n_neighbors = 5
  result = spectator.knn.info(i.test, i.train, conf, brute.force=FALSE)
  ## check output sizes
  expect_equal(dim(result$indexes), c(nrow(i.test), conf$n_neighbors))
  expect_equal(dim(result$distances), c(nrow(i.test), conf$n_neighbors))
  expect_equal(rownames(result$distances), rownames(i.test))
  expect_equal(rownames(result$indexes), rownames(i.test))
  ## ditances of first neighbor must be larger than zero
  expect_equal(mean(result$distances[,1]), 0)
  ## entries in i.test should connect to items in i4, which are more numerous than i.test
  indexes = result$indexes[, 2:ncol(result$indexes)]
  expect_gt(max(indexes), nrow(i.test))
  ## the first 5 items in i.test must be neighbors to the first 15 indexes in i4, etc.
  expect_lt(mean(indexes[1:test.size,]), train.size)
  expect_lt(mean(indexes[test.size+(1:test.size),]), 2*train.size)
  expect_gt(mean(indexes[2*test.size+(1:test.size),]), 2*train.size)
})

