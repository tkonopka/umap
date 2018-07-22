## tests for universal functions (umap_universal.R)

cat("\ntest_universal\n")


## ############################################################################
## Tests for smoothing of knns


d2 = matrix(c(1.1, 1.2, 1.3, 1.4, 0, 0, 0.5, 0.8), nrow=2, ncol=4, byrow=T)

test_that("smoothing, k2", {
  ## k=2 (expected from python iplementation)
  result2 = smooth.knn.dist(d2, 2)
  expected2 = list(distances=c(0.16410065, 0.000325), nearest=c(1.1, 0.5))
  expect_equal(result2, expected2, tolerance=1e-6)
})


test_that("smoothing, k3", {
  ## k=3 (expected from python implementation)
  result3 = smooth.knn.dist(d2, 3)
  expected3 = list(distances=c(0.2959671, 0.000325), nearest=c(1.1, 0.5))
  expect_equal(result3, expected3, tolerance=1e-6)
})


test_that("smoothing, local 0.5", {
  ## k=2, local.connectivity 0.5 (expected from python implementation)
  result.local05 = smooth.knn.dist(d2, 2, local.connectivity=0.5)
  expected.local05 = list(distances=c(0.67821503, 0.015625), nearest=c(0.55, 0.25))
  expect_equal(result.local05, expected.local05, tolerance=1e-6)
})


test_that("smoothing, local 3", {
  ## k=2, local connectivity 3 (expected from python implementation)
  result.local3 = smooth.knn.dist(d2, 2, local.connectivity=3)
  expected.local3 = list(distances=c(0.00125, 0.000325), nearest=c(1.3, 0.8))
  expect_equal(result.local3, expected.local3, tolerance=1e-6)
})




## ############################################################################
## Tests for estimating a/b parameters


test_that("param ab using search 1", {
  ## default has spread 1 and min dist 0.1
  result = find.ab.params(1, 0.1)
  expected = c(a=1.57694346041, b=0.895060878123)
  expect_equal(expected, result, tolerance=1e-4)
})


test_that("param ab using search 2", {
  ## another combination of spread and min.dist
  result = find.ab.params(2, 0.1)
  expected = c(a=0.544660540037, b=0.84205542675)
  expect_equal(expected, result, tolerance=1e-4)
})


test_that("param ab using search 3", {
  ## another combination of spread and min.dist
  result = find.ab.params(0.6, 0.2)
  expected = c(a=2.95031796402, b=1.14888917655)
  expect_equal(expected, result, tolerance=1e-4)
})




## ############################################################################
## Tests for estimating epochs per sample


test_that("epochs per sample with uniform weights", {
  result = make.epochs.per.sample(rep(1, 5), 4)
  expected = c(1, 1, 1, 1, 1)
  expect_equal(expected, result)
})


test_that("epochs per sample for a matrix", {
  ## this is some made-up matrix of numbers
  g = matrix(0, ncol=3, nrow=3)
  g[1,] = c(0.5, 0.5, 0.4)
  g[2,] = c(1.0, 0.8, 0.3)
  g[3,] = c(0.8, 1.1, 0)
  g = t(g)
  result = make.epochs.per.sample(g, 5)
  ## expected is computed by python iplementation
  expected = g
  expected[1:9] = c(2.2, 2.2, 2.75,
                    1.1, 1.375, 3.66666667,
                    1.375, 1, -1)
  expect_equal(expected, result)
})




## ############################################################################
## Tests for value clipping


test_that("number clipping with default xmax", {
  expect_equal(clip(2), 2)
  expect_equal(clip(4.2), 4)
  expect_equal(clip(-12), -4)
})


test_that("number clipping with vector", {
  result = clip(c(0,1,4.2, 6, -2, -9))
  expected = c(0, 1, 4, 4, -2, -4)
  expect_equal(expected, result)
})


test_that("number clipping with custom xmax", {
  expect_equal(clip(2, 1), 1)
  expect_equal(clip(4.2, 6), 4.2)
  expect_equal(clip(-12, 3), -3)
})




## ############################################################################
## Tests for matrix centering


test_that("number clipping with default xmax", {
  mat = matrix(1:8, ncol=2, nrow=4)
  rownames(mat) = letters[1:4]
  result = center.embedding(mat)
  expected = mat
  expected[,1] = mat[,1] - mean(mat[,1])
  expected[,2] = mat[,2] - mean(mat[,2])
  expect_equal(result, expected)
})


