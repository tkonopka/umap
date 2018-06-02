## tests using naive method

cat("\ntest_checks.R\n")




## ############################################################################
## Tests for primary input


test_that("input can be matrix or data frame", {
  ## class matrix is preserved
  mat = matrix(0, ncol=2, nrow=3)
  colnames(mat) = c("A", "B")
  mat[,1] = 1:3
  mat[,2] = 11:13
  conf = umap.defaults
  conf$metric.name = "euclidean"
  expect_equal(umap.prep.input(mat, conf), mat)
  ## data frames are converted to matrices
  df = data.frame(A=1:3, B=11:13)
  expect_silent(umap.prep.input(df, conf))
  result = umap.prep.input(df, conf)
  expect_equal(result, mat)
})


test_that("input cannot be non-matrix", {
  conf = umap.defaults
  conf$metric.name = "euclidean"
  expect_error(umap.prep.input(1:4, conf))
  expect_error(umap.prep.input(letters[1:4], conf))
})


test_that("prep centers input for pearson distance", {
  conf = umap.defaults
  conf$metric.name = "pearson"
  mat = matrix(0, ncol=2, nrow=3)
  colnames(mat) = c("A", "B")
  mat[,1] = 1:3
  mat[,2] = 11:13
  expected = mat
  for (i in 1:nrow(mat)) {
    expected[i,] = mat[i,] - mean(mat[i,])
  }
  result = umap.prep.input(mat, conf)
  expect_equal(result, expected)
})




## ############################################################################
## Tests for configuration settings


test_that("checking config detects errors with nearest neighbors", {
  ## detect errors encoded in config
  conf = umap.defaults
  conf$n.neighbors = 0.5
  expect_error(umap.check.config(conf))
  expect_error(umap.check.config(umap.defaults, n.neighbors=0))
})


test_that("config detects input methods", {
  conf = umap.defaults
  conf$input = NA
  expect_error(umap.check.config(conf))
})


test_that("config replaced metric.function by a function", {
  conf = umap.check.config(umap.defaults)
  expect_equal(class(conf$metric.function), "function")
})


test_that("config sets euclidean distance function", {
  d1 = 1:4
  d2 = c(3,5,2,1)
  ## euclidean
  conf = umap.defaults
  conf$metric.function = "euclidean"
  result = umap.check.config(conf)
  expect_equal(dEuclidean(d1, d2), result$metric.function(d1, d2))
  expect_equal(result$metric.name, "euclidean")
})


test_that("config sets pearson distance function", {
  d1 = c(-2,-1,0,1,2)
  d2 = c(-4,2,-2,0,4)
  ## pearson
  conf = umap.defaults
  conf$metric.function = "pearson2"
  result = umap.check.config(conf)
  expect_equal(dCenteredPearson(d1, d2), result$metric.function(d1, d2))
  expect_equal(result$metric.name, "pearson2")
})


test_that("config sets manhattan distance function", {
  d1 = c(-2,-1,0,1,2)
  d2 = c(-4.2,2,-2,0,4.2)
  ## pearson
  conf = umap.defaults
  conf$metric.function = "manhattan"
  result = umap.check.config(conf)
  expect_equal(dManhattan(d1, d2), result$metric.function(d1, d2))
  expect_equal(result$metric.name, "manhattan")
})


test_that("config sets custom metric function", {
  myfun = function(x, y) {
    -1.0
  }
  d1 = c(-2,-1,0,1,2)
  d2 = c(-4.2,2,-2,0,4.2)
  ## pearson
  conf = umap.defaults
  conf$metric.function = myfun
  result = umap.check.config(conf)
  expect_equal(myfun(d1, d2), result$metric.function(d1, d2))
  expect_equal(result$metric.name, "custom")
})


test_that("config reports unknown metric functions", {
  conf = umap.defaults
  conf$metric.function = "badfunction"
  expect_error(umap.check.config(conf))
})


test_that("config checks local connectivity", {
  conf = umap.defaults
  conf$local.connectivity = 0
  expect_error(umap.check.config(conf))
  conf$local.connectivity = -0.2
  expect_error(umap.check.config(conf))
})


test_that("config checks epochs", {
  conf = umap.defaults
  conf$n.epochs = NA
  expect_error(umap.check.config(conf))
  conf$n.epochs = -2
  expect_error(umap.check.config(conf))
})







