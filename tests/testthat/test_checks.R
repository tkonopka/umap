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
  expect_equal(umap.check.input(mat), mat)
  ## data frames are converted to matrices
  df = data.frame(A=1:3, B=11:13)
  expect_silent(umap.check.input(df))
  result = umap.check.input(df)
  expect_equal(result, mat)
})


test_that("input cannot be non-matrix", {
  expect_error(umap.check.input(1:4))
  expect_error(umap.check.input(letters[1:4]))
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


test_that("checking config detects input methods", {
  conf = umap.defaults
  conf$input = NA
  expect_error(umap.check.config(conf))
})


test_that("checking config replaced metric.function by a function", {
  conf = umap.check.config(umap.defaults)
  expect_equal(class(conf$metric.function), "function")
})


test_that("config complains about unknown functions", {
  conf = umap.defaults
  conf$metric.function = "badfunction"
  expect_error(umap.check.config(conf))
})


test_that("config complains about local connectivity", {
  conf = umap.defaults
  conf$local.connectivity = 0
  expect_error(umap.check.config(conf))
  conf$local.connectivity = -0.2
  expect_error(umap.check.config(conf))
})


test_that("config complains about epochs", {
  conf = umap.defaults
  conf$n.epochs = NA
  expect_error(umap.check.config(conf))
  conf$n.epochs = -2
  expect_error(umap.check.config(conf))
})
