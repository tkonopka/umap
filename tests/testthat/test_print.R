## tests for output of print functions

cat("\ntest_print\n")


## ############################################################################
## Tests for printing config objects


test_that("print complains on non-classed input", {
  expect_error(print.umap.config(1:4))
})


test_that("print of config produces output", {
  conf = umap.defaults
  ## print should display a welcome message
  expect_message(print(conf), "umap")
  ## print should display up-to-date content in the config object
  conf$seed = 1234567
  expect_message(print(conf), "1234567")
})




## ############################################################################
## Tests for printing umap objects


test_that("print complains with non-umap input", {
  expect_error(print.umap(1:4))
})

test_that("print display placeholder instead of matrix", {
  conf = umap.defaults
  conf$init = matrix(0, ncol=2, nrow=4)
  expect_message(print(conf), "matrix")
})




## ############################################################################
## Tests for printing knn information

test_that("print complains with wrong input", {
  expect_error(print.umap.knn(1:5))
})


test_that("print displays summary of umap result", {
  mat = matrix(1:36, ncol=2)
  ## create a fast umap result
  result = umap(mat, n_epochs=2, n_neighbors=3)
  ## check output for overall object
  expect_message(print(result), "umap")
  ## check display of knn information
  expect_message(print(result$knn), "k=3")
  expect_message(print(result$knn), "approximate")
})

