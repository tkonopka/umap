## test for creating embeddings

cat("\ntest_embedding\n")




## ############################################################################
## Tests for function make.initial.embedding


i4 = iris[, grep("Sepal|Petal", colnames(iris))]
i4m = as.matrix(dist(i4))


test_that("random initial embedding", {
  result = make.initial.embedding(nrow(i4m), umap.defaults)
  expect_equal(dim(result), c(nrow(i4m), umap.defaults$n.components))
})


test_that("force initial embedding", {
  config = umap.defaults
  config$init = matrix(1:4, ncol=4, nrow=nrow(i4m))
  config$n.components = 4  
  result = make.initial.embedding(nrow(i4m), config)
  expect_equal(result, config$init)
})


test_that("report bad manual initial embeddings", {  
  config = umap.defaults
  config$n.components = 2
  V = nrow(i4m)
  ## report if columns don't match
  config$init = matrix(0, ncol=3, nrow=nrow(i4m))
  expect_error(make.initial.embedding(V, config))
  ## report if rows don't match
  config$init = matrix(0, ncol=2, nrow=nrow(i4m)+2)
  expect_error(make.initial.embedding(V, config))
  ## pass if all is well
  config$init = matrix(0, ncol=2, nrow=nrow(i4m))
  expect_silent(make.initial.embedding(V, config))
})

