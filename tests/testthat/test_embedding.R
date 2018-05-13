## test for creating embeddings

cat("\ntest_embedding\n")




## ############################################################################
## Tests for function make.initial.embedding


i4 = iris[, grep("Sepal|Petal", colnames(iris))]
i4m = as.matrix(dist(i4))


test_that("random initial embedding", {
  conf = umap.defaults
  conf$init = "random"
  result = make.initial.embedding(nrow(i4m), conf)
  expect_equal(dim(result), c(nrow(i4m), umap.defaults$n.components))
})


test_that("force initial embedding", {
  conf = umap.defaults
  conf$init = matrix(1:4, ncol=4, nrow=nrow(i4m))
  conf$n.components = 4  
  result = make.initial.embedding(nrow(i4m), conf)
  expect_equal(result, conf$init)
})


test_that("report bad manual initial embeddings", {  
  conf = umap.defaults
  conf$n.components = 2
  V = nrow(i4m)
  ## report if columns don't match
  conf$init = matrix(0, ncol=3, nrow=nrow(i4m))
  expect_error(make.initial.embedding(V, conf))
  ## report if rows don't match
  conf$init = matrix(0, ncol=2, nrow=nrow(i4m)+2)
  expect_error(make.initial.embedding(V, conf))
  ## pass if all is well
  conf$init = matrix(0, ncol=2, nrow=nrow(i4m))
  ##expect_silent(make.initial.embedding(V, conf))
})

