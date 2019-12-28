## test for creating embeddings

cat("\ntest_embedding\n")




## ############################################################################
## Tests for function make.initial.embedding


i4 = iris[, grep("Sepal|Petal", colnames(iris))]
i4m = as.matrix(dist(i4))

# check that all values in x are within
all.within.range = function(x, lim=c(-10, 10)) {
  all(x >= lim[1] & x <= lim[2])
}
mean.within.range = function(x, lim=c(-10, 10)) {
  xmean = mean(x)
  xmean >= lim[1] & xmean<= lim[2]
}


test_that("random initial embedding", {
  conf = umap.defaults
  conf$init = "random"
  VV = 20
  result = make.initial.embedding(VV, conf)
  expect_equal(dim(result), c(VV, umap.defaults$n_components))
})


test_that("force initial embedding", {
  conf = umap.defaults
  VV = 20
  conf$init = matrix(1:4, ncol=4, nrow=VV)
  conf$n_components = 4  
  result = make.initial.embedding(VV, conf)
  expect_equal(result, conf$init)
})


test_that("report bad manual initial embeddings", {  
  conf = umap.defaults
  conf$n_components = 2
  VV = nrow(i4m)
  ## report if columns don't match
  conf$init = matrix(0, ncol=3, nrow=nrow(i4m))
  expect_error(make.initial.embedding(VV, conf))
  ## report if rows don't match
  conf$init = matrix(0, ncol=2, nrow=nrow(i4m)+2)
  expect_error(make.initial.embedding(VV, conf))
  ## pass if all is well
  conf$init = matrix(0, ncol=2, nrow=nrow(i4m))
  expect_silent(make.initial.embedding(VV, conf))
})


test_that("spectral embedding can revert to random", {
  ## small dataset with two disjoint components
  ## spectral embedding should focus on one connected components
  ## each componet is 3 elements, so should complain about size
  mm = matrix(0, ncol=2, nrow=6)
  mm[1, ] = c(0,0)
  mm[2,] = c(1,0)
  mm[3,] = c(0,1)
  mm[4,] = c(100, 100)
  mm[5,] = c(100, 101)
  mm[6,] = c(101, 100)
  mmdist = as.matrix(dist(mm))
  
  ## prepare a graph 
  config = umap.defaults
  config$init = "spectral"
  config$input = "dist"
  config$n_neighbors = 2
  config[c("a", "b")] = find.ab.params(config$spread, config$min.dist)
  knn =  knn.info(mmdist, config)
  graph = naive.fuzzy.simplicial.set(knn, config)
  
  ## should get warning that spectral embedding is abandoned
  expect_warning(make.initial.embedding(graph$n.elements, config, graph),"random")
})


test_that("spectral embedding with multiple components (2D)", {
  # small dataset with three disjoint components
  # components are of sizes 6,5,4 points
  # spectral embedding should produce three layouts and stitch them together
  mm = matrix(0, ncol=2, nrow=15)
  mm[1:6, 1:2] = c(0,0,1,1,0.5,0.5,  0,1,0,1,0,1)
  mm[7:11, 1:2] = c(0,0,1,1,0.5, 0,1,0,1,0) + 100
  mm[12:15, 1:2] = c(0,0,1,1, 0,1,0,1) + 10000
  mmdist = as.matrix(dist(mm))

  ## prepare a graph 
  config = umap.defaults
  config$n_components = 2
  config$init = "spectral"
  config$input = "dist"
  config$n_neighbors = 3
  config[c("a", "b")] = find.ab.params(config$spread, config$min.dist)
  knn =  knn.info(mmdist, config)
  graph = naive.fuzzy.simplicial.set(knn, config)

  result = make.initial.embedding(graph$n.elements, config, graph)
  expect_equal(dim(result), c(15, 2))
  
  # first group, which is the biggest group, should be centered around (0, 0)
  expect_true(mean.within.range(result[1:6, 1], c(-10, 10)))
  expect_true(mean.within.range(result[1:6, 2], c(-10, 10)))
  # second group should be around (20, 0)
  expect_true(mean.within.range(result[7:11, 1], c(10, 30)))
  expect_true(mean.within.range(result[7:11, 2], c(-10, 10)))
  # third group, which is the smallest group, should be centered around (0, 20)
  expect_true(mean.within.range(result[12:15, 1], c(-10, 10)))
  expect_true(mean.within.range(result[12:15, 2], c(10, 30)))
})
  

test_that("spectral embedding with multiple components (1D)", {
  # small dataset with three disjoint components
  # components are of sizes 6,5,4 points
  # spectral embedding should produce three layouts and stitch them together
  mm = matrix(0, ncol=2, nrow=15)
  mm[1:6, 1:2] = c(0,0,1,1,0.5,0.5, 0,1,0,1,0,1)
  mm[7:11, 1:2] = c(0,0,1,1,0.5, 0,1,0,1,0) + 100
  mm[12:15, 1:2] = c(0,0,1,1, 0,1,0,1) + 10000
  mmdist = as.matrix(dist(mm))

  ## prepare a graph 
  config = umap.defaults
  config$n_components = 1
  config$init = "spectral"
  config$input = "dist"
  config$n_neighbors = 3
  config[c("a", "b")] = find.ab.params(config$spread, config$min.dist)
  knn =  knn.info(mmdist, config)
  graph = naive.fuzzy.simplicial.set(knn, config)

  result = make.initial.embedding(graph$n.elements, config, graph)
  expect_equal(dim(result), c(15, 1))

  # first group, which is the biggest group, should be centered around (0)
  expect_true(mean.within.range(result[1:6, 1], c(-10, 10)))
  # second group should be around (20)
  expect_true(mean.within.range(result[7:11, 1], c(10, 30)))
  # third group, which is the smallest group, should be centered around (40)
  expect_true(mean.within.range(result[12:15, 1], c(30, 50)))
})
