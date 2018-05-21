## tests using naive method

cat("\ntest_naive.R\n")
source("synthetic.R")


## create a configuration for testing
conf.testing = umap.defaults
conf.testing$n.neighbors = 5
conf.testing$metric.function = dEuclidean


test_that("synthetic fuzzy simplicial set (from dist)", {
  conf = conf.testing
  conf$input = "dist"
  knn = knn.info(syn0.dist, conf)
  graph = naive.fuzzy.simplicial.set(knn, conf)
  ## top-part of expected graph (computed by python implementation)
  expected.graph = matrix(0, ncol=3, nrow=3)
  expected.graph[1,] = c(1,2, 1)
  expected.graph[2,] = c(1,3, 0.958418847698)
  expected.graph[3,] = c(1,4, 0.204675928724)  
  colnames(expected.graph) = c("from", "to", "value")
  #expected.graph = matrix(0, ncol=4, nrow=4)
  #expected.graph[1,] = c(0,1,0.958418847698, 0.204675928724)
  #expected.graph[2,] = c(1,0,1, 0.439842238909)
  #expected.graph[3,] = c(0.958418847698, 1, 0, 0.508078540941)
  #expected.graph[4,] = c(0.204675928724, 0.439842238909, 0.508078540941, 0)
  ## compoare top parts of graph
  expect_equal(expected.graph, graph$coo[1:3,], tolerance=1e-4)
})


test_that("synthetic fuzzy simplicial set (from data)", {
  conf = conf.testing
  conf$input = "data"
  knn = knn.info(syn0, conf)
  graph = naive.fuzzy.simplicial.set(knn, conf)
  ## top-part of expected graph (computed by python implementation)
  expected.graph = matrix(0, ncol=3, nrow=3)
  expected.graph[1,] = c(1,2, 1)
  expected.graph[2,] = c(1,3, 0.958418847698)
  expected.graph[3,] = c(1,4, 0.204675928724)  
  colnames(expected.graph) = c("from", "to", "value")
  ## compoare top parts of graph
  expect_equal(expected.graph, graph$coo[1:3, ], tolerance=1e-4)
})

