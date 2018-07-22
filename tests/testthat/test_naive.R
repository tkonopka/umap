## tests using naive method

cat("\ntest_naive\n")
source("synthetic.R")
source("train_test.R")


## configuration for testing
conf.testing = umap.defaults
conf.testing$n_neighbors = 5
conf.testing = umap.check.config(conf.testing)


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


test_that("saving intermediate embeddings", {
  ## must enable verbose
  conf = umap.defaults
  conf$n_neighbors = 4
  conf$n_epochs = 2
  conf$verbose = TRUE
  conf$save = file.path("syn0.intermediate")
  expect_message(umap(syn0, conf))
  expected.files = c("syn0.intermediate.1.Rda", "syn0.intermediate.2.Rda")
  expect_true(all(file.exists(expected.files)))
  file.remove(expected.files)
})




## ############################################################################
## predictions (this uses data from train_test.R


test_that("predict checks knn, data components are available", {
  usmall = i.train.u
  usmall$knn = NULL
  expect_error(predict(usmall, i.test), "knn")
  usmall$data = NULL
  expect_error(predict(usmall, i.test), "data")
})


test_that("predict returns an embedding", {
  result = predict(i.train.u, i.test)
  expect_equal(dim(result), c(nrow(i.test), 2))
  expect_equal(rownames(result), rownames(i.test))
})


test_that("predicted layout is reasonable", {  
  result = predict(i.train.u, i.test)
  ## both training and test data are in batches of 3 from iris species
  setsize = nrow(i.train.u$layout)/3
  primary = i.train.u$layout
  cluster.centers = matrix(0, ncol=2, nrow=3)
  cluster.centers[1,] = colMeans(primary[1:setsize,])
  cluster.centers[2,] = colMeans(primary[setsize+(1:setsize),])
  cluster.centers[3,] = colMeans(primary[2*setsize+(1:setsize),])
  rownames(cluster.centers) = paste0("medoid", 1:3)
  distances = as.matrix(dist(rbind(result, cluster.centers)))
  distances.to.medoids = distances[1:nrow(result), rownames(cluster.centers)]
  nearest.medoid = apply(distances.to.medoids, 1, which.min)
  expected = setNames(rep(1:3, each=nrow(result)/3), rownames(result))
  expect_equal(nearest.medoid, expected)
})


test_that("predicted layout changes with epochs", {
  ## predict with no optimization
  u0 = i.train.u
  u0$config$n_epochs=0
  result0 = predict(u0, i.test)
  ## predict with optimization
  u1 = i.train.u
  result1 = predict(u1, i.test)
  ## the two results should be different
  expect_gt(sum(abs(result1-result0)), 0)

  #plot(i.train.u$layout, pch=19, col=i.train.labels)
  #points(result0, pch=4, cex=4, xpd=1)
  #points(result1, pch=6, cex=4, xpd=1) 
})


test_that("predict is reproducible when seed is set", {
  conf = umap.defaults
  conf$random_state= 102030405
  conf$n_neighbors = 6
  conf$transform_state = 982771
  conf$n_epochs=60
  i.seeded = umap(i.train, conf)
  
  ## start rng in this environment
  set.seed(9001)
  r1 = runif(1)

  ## restart rng in this environment
  set.seed(9001)
  result.a = predict(i.seeded, i.test)
  result.b = predict(i.seeded, i.test)
  ## even though umap uses random numbers, seed in parent should remain unchanged
  r2 = runif(1)
  
  abdiff = sum(abs(result.a-result.b))
  expect_equal(result.a, result.b, tolerance=1e-4)
  expect_equal(r1, r2, tolerance=1e-4)
})


