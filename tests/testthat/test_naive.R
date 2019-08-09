# tests using naive method

cat("\ntest_naive\n")
source("synthetic.R")
source("train_test.R")


# configuration for testing
conf.testing = umap.defaults
conf.testing$n_neighbors = 5
conf.testing = umap.check.config(conf.testing)


test_that("synthetic fuzzy simplicial set (from dist)", {
  conf = conf.testing
  conf$input = "dist"
  knn = knn.info(syn0.dist, conf)
  graph = naive.fuzzy.simplicial.set(knn, conf)
  # top-part of expected graph (computed by python implementation)
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
  # top-part of expected graph (computed by python implementation)
  expected.graph = matrix(0, ncol=3, nrow=3)
  expected.graph[1,] = c(1,2, 1)
  expected.graph[2,] = c(1,3, 0.958418847698)
  expected.graph[3,] = c(1,4, 0.204675928724)  
  colnames(expected.graph) = c("from", "to", "value")
  # compoare top parts of graph
  expect_equal(expected.graph, graph$coo[1:3, ], tolerance=1e-4)
})




# ############################################################################
# small datasets down to 1 component 

test_that("embedding to one component", {
  # create full configuration object
  conf = umap.defaults
  conf$n_neighbors=3
  conf$n_components=1
  ismall = iris[1:5,1:4]
  # only basic checks (no errors, correct format)
  result = umap(ismall, conf)
  expect_equal(dim(result$layout), c(5, 1))
})




# ############################################################################
# predictions (this uses data from train_test.R


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
  # both training and test data are in batches of 3 from iris species
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
  # in principle, nearest.medoid and expected should be exactly equal
  # but due to randomness and some OS- or architecture-dependent minutae,
  # sometimes one component can be off. So allow some discrepancies in the test.
  expect_lte(sum(abs(nearest.medoid-expected)), 1)
})


test_that("predicted layout changes with epochs", {
  # predict with no optimization
  u0 = i.train.u
  u0$config$n_epochs=0
  result0 = predict(u0, i.test)
  # predict with optimization
  u1 = i.train.u
  result1 = predict(u1, i.test)
  # the two results should be different
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
  conf$n_epochs = 60
  i.seeded = umap(i.train, conf)
  
  # start rng in this environment
  set.seed(9001)
  r1 = runif(1)

  # restart rng in this environment
  set.seed(9001)
  result.a = predict(i.seeded, i.test)
  result.b = predict(i.seeded, i.test)
  # even though umap uses random numbers, seed in parent should remain unchanged
  r2 = runif(1)
  
  expect_equal(result.a, result.b, tolerance=1e-4)
  expect_equal(r1, r2, tolerance=1e-4)
})


test_that("predict gives slightly different result based on seed", {
  # a configuration with a fixed seed
  conf1 = umap.defaults
  conf1$random_state= 102030405
  conf1$n_neighbors = 6
  conf1$transform_state = 982771
  conf1$n_epochs = 60
  # a second configuration with a different seed
  conf2 = conf1
  conf2$transform_state = 987654
  u1 = umap(i.train, conf1)
  u2 = umap(i.train, conf2)
  
  # initial layouts should be the same (random_state is the same)
  expect_equal(u1$layout, u2$layout)

  # predictions should be different
  p1 = predict(u1, i.test)
  p2 = predict(u2, i.test)
  expect_gt(sum(abs(p1-p2)), 0)
})


# ###########################################################################
# stability of predictions (this uses data from train_test.R


test_that("predict return same layout individually and in batch", {
  conf = umap.defaults
  conf$random_state= 102030405
  conf$n_neighbors = 4
  conf$transform_state = 982771
  conf$n_epochs=50
  i.seeded = umap(i.train, conf)
  
  result.batch = predict(i.seeded, i.test)
  result.individual = matrix(0, ncol=2, nrow=nrow(i.test))
  for (i in seq_len(nrow(i.test))) {
    result.individual[i,] = predict(i.seeded, i.test[i,,drop=FALSE])
  }
  rownames(result.individual) = rownames(i.test)
  
  # sanity check = two output objects should match in structure
  expect_equal(dim(result.individual), dim(result.batch))
  # all the coordinates should match
  expect_equal(result.batch[,1], result.individual[,1])
  expect_equal(result.batch[,2], result.individual[,2])
})

