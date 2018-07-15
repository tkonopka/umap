## tests for Rcpp components

cat("\ntest_rcpp\n")


## ############################################################################
## Tests for clipping


test_that("number clipping", {
  ff = seq(-5, 5, length=30)
  outer = 2.3
  inner = 1.023
  result1 = outer*clip(inner*ff, 4)
  result2 = clip4(ff, inner, outer)
  expect_equal(result1, result2)
})




## ############################################################################
## Tests for distance functions


test_that("euclidean distance", {
  a1 = c(1, 2, 3, 4)
  a2 = 1+a1
  expect_equal(dEuclidean(a1, a2), 2)
  b1 = c(1.3, 2.11, 9.101, 2.45)
  b2 = c(0.2, -0.2, 0.7, 12.2)
  expect_equal(dEuclidean(b1, b2), sqrt(sum((b1-b2)*(b1-b2))))
})


test_that("manhattan distance", {
  a1 = c(1, 2, 3, 4)
  a2 = 1+a1
  expect_equal(dManhattan(a1, a2), 4)
  b1 = c(1.3, 2.11, 9.101, 2.45)
  b2 = c(0.2, -0.2, 0.7, 12.2)
  expect_equal(dManhattan(b1, b2), sum(abs(b1-b2)))
})


test_that("pearson distance", {
  a1 = rnorm(10)
  a2 = a1+rnorm(10)
  expected = 1-cor.test(a1, a2)$estimate^2
  names(expected) = NULL
  result = dCenteredPearson(a1-mean(a1), a2-mean(a2))
  expect_equal(expected, result, tolerance=1e-2)
})


test_that("cosine distance", {
  a1 = rnorm(10)
  a2 = a1+rnorm(10)
  l2norm = function(x) {
    sqrt(sum(x*x))
  }
  expected = 1-sum(a1*a2)/(l2norm(a1)*l2norm(a2))
  names(expected) = NULL
  result = dCosine(a1, a2)
  expect_equal(expected, result, tolerance=1e-2)
})



## ############################################################################
## Tests for matrix distance functions


test_that("euclidean distances from matrix", {
  v1 = (1:30) %% 7
  v2 = (1:30) %% 3
  mat = matrix(v1+(v2/10), ncol=3)
  m1 = mat[1,]

  ## compute several distances (loop in c)
  targets = c(6,7,2,3,4)
  output = mdEuclidean(t(mat), 1, targets)
  ## compute several distance (loop in apply)
  expected = apply(mat[targets,], 1, dEuclidean, m1)
  expect_equal(output, expected)
  
  #t0 = system.time(replicate(5e4, apply(mat[targets,,drop=FALSE], 1, dEuclidean, m1)))
  #t1 = system.time(replicate(5e4, mdEuclidean(mat[c(1, targets),])))
})




## ############################################################################
## Tests for layout optimization


test_that("layout optimization", {
  conf = umap.defaults
  conf$n.neighbors = 3
  conf$n.epochs=2
  conf = umap.check.config(conf)
  i4 = iris[c(1:5,61:65, 111:115),1:4]
  knn = knn.info(i4, conf)
  graph = naive.fuzzy.simplicial.set(knn, conf)
  embedding = make.initial.embedding(graph$n.elements, conf, graph)
  embedding = naive.simplicial.set.embedding(graph, embedding, conf)

  ## just test that output is of correct form
  expect_equal(dim(embedding), c(nrow(i4), 2))
})

