## tests using a complete synthetic example

cat("\ntest_synthetic\n")
source("synthetic.R")


test_that("synthetic knn.info", {
  data.knn.info = knn.from.dist(as.matrix(syn0.dist), 5)
  expected.indexes = matrix(c(0,1,2,4,3,  1,2,0,4,3,  2,1,0,4,3,
                              3,4,5,2,1,  4,3,5,2,1,  5,3,4,2,1,
                              6,8,7,2,1,  7,6,8,2,1,  8,6,7,2,1),
                            ncol=5, nrow=9, byrow=T)
  expected.indexes = expected.indexes + 1
  rownames(expected.indexes) = rownames(syn0)

  expect_equal(data.knn.info$indexes, expected.indexes)
  ## distances should also match. Avoid explicit test here 
})


test_that("synthetic smooth", {
  info = knn.from.dist(as.matrix(syn0.dist), 5)
  smooth = smooth.knn.dist(info$distance, 5)
  ##print(smooth)
  ## sigmas (computed by python implementation)
  expected.distances = c(1.14962769,  1.20117188,  1.19216919,
                         1.23526001,  1.21936035,  1.04608154,
                         1.88842773,  1.38781738,  1.83068848)
  ## rhos (computed by python implementation)
  expected.nearest = c(0.53851648,  0.24494897,  0.24494897,
                       0.24494897,  0.24494897,  0.58309519,
                       0.3,         0.48989795,  0.3)
  
  ## compare sigmas
  expect_equal(smooth$distances, expected.distances, tolerance=1e-4)
  ## compare nearest distances
  expect_equal(smooth$nearest, expected.nearest, tolerance=1e-4)
})

