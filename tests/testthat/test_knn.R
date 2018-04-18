## tests knn manipulations


test_that("k nearest neighbors information", {
  ## da is a distance matrix (not necessarily metric)
  da = matrix(0, ncol=5, nrow=5)
  ## object 1 - nearest items are 2,3 
  da[1,2] = da[2,1] = 1
  da[1,3] = da[3,1] = 2
  da[1,4] = da[4,1] = 3
  da[1,5] = da[5,1] = 4
  ## object 2 - nearest items are 1,4
  da[2,3] = da[3,2] = 10
  da[2,4] = da[4,2] = 2
  da[2,5] = da[5,2] = 11
  ## object 3 - nearest items are 1,4
  da[3,4] = da[4,3] = 3
  da[3,5] = da[5,3] = 12
  ## object 4 - nearest items are 5,2
  da[4,5] = da[5,4] = 1
  
  result = knn.info(da, 2)

  ## these matrices constructed manually by visual inspection of da
  expected.indexes = matrix(c(2,3, 1,4, 1,4, 5,2, 4,1),
                            byrow=TRUE, nrow=5, ncol=2)
  expected.distances = matrix(c(1,2, 1,2, 2,3, 1,2, 1,4),
                              byrow=TRUE, nrow=5, ncol=2)

  expect_equal(result$indexes, expected.indexes)
  expect_equal(result$distances, expected.distances)
})



