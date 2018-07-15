## tests for spectral analysis using coos

cat("\ntest_spectral\n")


## ############################################################################
## Tests for counting of connected components using coo

test_that("count components in single-componet ", {
  mat = matrix(0, ncol=6, nrow=6)
  mat[1,c(2,3)] = 1
  mat[2, c(3,4)] = 1
  mat[3, c(4,5)] = 1
  mat[4, c(5,6)] = 1
  mat[5, c(6)] = 1
  mat = mat + t(mat)
  result = concomp.coo(coo(mat))
  expect_equal(result$n.components, 1)
  expect_equal(result$components, rep(0, 6))
})


test_that("count components, one two roughly equal-size componets", {
  mat = matrix(0, ncol=6, nrow=6)
  mat[1,c(2,3)] = 1
  mat[2, c(3)] = 1
  mat[4, c(5,6)] = 1
  mat[5, c(6)] = 1
  mat = mat + t(mat)
  result = concomp.coo(coo(mat))
  expect_equal(result$n.components, 2)
  expect_equal(result$components, c(rep(0, 3), rep(1, 3)))
})


test_that("count components, one disjoint element", {
  mat = matrix(0, ncol=6, nrow=6)
  mat[1,c(2,3)] = 1
  mat[2, c(3)] = 1
  mat[4, c(5)] = 1
  mat = mat + t(mat)
  result = concomp.coo(coo(mat))
  expect_equal(result$n.components, 3)
  expect_equal(result$components, c(rep(0, 3), rep(1, 2), 2))
})


test_that("count components, null matrix", {
  mat = matrix(0, ncol=6, nrow=6)
  result = concomp.coo(coo(mat))
  expect_equal(result$n.components, 6)
  expect_equal(result$components, 0:5)
})




## ############################################################################
## Tests for constructing identity matrix


test_that("simple identity coo", {
  result = identity.coo(4)
  expected = reduce.coo(coo(diag(4)))
  expect_equal(result, expected)
})


test_that("identity complains when names and n.elements don't match", {
  expect_error(identity.coo(4, letters[1:2]))
  expect_error(identity.coo(4, NA))
})


test_that("identity with names", {
  result = identity.coo(4, letters[1:4])
  mat = diag(4)
  rownames(mat) = colnames(mat) = letters[1:4]
  expected = reduce.coo(coo(mat))
  expect_equal(result, expected)
})




## ############################################################################
## Tests for constructing Laplacian


test_that("laplacian of matrix", {
  mat = matrix(0, ncol=6, nrow=6)
  mat[1,c(2,3)] = 1
  mat[2, c(3,4)] = 1
  mat[3, c(4,5)] = 1
  mat[4, c(5,6)] = 1
  mat[5, c(6)] = 1
  mat = mat + t(mat)

  ## compute laplacian using coo representation
  result = laplacian.coo(coo(mat))
  
  ## construct expected laplacian matrix
  degrees = apply(mat, 1, sum)
  D = diag(1/sqrt(degrees))
  I = diag(nrow(mat))
  L = I - (D %*% mat %*% D)
  expected = reduce.coo(coo(L))

  expect_equal(result, expected)
})


test_that("laplacian of graph matrix", {
  ## create a large matrix with some numbers
  mat = matrix(abs(rnorm(20*20)), ncol=20)
  diag(mat) = 0
  
  ## compute a laplacian using coo methods
  result = laplacian.coo(coo(mat))
  
  ## construct expected laplacian matrix using matrix methods
  degrees = apply(mat, 1, sum)
  D = diag(1/sqrt(degrees))
  I = diag(nrow(mat))
  L = I - (D %*% mat %*% D)
  expected = reduce.coo(coo(L))
  
  expect_equal(result, expected, tolerance=1e-3)
})


test_that("laplacian of matrix with zeros", {
  mat = matrix(1:36, ncol=6, nrow=6)
  mat[3,] =0
  matcoo = reduce.coo(coo(mat))
  expect_error(laplacian.coo(matcoo))
})




## ############################################################################
## Tests for subsetting coo matrices


test_that("subset coo detects early exit", {
  mat = matrix(1:9, ncol=3, nrow=3)
  diag(mat) = 0
  c1 = coo(mat)
  result = subset.coo(c1, 1:3)
  expect_equal(result, c1)
})


test_that("subset operates and produces a new coo", {
  mat = matrix(1:16, ncol=4, nrow=4)
  diag(mat) = 0
  c1 = coo(mat)
  keep = c(1,3)
  result = subset.coo(c1, keep)
  expected = mat[keep, keep]
  expect_equal(result, coo(expected))
})

