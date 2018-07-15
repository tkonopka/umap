## tests for coo functions

cat("\ntest_coo\n")


## create a simple connectivity matrix for testing purposes
conmat = matrix(1:9, ncol=3, nrow=3)
diag(conmat) = 0




## ############################################################################
## Tests for converting between a connectivity matrix to coo


test_that("coo construction from matrix without names", {
  result = coo(conmat)
  expect_equal(result$n.elements, 3)
  expect_null(result$names)
})


test_that("coo construction from matrix with names", {
  mm = conmat
  colnames(mm) = rownames(mm) = letters[1:3]
  result = coo(mm)
  expect_equal(result$n.elements, 3)
  expect_equal(result$names, letters[1:3])
})
 

test_that("coo in simple matrix", {
  result = coo(conmat)
  ## check construction of object
  expect_equal(class(result), "coo")
  ## check size of coo object
  expect_equal(result$n.elements, 3)
  
  ## create a coo representation of the matrix
  expected = matrix(0, ncol=3, nrow=9)
  expected[1,] = c(1,1,0)
  expected[2,] = c(1,2,4)
  expected[3,] = c(1,3,7)
  expected[4,] = c(2,1,2)
  expected[5,] = c(2,2,0)
  expected[6,] = c(2,3,8)
  expected[7,] = c(3,1,3)
  expected[8,] = c(3,2,6)
  expected[9,] = c(3,3,0)
  colnames(expected) = c("from", "to", "value")
  expect_equal(expected, result$coo)
})


test_that("coo signals input must be matrix", {
  ## input must be a matrix
  expect_error(coo(1:5))
  ## input must be a square matrix
  expect_error(coo(matrix(0, ncol=2, nrow=4)))
})




## ############################################################################
## Tests for simplifying (i.e. reducing, thresholding) coo matrices


test_that("simplify must act on a coo", {
  expect_error(reduce.coo(1:5))
})


test_that("simplify coo with zeros, default settings", {
  result = reduce.coo(coo(conmat))
  expected = matrix(0, ncol=3, nrow=6)
  expected[1,] = c(1,2,4)
  expected[2,] = c(1,3,7)
  expected[3,] = c(2,1,2)
  expected[4,] = c(2,3,8)
  expected[5,] = c(3,1,3)
  expected[6,] = c(3,2,6)
  colnames(expected) = c("from", "to", "value")
  expect_equal(expected, result$coo)
})


test_that("simplify coo, to just one row", {
  mat = matrix(0, ncol=5, nrow=5)
  mat[2,3] = 8
  result = reduce.coo(coo(mat))
  expected = matrix(0, ncol=3, nrow=1)
  expected[1,] = c(2,3,8)
  colnames(expected) = c("from", "to", "value")
  expect_equal(expected, result$coo)
})



## ############################################################################
## Tests for transposing coo matrices


test_that("coo of transpose is the same as transpose of coo", {
  result = t.coo(coo(conmat))
  ## to compare directly later, will need ordering by to/from
  result$coo = result$coo[order(result$coo[, "from"], result$coo[, "to"]),]
  expected = coo(t(conmat))
  expect_equal(expected, result)
})




## ############################################################################
## Tests for coo multiplication (element-wise)


test_that("multiplication between incompatible objects", {
  ## m1 is a matrix with names
  m1 = conmat
  rownames(m1) = colnames(m1) = letters[1:3]
  c1 = coo(m1)
  ## m2 is a smaller matrix
  m2 = m1[1:2,1:2]
  c2 = coo(m2)
  ## m3 is same size as m1, but with different names
  m3 = m1
  colnames(m3) = rownames(m3) = NULL
  c3 = coo(m3)
  
  ## cannot multiply any of these together
  expect_error(multiply.coo(c1, c2))
  expect_error(multiply.coo(c1, c3))
  expect_error(multiply.coo(c2, c3))
})


test_that("multiplication gives object of correct class and size", {
  mm = conmat
  rownames(mm) = colnames(mm) = letters[1:3]
  c1 = coo(mm)
  c2 = coo(mm)
  result = multiply.coo(c1, c2)
  expect_equal(result$n.elements, 3)
  expect_equal(result$names, c1$names)
})


test_that("multiplication with matrix of all-1", {
  c1 = coo(conmat)
  c2 = coo(matrix(1, ncol=3, nrow=3))
  result = multiply.coo(c1, c2)
  expect_equal(c1, result)
})


test_that("multiplication with matrix of all-0", {
  c1 = coo(conmat)
  c2 = coo(matrix(0, ncol=3, nrow=3))
  result = multiply.coo(c1, c2)
  c2.red = reduce.coo(c2)
  result.red = reduce.coo(result)
  expect_equal(c2.red, result.red)
  expect_equal(nrow(result.red$coo), 0)
})



## ############################################################################
## Tests for coo addition

test_that("simple add", {
  c1 = coo(conmat)
  result = add.coo(c1, c1)
  expected = c1
  expected$coo[, "value"] = c1$coo[, "value"]*2
  expect_equal(expected, result)
})


test_that("simple subtract", {
  c1 = coo(conmat)
  result = add.coo(c1, c1, a=1, b=-1)
  expected = c1
  expected$coo[, "value"] = 0
  expect_equal(expected, result)
})

test_that("add to a zero matrix", {
  c1 = coo(conmat)
  c2 = coo(matrix(0, ncol=ncol(conmat), nrow=nrow(conmat)))
  result = add.coo(c1, c2)
  expected = c1
  expect_equal(expected, result)
})




## ############################################################################
## Tests for conversion back to matrix


test_that("matrix to coo and back", {
  c1 = reduce.coo(coo(conmat))
  c1mat = coo2mat(c1)
  expect_equal(conmat, c1mat)
})

