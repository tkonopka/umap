## tests for Rcpp components

cat("\ntest_rcpp\n")


## ############################################################################
## Tests for smoothing of knns


test_that("number clipping with custom xmax", {
  ff = seq(-5, 5, length=30)
  outer = 2.3
  inner = 1.023
  result1 = outer*clip(inner*ff, 4)
  result2 = c4(ff, inner, outer)
  expect_equal(result1, result2)
})


