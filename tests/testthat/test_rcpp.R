## tests for Rcpp components

cat("\ntest_rcpp\n")


## ############################################################################
## Tests for smoothing of knns


test_that("number clipping", {
  ff = seq(-5, 5, length=30)
  outer = 2.3
  inner = 1.023
  result1 = outer*clip(inner*ff, 4)
  result2 = clip4(ff, inner, outer)
  expect_equal(result1, result2)
})


test_that("euclidean distance", {
  a1 = c(1, 2, 3, 4)
  a2 = 1+a1
  expect_equal(dEuclidean(a1, a2), 2)
  b1 = c(1.3, 2.11, 9.101, 2.45)
  b2 = c(0.2, -0.2, 0.7, 12.2)
  expect_equal(dEuclidean(b1, b2), sqrt(sum((b1-b2)*(b1-b2))))
})





if (FALSE) {
  
  
  
  f1 = function(V) {
    aa = matrix(0, ncol=V, nrow=V)
    for (i in 1:(V-1)) {
      for (j in (i+1):V) {
        aa[i,j] = aa[j,i] = i+j
      }
    }
    aa
  }
  f2 = function(V) {
    aa = matrix(0, ncol=V, nrow=V)
    for (i in 1:(V-1)) {
      for (j in (i+1):V) {
        aa[i,j] = i+j
      }
    }
    aa + t(aa)
  }
  
  
  eps = cbind(A=1:3000, B=1:3000, C=1:3000)
  g1 = function(eps) {
    result = 0
    for (i in 1:nrow(eps)) {
      a1 = eps[i,1]
      a2 = eps[i,2]
      a3 = eps[i,3]
      result = result+a1+a2+a3
    }
    result
  }
  g2 = function(eps) {
    e1 = eps[,1]
    e2 = eps[,2]
    e3 = eps[,3]
    result = 0
    for (i in 1:nrow(eps)) {
      a1 = e1[i]
      a2 = e2[i]
      a3 = e3[i]
      result = result+a1+a2+a3
    }
    result
  }

  


  

}
