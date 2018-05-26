## tests for small datasets

cat("\ntest small\n")




## ############################################################################
## General expected behavior

test_that("empty dataset gives empty layout", {
  ## empty dataset
  i0 = iris[c(),]
  expect_warning(umap(i0), "small")
  ## result should still be well-formed
  result = suppressWarnings(umap(i0))
  expect_equal(sort(names(result)), c("config", "layout"))
})


test_that("single-item dataset", {
  ## empty dataset
  i1 = iris[1,1:3]
  rownames(i1) = "A"
  expect_warning(umap(i1), "small")
  ## result should still be well-formed
  result = suppressWarnings(umap(i1))
  expect_equal(sort(names(result)), c("config", "layout"))
  expected = matrix(0, ncol=2, nrow=1)
  rownames(expected)="A"
  expect_equal(result$layout, expected)
})


test_that("two-item dataset", {
  ## empty dataset
  i2 = iris[1:2,1:3]
  rownames(i2) = c("A", "B")
  expect_warning(umap(i2), "small")
  ## result should still be well-formed
  result = suppressWarnings(umap(i2))
  expect_equal(sort(names(result)), c("config", "layout"))
  expect_equal(dim(result$layout), c(2,2))
  expect_gt(sum(abs(result$layout[1,]-result$layout[2,])), 0)
})

