## tests for selecting methods

i4 = iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
i4d = dist(i4)


test_that("method cmdscale", {
  cmd1 = cmdscale(i4d)
  cmd2 = umap(i4d, method="cmdscale")
  expect_true(identical(cmd1, cmd2))
})


test_that("method bad", {
  expect_error(umap(i4d, method="non_existent_method"))
})


test_that("method naive", {
  cmd = umap(i4d, method="cmd")
  naive = umap(i4d, method="naive") 
  expect_false(identical(cmd, naive))
})

