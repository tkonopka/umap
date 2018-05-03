## tests for selecting methods

cat("\ntest_methods\n")


i.select = c(1:20, 61:80, 121:140)
i4 = iris[i.select, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
icolors = as.integer(as.factor(iris[i.select, "Species"]))
i4d = dist(i4)
i4m = as.matrix(i4d)
i4c = cmdscale(i4d)


test_that("umap provides some output with method cmdscale", {
  cmd1 = cmdscale(i4d)
  cmd2 = umap(i4d, method="cmdscale")
  expect_true(identical(cmd1, cmd2))
})


test_that("umap gives error with inappropriate method argument", {
  expect_error(umap(i4d, method="non_existent_method"))
})


test_that("umap provides some output", {
  cmd = umap(i4d, method="cmd")
  naive = umap(i4d, method="naive") 
  expect_false(identical(cmd, naive))
})


test_that("umap gives reproducible output when seed is set", {
  conf = umap.defaults
  conf$seed = 102030405
  result.a = umap(i4, conf)
  result.b = umap(i4, conf)
  expect_true(identical(result.a, result.b))
})

