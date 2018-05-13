## tests for selecting methods

cat("\ntest_methods\n")


i.select = c(1:12, 61:72, 121:132)
i4 = iris[i.select, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
icolors = as.integer(as.factor(iris[i.select, "Species"]))




test_that("umap gives error with inappropriate method argument", {
  expect_error(umap(i4, method="non_existent_method"))
})


test_that("umap gives reproducible output when seed is set", {
  conf = umap.defaults
  conf$seed = 102030405
  conf$n.epochs = 5
  ## start rng in this environment
  set.seed(9001)
  r1 = runif(1)

  ## restart rng in this environment
  set.seed(9001)
  result.a = umap(i4, conf)
  result.b = umap(i4, conf)
  ## even though umap uses random numbers, seed in parent should remain unchanged
  r2 = runif(1)
  
  abdiff = sum(abs(result.a-result.b))
  expect_equal(result.a, result.b, tolerance=1e-4)
  expect_equal(r1, r2, tolerance=1e-4)
})


test_that("umap gives log messages in verbose mode", {
  conf = umap.defaults
  conf$verbose = TRUE
  conf$n.epochs = 5
  expect_message(umap(i4, conf))
})


test_that("umap gives output with rownames", {
  conf = umap.defaults
  conf$n.epochs = 5
  data = i4
  rownames(data) = paste0("S", 1:nrow(i4))
  result = umap(data, conf)
  expect_equal(rownames(result), rownames(data))
})


test_that("umap spectral layout on two component data", {
  ## create dataset with 
  ilarge = rbind(i4, i4+100)  
  conf = umap.defaults
  conf$n.epochs = 5
  result = umap(ilarge, conf)
  expect_equal(dim(result), c(nrow(ilarge), 2))
})



