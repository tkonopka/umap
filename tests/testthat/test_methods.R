## tests for selecting methods

cat("\ntest_methods\n")


i.select = c(1:12, 61:72, 121:132)
i4 = iris[i.select, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]




## ############################################################################
## General expected behavior


test_that("error with inappropriate method argument", {
  expect_error(umap(i4, method="non_existent_method"))
})


test_that("reproducible output when seed is set", {
  conf = umap.defaults
  conf$random_state= 102030405
  conf$init = "random"
  conf$n_epochs = 2
  ## start rng in this environment
  set.seed(9001)
  r1 = runif(1)

  ## restart rng in this environment
  set.seed(9001)
  result.a = umap(i4, conf)
  result.b = umap(i4, conf)
  ## even though umap uses random numbers, seed in parent should remain unchanged
  r2 = runif(1)
  
  abdiff = sum(abs(result.a$laytout-result.b$layout))
  expect_equal(result.a, result.b, tolerance=1e-4)
  expect_equal(r1, r2, tolerance=1e-4)
})


test_that("output with rownames", {
  conf = umap.defaults
  conf$n_epochs = 2
  conf$init = "random"
  data = i4
  rownames(data) = paste0("S", 1:nrow(i4))
  result = umap(data, conf)
  expect_equal(rownames(result$layout), rownames(data))
})




## ############################################################################
## Settings parameters without configuration object


test_that("accept parameters in main function", {
  conf = umap.defaults
  conf$n_neighbors = 5
  conf$n_epochs = 2
  conf$init = "random"
  data = i4
  ## override n_neighbors explicitly 
  result = umap(data, conf, n_neighbors=3)
  expect_equal(result$config$n_neighbors, 3)
  expect_equal(ncol(result$knn$indexes), 3)
})


test_that("supply knn in main", {
  conf = umap.defaults
  conf$n_neighbors = 5
  conf$n_epochs = 2
  conf$init = "random"
  data = i4
  result1 = umap(data, conf)
  ## supply knn, result2 should not recompute knn
  result2 = umap(data, conf, knn=result1$knn)
  expect_equal(result1$knn, result2$knn)
})



## ############################################################################
## Logging 


test_that("log messages in verbose mode", {
  conf = umap.defaults
  conf$verbose = TRUE
  conf$init = "random"
  conf$n_epochs = 0
  expect_message(umap(i4, conf), "starting")
  expect_message(umap(i4, conf), "creating")
})


test_that("log messages during prediction verbose mode", {
  conf = umap.defaults
  conf$verbose = TRUE
  conf$init = "random"
  conf$n_epochs = 0
  u4 = suppressMessages(umap(i4, conf))
  expect_message(predict(u4, i4), "creating")
})


test_that("progress messages in verbose mode", {
  conf = umap.defaults
  conf$verbose = TRUE
  conf$init = "random"
  conf$n_epochs = 13
  conf$verbose = 4
  expect_message(umap(i4, conf), "12")
})


test_that("use of supplied knn", {
  conf = umap.defaults
  conf$n_neighbors = 3
  conf$n_epochs = 2
  conf$init = "random"
  data = i4
  ## override n_neighbors explicitly 
  result1 = umap(data, conf)
  ## expect log message
  expect_message(umap(data, conf, verbose=TRUE, knn=result1$knn), "supplied")
})



## ############################################################################
## Unusual datasets


test_that("spectral layout on two component data", {
  ## create dataset with 
  ilarge = rbind(i4, i4+100)  
  conf = umap.defaults
  conf$n_epochs = 0
  result = umap(ilarge, conf)$layout
  expect_equal(dim(result), c(nrow(ilarge), 2))
})


test_that("spectral layout aborts and uses random init with small datasets", {
  ismall = i4[1:3,]
  conf = umap.defaults
  conf$n_epochs = 2
  conf$n_neighbors = 3
  expect_warning(umap(ismall, conf), "init")
})

