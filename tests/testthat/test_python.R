## tests for running python umap

cat("\ntest_python\n")
library(reticulate)


## for the python implemenation, small iris-based datasets give warnings
## so generate synthetic data 
d.train = matrix(rnorm(400), ncol=4)
d.train[,1] = d.train[,1] - 1
d.train[,2] = d.train[,2] + 1
rownames(d.train) = paste0("Train", 1:nrow(d.train))
d.test = matrix(rnorm(200), ncol=4)
rownames(d.test) = paste0("Test", 1:nrow(d.test))




## ############################################################################
## Only test if umap is available as python package


if (reticulate::py_module_available("umap")) {
  ## Note: a more elegant approach with skip() does not seem to work
  ## devtools::test() stops when skip() is invoked
  
  ## create initial embedding
  u1 = umap(d.train, method="umap-learn", n_neighbors=10)
  
  
  test_that("python umap produces output", {
    ## just check the rough type of expected output
    ## i.e. that some output came out of the python UMAP fit
    expect_equal(class(u1), "umap")
    expect_true("layout" %in% names(u1))
    expect_true("config" %in% names(u1))
    ## python implementation sets config, conveys arguments used
    expect_gt(length(u1$config$umap_learn_args), 5)
    ## output layout makes sense, has rownames
    expect_equal(dim(u1$layout), c(nrow(d.train), 2))
    expect_equal(rownames(u1$layout), rownames(d.train))
    ## python implementation returns a UMAP object
    expect_true("UMAP" %in% names(u1))
  })
  
  
  test_that("python umap can use specified arguments", {
    conf = umap.defaults
    conf$n_neighbors = 10
    conf$umap_learn_args = c("n_neighbors", "random_state")
    result = umap(d.train, conf, method="umap-learn")
    expect_message(umap(d.train, conf, method="umap-learn", verbose=1), "calling")
    ## python implementation sets config, conveys arguments used
    expect_equal(length(result$config$umap_learn_args), 2)
  })
  
  
  test_that("python umap considers user-specified inputs", {
    uconf = umap.defaults
    uconf$n_neighbors = 10
    uconf$random_state = 567
    ## repeat calculations should give same results
    result1 = umap(d.train, uconf, method="umap-learn")
    result2 = umap(d.train, uconf, method="umap-learn")
    ## check reproducible results in layout
    expect_true(identical(result1$layout, result2$layout))
    ## calculations with different setting should give different result
    result3 = umap(d.train, uconf, method="umap-learn")
    expect_false(identical(result1, result3))
    result4 = umap(d.train, uconf, method="umap-learn", spread=2)
    expect_false(identical(result3, result4))
  })
  
  
  test_that("transform requires presence of UMAP components", {
    u1small = u1
    u1small$UMAP = NULL
    expect_error(predict(u1small, d.test), "available")
    u1bad = u1
    u1bad$UMAP = "some other object"
    expect_error(predict(u1bad, d.test), "corrupt")
  })
  
  
  test_that("transform proeduces an embedding", {
    u2 = predict(u1, d.test)
    expect_equal(dim(u2), c(nrow(d.test), 2))
    expect_equal(rownames(u2), rownames(d.test))
  })
  
}




