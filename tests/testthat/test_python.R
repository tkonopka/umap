## tests for running python umap

cat("\ntest_python\n")
library(reticulate)


i.select = c(1:12, 61:72, 121:132)
i4 = iris[i.select, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
rownames(i4) = paste0("X", 1:nrow(i4))



## ############################################################################
## Only test if umap is available as python package


if (reticulate::py_module_available("umap")) {
  ## Note: a more elegant approach with skip() does not seem to work
  ## devtools::test() stops when skip() is invoked
  
  test_that("python umap produces output", {
    result = umap(i4, method="umap-learn")
    ## just check the rough type of expected output
    ## i.e. that some output came out of the python UMAP fit
    expect_equal(class(result), "umap")
    expect_true("layout" %in% names(result))
    expect_true("config" %in% names(result))
    ## python implementation sets config, conveys arguments used
    expect_gt(length(result$config$umap_learn_args), 5)
    ## output layout makes sense, has rownames
    expect_equal(dim(result$layout), c(nrow(i4), 2))
    expect_equal(rownames(result$layout), rownames(i4))
    ## python implementation returns a UMAP object
    expect_true("UMAP" %in% names(result))
  })

  
  test_that("python umap can use specified arguments", {
    conf = umap.defaults
    conf$umap_learn_args = c("n_neighbors", "random_state")
    result = umap(i4, conf, method="umap-learn")
    expect_message(umap(i4, conf, method="umap-learn", verbose=1), "calling")
    ## python implementation sets config, conveys arguments used
    expect_equal(length(result$config$umap_learn_args), 2)
  })

  
  test_that("python umap considers user-specified inputs", {
    uconf = umap.defaults
    uconf$n_epochs = 5
    uconf$n_neighbors = 5
    uconf$random_state = 123
    ## repeat calculations should give same results
    result1 = umap(i4, uconf, method="umap-learn")
    result2 = umap(i4, uconf, method="umap-learn")
    ## check reproducible results in layout
    expect_true(identical(result1$layout, result2$layout))
    ## calculations with different setting should give different result
    result3 = umap(i4, uconf, method="umap-learn", n_neighbors=6)
    expect_false(identical(result1, result3))
    result4 = umap(i4, uconf, method="umap-learn", spread=2)
    expect_false(identical(result3, result4))
  })

}

