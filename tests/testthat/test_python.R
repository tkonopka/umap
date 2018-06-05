## tests for running python umap

cat("\ntest_python\n")
library(reticulate)


i.select = c(1:12, 61:72, 121:132)
i4 = iris[i.select, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]




## ############################################################################
## Only test if umap is available as python package


if (reticulate::py_module_available("umap")) {
  ## Note: a more elegant approach with skip() does not seem to work
  ## devtools::test() stops when skip() is invoked
  
  test_that("python umap produces output", {
    result = umap(i4, method="python")
    ## just check the rough type of expected output
    ## i.e. that some output came out of the python UMAP fit
    expect_equal(class(result), "umap")
    expect_true("layout" %in% names(result))
    expect_true("config" %in% names(result))
    expect_equal(dim(result$layout), c(nrow(i4), 2))
  })

  
  test_that("python umap considers user-specified inputs", {
    uconf = umap.defaults
    uconf$n.epochs = 5
    uconf$n.neighbors = 5
    uconf$seed = 123
    ## repeat calculations should give same results
    result1 = umap(i4, uconf, method="python")
    result2 = umap(i4, uconf, method="python")
    expect_true(identical(result1, result2))
    ## calculations with different setting should give different result
    result3 = umap(i4, uconf, method="python", n.neighbors=6)
    expect_false(identical(result1, result3))
    result4 = umap(i4, uconf, method="python", spread=2)
    expect_false(identical(result3, result4))
  })
  
}


