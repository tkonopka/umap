## tests using naive method

cat("\ntest_checks.R\n")


test_that("checking config detects errors with nearest neighbors", {
  ## detect errors encoded in config
  conf = umap.defaults
  conf$n.neighbors = 0.5
  expect_error(umap.check.config(conf))

  ## detect errors from command line
  expect_error(umap.check.config(umap.defaults, n.neighbors=0))
})


test_that("checking config detects input methods", {
  conf = umap.defaults
  conf$input = NA
  expect_error(umap.check.config(conf))
})


test_that("checking config replaced metric.function by a function", {
  conf = umap.check.config(umap.defaults)
  expect_equal(class(conf$metric.function), "function")
})


test_that("checking config complains about unknown functions", {
  conf = umap.defaults
  conf$metric.function = "badfunction"
  expect_error(umap.check.config(conf))
})
