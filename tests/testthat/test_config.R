## tests for configuration updating

cat("\ntest_config\n")


test_that("assemble umap configuration from default object and arguments", {
  cc = umap.check.config()
  expect_equal(cc$n.neighbors, umap.defaults$n.neighbors)
  cc100 = umap.check.config(config=umap.defaults, n.neighbors=100)
  expect_equal(cc100$n.neighbors, 100)
})


test_that("print of config produces outpu", {
  conf = umap.defaults
  ## print should display a welcome message
  expect_message(print(conf), "umap")
  ## print should display up-to-date content in the config object
  conf$seed = 1234567
  expect_message(print(conf), "1234567")
})


test_that("print complains on non-classed input", {
  expect_error(print.umap.config(1:4))
})


test_that("print display placeholder instead of matrix", {
  conf = umap.defaults
  conf$init = matrix(0, ncol=2, nrow=4)
  expect_message(print(conf), "matrix")
})

