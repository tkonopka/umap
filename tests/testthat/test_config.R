## tests for configuration updating

cat("\ntest_print\n")


test_that("assemble umap configuration from default object and arguments", {
  cc = umap.check.config()
  expect_equal(cc$n.neighbors, umap.defaults$n.neighbors)
  cc100 = umap.check.config(config=umap.defaults, n.neighbors=100)
  expect_equal(cc100$n.neighbors, 100)
})

