## tests for configuration updating

test_that("create a default config object", {
  result <- umap.prep.config()
  expect_equal(result$n.neighbors, umap.defaults$n.neighbors)
})

test_that("create a config object with some modifications", {
  result <- umap.prep.config(config=umap.defaults, n.neighbors=100)
  expect_equal(result$n.neighbors, 100)
})
