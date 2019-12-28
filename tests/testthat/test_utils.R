## tests for universal functions (umap_universal.R)

cat("\ntest_utils\n")



## ############################################################################
## Tests for exact nearest neighbors extraction


test_that("message when verbose set", {
  expect_message(message.w.date("hello", TRUE))
})


test_that("message by default when verbose not set", {
  expect_silent(message.w.date("hello"))
})

