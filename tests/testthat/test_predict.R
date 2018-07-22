## test for predictions


cat("\ntest_predict\n")


## set up training and testing datasets
iris.columns = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
i.train    = iris[c(1:12, 61:72, 121:132), iris.columns]
i.repeat   = iris[c(1:12), iris.columns]
i.test     = iris[c(13:16, 73:76, 123:126), iris.columns]
## configuration for fast computation
conf.testing = umap.defaults
conf.testing$n_neighbors = 5
conf.testing$random_stats = 12345
## umap once
u.train = umap(i.train, conf.testing)


test_that("prediction method is appropriate in config", {
  utemp = u.train
  utemp$config$method = "bad"
  expect_error(predict(utemp, i.test), "method")
})


test_that("prediction detects when umap was trained on dist", {
  idist = as.matrix(dist(i.train))
  u2 = umap(idist, conf.testing, input="dist")
  expect_error(predict(u2, i.test), "dist")
})


test_that("prediction does not work with poor training data", {
  train = matrix(1:6, ncol=3, nrow=2)
  u.train = suppressWarnings(umap(train))
  test.data = matrix(1:9, ncol=3, nrow=3)
  expect_error(predict(u.train, test.data), "small")
})
