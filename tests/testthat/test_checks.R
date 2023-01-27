## tests using naive method

## ############################################################################
## Tests for primary input

test_that("input can be matrix or data frame", {
  # class matrix is preserved
  mat <- matrix(0, ncol=2, nrow=3)
  colnames(mat) <- c("A", "B")
  mat[,1] <- 1:3
  mat[,2] <- 11:13
  conf <- umap.defaults
  conf$metric.name <- "euclidean"
  expect_equal(umap.prep.input(mat, conf), mat)
  # data frames are converted to matrices
  df <- data.frame(A=1:3, B=11:13)
  expect_silent(umap.prep.input(df, conf))
  result <- umap.prep.input(df, conf)
  expect_equal(result, mat)
})

test_that("input cannot be non-matrix", {
  conf <- umap.defaults
  conf$metric <- "euclidean"
  expect_error(umap.prep.input(1:4, conf))
  expect_error(umap.prep.input(letters[1:4], conf))
})

test_that("input is forced into numeric data type", {
  conf <- umap.defaults
  conf$metric <- "euclidean"
  mat <- matrix(1:4, ncol=2)
  expect_is(mat[,1], "integer")
  result <- umap.prep.input(mat, conf)
  expect_is(result[,1], "numeric")
})

test_that("prep centers input for pearson distance", {
  conf <- umap.defaults
  conf$metric <- "pearson"
  mat <- matrix(0, ncol=2, nrow=3)
  colnames(mat) <- c("A", "B")
  mat[,1] <- 1:3
  mat[,2] <- 11:13
  expected <- mat
  for (i in seq_len(nrow(mat))) {
    expected[i,] <- mat[i,] - mean(mat[i,])
  }
  result <- umap.prep.input(mat, conf)
  expect_equal(result, expected)
})

## ############################################################################
## Tests for configuration settings

test_that("checking config detects errors with nearest neighbors", {
  # detect errors encoded in config
  conf <- umap.defaults
  conf$n_neighbors <- 0.5
  expect_error(umap.prep.config(conf))
  expect_error(umap.prep.config(umap.defaults, n_neighbors=0))
})

test_that("config detects input methods", {
  conf <- umap.defaults
  conf$input <- NA
  conf$input <- NA
  expect_error(umap.prep.config(conf))
})

test_that("config replaced metric.function by a function", {
  conf <- umap.prep.config(umap.defaults)
  expect_is(conf$metric.function, "function")
})

## ############################################################################
## Tests for specific distance functions out of check

d2 <- matrix(0, ncol=5, nrow=2)
d2[1,] <- c(-2,-1,0,1,2)
d2[2,] <- c(-4,2,-2,0,4)
d2 <- t(d2)

test_that("config sets euclidean distance function", {
  conf <- umap.defaults
  conf$metric <- "euclidean"
  result <- umap.prep.config(conf)
  expect_equal(mdEuclidean(d2,1,2), result$metric.function(d2,1,2))
  expect_equal(result$metric, "euclidean")
})

test_that("config sets pearson distance function", {
  conf <- umap.defaults
  conf$metric <- "pearson2"
  result <- umap.prep.config(conf)
  expect_equal(mdCenteredPearson(d2,1,2), result$metric.function(d2,1,2))
  expect_equal(result$metric, "pearson2")
})

test_that("config sets manhattan distance function", {
  conf <- umap.defaults
  conf$metric <- "manhattan"
  result <- umap.prep.config(conf)
  expect_equal(mdManhattan(d2,1,2), result$metric.function(d2,1,2))
  expect_equal(result$metric, "manhattan")
})

test_that("config sets cosine distance function", {
  conf <- umap.defaults
  conf$metric <- "cosine"
  result <- umap.prep.config(conf)
  expect_equal(mdCosine(d2,1,2), result$metric.function(d2,1,2))
  expect_equal(result$metric, "cosine")
})

test_that("config sets custom metric function", {
  myfun <- function(m,i,j) {
    rep(-1.0, length(j))
  }
  conf <- umap.defaults
  conf$metric <- myfun
  result <- umap.prep.config(conf)
  expect_equal(myfun(d2,1,2), result$metric.function(d2,1,2))
  expect_equal(result$metric, "custom")
})

test_that("config reports unknown metric functions", {
  conf <- umap.defaults
  conf$method <- "naive"
  conf$metric <- "badfunction"
  expect_error(umap.prep.config(conf), "unrecognized")
})

test_that("config checks local connectivity", {
  conf <- umap.defaults
  conf$local_connectivity <- 0
  expect_error(umap.prep.config(conf), "connect")
  conf$local_connectivity <- -0.2
  expect_error(umap.prep.config(conf), "connect")
})

test_that("config checks epochs", {
  conf <- umap.defaults
  conf$n_epochs <- NA
  expect_error(umap.prep.config(conf), "epoch")
  conf$n_epochs <- -2
  expect_error(umap.prep.config(conf), "epoch")
})

test_that("config checks detect missing items", {
  conf <- umap.defaults
  conf$n_epochs <- NULL
  conf$random_state <- NULL
  expect_error(umap.prep.config(conf), "missing")
})

## ############################################################################
## Tests for settings a/b vs min_dist/spread

test_that("check gives warningg if a has a value but b is NA", {
  conf <- umap.defaults
  conf$a <- 1
  expect_warning(umap.prep.config(conf), "'b' is not")
})

test_that("check gives warning if b has a value but a is NA", {
  conf <- umap.defaults
  conf$b <- 1
  expect_warning(umap.prep.config(conf), "'a' is not")
})

test_that("check gives warning if attempting to over-specify min_dist", {
  conf <- umap.defaults
  conf$a <- conf$b <- 1
  expect_silent(umap.prep.config(conf))
  conf$min_dist <- 0.2
  expect_warning(umap.prep.config(conf), "non-default")
})

test_that("check gives warning if attempting to over-specify spread", {
  conf <- umap.defaults
  conf$a <- conf$b <- 1
  expect_silent(umap.prep.config(conf))
  conf$spread <- 2
  expect_warning(umap.prep.config(conf), "non-default")
})

test_that("check gives error when min_dist is too large", {
  conf <- umap.defaults
  conf$min_dist <- conf$spread <- 1
  expect_error(umap.prep.config(conf), "smaller")
})

test_that("check gives error when min_dist is too small", {
  conf <- umap.defaults
  conf$min_dist <- 0
  expect_error(umap.prep.config(conf), "min_dist")
})

## ############################################################################
## Tests for expected umap.config 

test_that("config class is validated", {
  expect_error(umap.prep.config.class(NULL))
  expect_error(umap.prep.config.class(list(n_epochs=20)))
})
