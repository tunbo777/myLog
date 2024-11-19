
test_that("train_logistic works", {
  X <- matrix(rnorm(100 * 2), ncol = 2)
  y <- ifelse(X[, 1] + X[, 2] > 0, 1, 0)
  model <- train_logistic(X, y)
  expect_true(is.list(model))
  expect_equal(length(model$coefficients), ncol(X) + 1)
})
