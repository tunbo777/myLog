test_that("predict_logistics works correctly", {
  # 创建模拟数据
  set.seed(123)
  X <- matrix(rnorm(100), ncol = 5)
  y <- sample(0:1, 20, replace = TRUE)

  # 训练模型
  model <- train_logistic(X, y)

  # 获取预测值
  predictions <- predict_logistic(model, X)

  # 确保返回值是一个 list
  expect_type(predictions, "list")

  # 如果 list 中有概率值，验证它们在 [0, 1] 范围内
  if (!is.null(predictions$probabilities)) {
    expect_true(all(predictions$probabilities >= 0 & predictions$probabilities <= 1))
  }
})
