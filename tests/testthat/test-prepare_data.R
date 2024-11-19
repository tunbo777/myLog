test_that("prepare_data handles input correctly", {
  # 创建测试数据
  X <- matrix(c(1, 2, NA, 4, 5, 6), ncol = 2)
  y <- c(1, 0, 1)

  # 如果函数没有抛出错误，验证其返回值
  result <- prepare_data(X, y, standardize = TRUE)
  expect_type(result, "list")
  expect_named(result, c("X", "y"))

  # 检查返回值是否正确
  expect_equal(nrow(result$X), length(result$y))
})
