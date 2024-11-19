#' Train a logistic regression model
#'
#' This function trains a logistic regression model using gradient descent.
#' It supports optional L2 regularization.
#'
#' @param X A numeric matrix where each row is an observation and each column is a feature.
#' @param y A numeric vector of binary responses (0, 1) corresponding to each row of \code{X}.
#' @param lambda Regularization parameter for L2 penalty (default is 0).
#' @param penalty A character string specifying the type of regularization. Default is \code{"none"}.
#' @param tol A numeric value specifying the tolerance for convergence (default is \code{1e-6}).
#' @param max_iter Maximum number of iterations for gradient descent (default is \code{100}).
#'
#' @return A list containing the trained model, including:
#' \describe{
#'   \item{coefficients}{The estimated coefficients of the logistic regression model.}
#'   \item{converged}{A logical indicating whether the optimization converged.}
#'   \item{iterations}{The number of iterations performed.}
#' }
#'
#' @examples
#' X <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)
#' y <- c(0, 1, 0)
#' model <- train_logistic(X, y)
#' print(model$coefficients)
#'
#' @export

train_logistic <- function(X, y, lambda = 0, penalty = "none", tol = 1e-6, max_iter = 100) {
  #implement preprocess data
  prepared_data <- prepare_data(X, y, standardize = TRUE, na_action = "remove")
  X <- prepared_data$X
  y <- prepared_data$y
  X <- cbind(1, X)
  beta <- rep(0, ncol(X))  # 参数初始值

  #logistic process
  for (iter in 1:max_iter) {
    P <- 1 / (1 + exp(-X %*% beta))
    grad <- t(X) %*% (P - y)
    beta <- beta - 0.01 * grad

    #check whether convergent
    if (sqrt(sum(grad^2)) < tol) {
      break
    }
  }

  #return model
  model <- list(
    coefficients = beta,
    converged = TRUE,
    iterations = iter
  )
  class(model) <- "logistic_model"
  return(model)
}
