#' Predict using a logistic regression model
#'
#' This function generates predictions for new data using a trained logistic regression model.
#'
#' @param model A logistic regression model object trained by \code{train_logistic}.
#' @param X A numeric matrix of new data where each row is an observation.
#' @param threshold A numeric value between 0 and 1. Predictions with probabilities
#' above this threshold are classified as 1, otherwise 0 (default is 0.5).
#'
#' @return A list with two components:
#' \describe{
#'   \item{probs}{A numeric vector of predicted probabilities.}
#'   \item{preds}{A numeric vector of predicted classes (0 or 1).}
#' }
#'
#' @examples
#' X <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)
#' y <- c(0, 1, 0)
#' model <- train_logistic(X, y)
#' result <- predict_logistic(model, X)
#' print(result$preds)
#'
#' @export

predict_logistic <- function(model, X, threshold = 0.5) {
  #Check X
  if (!is.matrix(X)) stop("X must be a matrix.")
  X <- scale(X)
  X <- cbind(1, X)

  #Compute P
  probs <- 1 / (1 + exp(-X %*% model$coefficients))

  #Predict
  preds <- ifelse(probs >= threshold, 1, 0)
  return(list(probs = probs, preds = preds))
}
