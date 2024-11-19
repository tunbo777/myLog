#' Prepare and preprocess data for logistic regression
#'
#' This function validates and preprocesses input data for logistic regression,
#' including handling missing values and standardizing numeric features.
#'
#' @param X A numeric matrix where each row is an observation and each column is a feature.
#' @param y An optional numeric vector of binary responses (0, 1) corresponding to each row of \code{X}.
#' @param standardize A logical value indicating whether to standardize numeric columns (default is \code{TRUE}).
#' @param na_action A character string specifying how to handle missing values. Options are
#' \code{"remove"} (default) to remove rows with missing values, or \code{"mean"} to replace missing values with column means.
#'
#' @return A list containing:
#' \describe{
#'   \item{X}{The preprocessed feature matrix.}
#'   \item{y}{The preprocessed response vector (if provided).}
#' }
#'
#' @examples
#' X <- matrix(c(1, 2, NA, 4, 5, 6), ncol = 2)
#' y <- c(0, 1, 0)
#' prepared_data <- prepare_data(X, y, standardize = TRUE, na_action = "mean")
#' print(prepared_data)
#'
#' @export


prepare_data <- function(X, y = NULL, standardize = TRUE, na_action = "remove") {
  #Check whether X is matrix
  if (!is.matrix(X)) stop("X must be a matrix.")

  #check whether y is binary if it is not null
  if (!is.null(y)) {
    if (!is.numeric(y) || length(unique(y)) != 2) stop("y must be binary (0, 1).")
    if (nrow(X) != length(y)) stop("Number of rows in X must match length of y.")
  }

  #handling missing value
  if (na_action == "remove") {
    complete_cases <- stats::complete.cases(X)
    X <- X[complete_cases, , drop = FALSE]
    if (!is.null(y)) y <- y[complete_cases]
  } else if (na_action == "mean") {
    X <- apply(X, 2, function(col) {
      if (is.numeric(col)) {
        col[is.na(col)] <- mean(col, na.rm = TRUE)
      }
      return(col)
    })
  }

  #standardize
  if (standardize) {
    X <- scale(X)
  }

  return(list(X = X, y = y))
}
