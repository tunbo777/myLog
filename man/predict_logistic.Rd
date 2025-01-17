% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict_logistics.R
\name{predict_logistic}
\alias{predict_logistic}
\title{Predict using a logistic regression model}
\usage{
predict_logistic(model, X, threshold = 0.5)
}
\arguments{
\item{model}{A logistic regression model object trained by \code{train_logistic}.}

\item{X}{A numeric matrix of new data where each row is an observation.}

\item{threshold}{A numeric value between 0 and 1. Predictions with probabilities
above this threshold are classified as 1, otherwise 0 (default is 0.5).}
}
\value{
A list with two components:
\describe{
  \item{probs}{A numeric vector of predicted probabilities.}
  \item{preds}{A numeric vector of predicted classes (0 or 1).}
}
}
\description{
This function generates predictions for new data using a trained logistic regression model.
}
\examples{
X <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)
y <- c(0, 1, 0)
model <- train_logistic(X, y)
result <- predict_logistic(model, X)
print(result$preds)

}
