% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_data.R
\name{prepare_data}
\alias{prepare_data}
\title{Prepare and preprocess data for logistic regression}
\usage{
prepare_data(X, y = NULL, standardize = TRUE, na_action = "remove")
}
\arguments{
\item{X}{A numeric matrix where each row is an observation and each column is a feature.}

\item{y}{An optional numeric vector of binary responses (0, 1) corresponding to each row of \code{X}.}

\item{standardize}{A logical value indicating whether to standardize numeric columns (default is \code{TRUE}).}

\item{na_action}{A character string specifying how to handle missing values. Options are
\code{"remove"} (default) to remove rows with missing values, or \code{"mean"} to replace missing values with column means.}
}
\value{
A list containing:
\describe{
  \item{X}{The preprocessed feature matrix.}
  \item{y}{The preprocessed response vector (if provided).}
}
}
\description{
This function validates and preprocesses input data for logistic regression,
including handling missing values and standardizing numeric features.
}
\examples{
X <- matrix(c(1, 2, NA, 4, 5, 6), ncol = 2)
y <- c(0, 1, 0)
prepared_data <- prepare_data(X, y, standardize = TRUE, na_action = "mean")
print(prepared_data)

}
