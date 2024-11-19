# myLog

The goal of myLog is to train logistic regression models. It provides function for preprocessing data, training models, and making predictions.

## Installation

You can install the development version of myLog like so:

``` r
# Install devtools if not already installed
install.packages("devtools")

# Install myLog from GitHub
devtools::install_github("tunbo777/myLog")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(myLog)

# Generate sample data
set.seed(123)
X <- matrix(rnorm(100), ncol = 5)
y <- sample(0:1, 20, replace = TRUE)

# Train a logistic regression model
model <- train_logistic(X, y)

# Make predictions
predictions <- predict_logistic(model, X)

# View coefficients
print(model$coefficients)
```

## Documentation

For more detailed documentation, use the help files:

``` r
vignette("mylog")
```
