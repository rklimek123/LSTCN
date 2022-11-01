# Generics used both by stcn and lstcn classes.
source('errors.R')


# Initialize the object with provided values.
init <- function(object, ...) UseMethod("init")
init.default <- function(object, ...) gen_error("init", class(object))


# Fit the data to train the model.
fit <- function(object, ...) UseMethod("fit")
fit.default <- function(object, ...) gen_error("fit", class(object))


# Predict on a trained model.
predict <- function(object, ...) UseMethod("predict")
predict.default <- function(object, ...) gen_error("predict", class(object))
