source('errors.R')


# Get output of the chosen layer of neural network, given set X.
run_layer <- function(object, ...) UseMethod("run_layer")
run_layer.default <- function(object, ...) gen_error("run_layer", class(object))


# Acquire updated W1 knowledge, based on W1 and W2 of the block.
get_updated_W1 <- function(object, ...) UseMethod("get_updated_W1")
get_updated_W1.default <- function(object, ...) gen_error("get_updated_W1", class(object))


# Acquire updated B1 knowledge, based on B1 and B2 of the block.
get_updated_B1 <- function(object, ...) UseMethod("get_updated_B1")
get_updated_B1.default <- function(object, ...) gen_error("get_updated_B1", class(object))


# Run a single iteration of STCN.
iteration <- function(object, ...) UseMethod("iteration")
iteration.default <- function(object, ...) gen_error("iteration", class(object))
