# Utility functions for working with matrices and vectors.
source('errors.R')

# Check if the argument is a matrix with desired shape
check_matrix_shape <- function(X = ? numeric, shape = ? integer) {
  is.matrix(X) & all(dim(X) == shape)
}

# Check if the argument is a vector with desired length
check_vector_length <- function(X = ? numeric, n = ? integer) {
  is.vector(X) & length(X) == n
}

# Standardize the vector X.
# Returns a standardized vector with the previous mean and standard deviation.
standardize <- function(X = ? numeric) {
  if(!is.vector(X)) {
    error_msg("X should be a vector")
  }
  else {
    X_mean <- mean(X)
    X_sd <- sd(X)
    V <- (X - X_mean) / X_sd
    list(V = V, prev_mean = X_mean, prev_sd = X_sd)
  }
}

get_begin_end_for_minmax <- function(X = ? numeric, epsilon = ? numeric) {
  begin <- min(X) - epsilon
  end <- max(X) + epsilon
  c(begin, end)
}

fit_min_max <- function(X = ? numeric, epsilon = ? numeric) {
  minmax_range <- get_begin_end_for_minmax(X, epsilon)
  begin <- minmax_range[1]
  end <- minmax_range[2]
  (X - begin) / (end - begin)
}

# Fit min_max, but before that, fit each dimension to [0, 1] independently. 
fit_min_max_even <- function(X = ? numeric, epsilon = 1e-6) {
  even_fit <- apply(X, 2, fit_min_max, epsilon = 0)
  fit_min_max(even_fit, epsilon)
}

# Convert a vector into a diagonal matrix,
# with this vector as its diagonal.
vector_to_diag <- function(V) {
  result <- matrix(0, nrow=length(V), ncol=length(V))
  diag(result) <- V
  result
}

# The final error is the Mean Absolute Error.
# For each data dimension an error is calculated, then a mean is taken, and
# then a mean is taken for all observations.
get_MAE <- function(real, expected) {
  abs_errors <- abs(real - expected)
  obs_means <- apply(abs_errors, 1, mean)
  mean(obs_means)
}
