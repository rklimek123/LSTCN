# Utility functions for working with matrices and vectors.

# Check if the argument is a matrix with desired shape
check_matrix_shape <- function(X = ? numeric, shape = ? integer) {
  is.matrix(X) & dim(X) == shape
}

# Check if the argument is a vector with desired length
check_vector_length <- function(X = ? numeric, n = ? integer) {
  is.vector(X) & length(X) == n
}

# Standardize the vector X.
# Returns a standardized vector with the previous mean and standard deviation.
standardize <- function(X = ? numeric) {
  if(!is.vector(X)) {
    "X should be a vector"
  }
  else {
    X_mean <- mean(X)
    X_sd <- sd(X)
    V <- (X - X_mean) / X_sd
    list(V = V, prev_mean = X_mean, prev_sd = X_sd)
  }
}

# Convert a vector into a diagonal matrix,
# with this vector as its diagonal.
vector_to_diag <- function(V) {
  result <- matrix(0, nrow=length(V), ncol=length(V))
  diag(result) <- V
  result
}
