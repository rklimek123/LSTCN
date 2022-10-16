# Short-term Cognitive Network building block for LSTCN ensemble.
# Describes a S3 class stcn, with some complimentary functions.
library(MASS)
library(sigmoid)
library(types)

# Determine the initial W2 matrix.
get_W2 <- function(M = ? integer) {
  diag(M, M)
}

# Determine the initial B2 matrix.
get_B2 <- function(M = ? integer) {
  t(as.matrix(rep(0, M)))
}

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

# Activation function for the neurons.
activate <- function(x, inverse = FALSE) {
  sigmoid(x, method = "logistic", inverse = inverse)
}


# Create an empty instance of STCN.
stcn.new <- function() {
  stcn_obj <- list()
  class(stcn_obj) <- "stcn"
  stcn_obj
}


# Configure the starting parameters of 
init.stcn <- function(instance = ? list,
                      K = ? integer,
                      N = ? integer,
                      L = ? integer,
                      W1 = ? numeric,
                      B1 = ? numeric) {
  M = N * L
  if (!check_matrix_shape(W1, c(M, M))) {
    "W1 should be a MxM matrix"
  }
  else if (!check_matrix_shape(B1, c(1, M))) {
    "B1 should be a 1xM matrix"
  }
  else {
    append(instance,
           K = K,
           M = M,
           N = N,
           L = L,
           W1 = W1,
           B1 = B1,
           W2 = get_W2(M),
           B2 = get_B2(M),
           is_fitted = FALSE
        )
  }
}

# Get output of the chosen layer of neural network, given set X.
run_layer.stcn <- function(I = ? list,
                           X = ? numeric,
                           layer = ? integer) {
  if (!is.matrix(X) | dim(X)[2] != I$M) {
    "X should be a matrix with M columns"
  }
  else if (!is.element(layer, 1:2)) {
    "layer should be 1 or 2, there are 2 layers total in STCN"
  }
  else {
    if (layer == 1) {
      W <- I$W1
      B <- I$B1
    }
    else {
      W <- I$W2
      B <- I$B2
    }
    
    unbiased <- X %*% W
    activate(apply(unbiased, 1, function(r) r + B))
  }
}


# Predict on a trained model.
predict.stcn <- function(I = ? list,
                         X = ? numeric) {
  run_layer(I, run_layer(I, X, 1), 2)
}


# Fit the data to train the model.
# X - input data,
# Y - expected output,
# lambda - ridge regularization penalty.
fit.stcn <- function(I = ? list,
                     X = ? numeric,
                     Y = ? numeric,
                     lambda = ? numeric) {
  nrows <- dim(X)[1]
  if (!is.matrix(Y) | dim(Y) != c(nrows, I$M)) {
    "Y should be a matrix with M columns and same number of rows as X"
  }
  else if (lambda < 0) {
    "lambda should be a single value that is greater or equal to 0"
  }
  else {
    H <- run_layer(I, X, 1)
    phi <- cbind(H, rep(1, nrows))
    phiTphi <- t(phi) %*% phi
    diagLambdaOmega <- lambda * diag(phiTphi)
    # Q: standardization of the inner layer?
    #    Which is the inner, 1st or 2nd?
    lambdaOmega <- matrix(diagLambdaOmega, nrow=length(diagOmega))
    # Q: how is gamma (K+1)xM ?
    gamma <- ginv(phiTphi + lambdaOmega)
             %*% t(phi)
             %*% activate(Y, inverse = TRUE)
    I$W2 <- gamma[1:I$M,]
    I$B2 <- gamma[I$M + 1,]
    I
  }
}


