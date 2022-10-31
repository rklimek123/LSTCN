# Short-Term Cognitive Network building block for LSTCN ensemble.
# Describes a S3 class stcn, with some complimentary functions.
library(MASS)
library(sigmoid)
library(types)
source('util.R')

# Determine the initial W2 matrix.
get_W2 <- function(M = ? integer) {
  diag(M, M)
}

# Determine the initial B2 matrix.
get_B2 <- function(M = ? integer) {
  t(as.matrix(rep(0, M)))
}

# Activation function for the neurons.
activate <- function(x, inverse = FALSE) {
  sigmoid(x, method = "logistic", inverse = inverse)
}

# Psi function, aggregate *1 and *2 matrices to propagate knowledge.
knowledge_updater <- function(M1, M2) {
  # Matrix-wise max
  first_geq <- M1 >= M2
  M <- M2
  M[first_geq] <- M1[first_geq]
  
  # tanh
  tanh(M)
}

# Run training function as described in 4.2 (8)(9)
gamma_training <- function(X, Y, lambda) {
  if (nrow(X) != nrow(Y)) {
    "X and Y should have corresponding number of rows"
  }
  else {
    xTx <- t(X) %*% X
    # TODO: standardization of the inner layer
    # I'm not sure whether standardization is necessary in (9).
    # Q: Standardization in general, it's advised to look into that.
    lambdaOmega <- vector_to_diag(lambda * diag(xTx))
    gamma <- ginv(xTx + lambdaOmega)
             %*% t(X)
             %*% activate(Y, inverse = TRUE)
    gamma
  }
}


# Create an empty instance of STCN.
stcn.new <- function() {
  stcn_obj <- list()
  class(stcn_obj) <- "stcn"
  stcn_obj
}


# Configure the starting parameters of STCN.
init.stcn <- function(instance = ? list,
                      M = ? integer,
                      W1 = ? numeric,
                      B1 = ? numeric) {
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

# Acquire updated W1 knowledge, based on W1 and W2 of the block.
get_updated_W1.stcn <- function(I = ? list) {
  knowledge_updater(I$W1, I$W2)
}

# Acquire updated B1 knowledge, based on B1 and B2 of the block.
get_updated_B1.stcn <- function(I = ? list) {
  knowledge_updater(I$B1, I$B2)
}

# Predict on a trained model.
predict.stcn <- function(I = ? list,
                         X = ? numeric) {
  run_layer(I, run_layer(I, X, 1), 2)
}

# One iteration of the fitting process.
# X - input
# Y - expected output
# lambda - ridge regularization penalty.
iteration.stcn <- function(I = ? list,
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
    
    gamma <- gamma_training(phi, Y, lambda)
    I$W2 <- gamma[1:I$M,]
    I$B2 <- gamma[I$M + 1,]
    I
  }
}

# Fit the data to train the model.
# data - list of pairs, input and expected
# lambda - ridge regularization penalty.
fit.stcn <- function(I = ? list,
                     data = ? list,
                     lambda = ? numeric) {
  for (sample in data) {
    I <- iteration(I, sample[[1]], sample[[2]], lambda)
    I$W1 <- get_updated_W1(I)
    I$B1 <- get_updated_B1(I)
  }
  
  I
}
