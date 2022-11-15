# Short-Term Cognitive Network building block for LSTCN ensemble.
# Describes a S3 class stcn, with some complimentary functions.
library(MASS)
library(types)

source('errors.R')
source('gen_shared.R')
source('gen_stcn.R')
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
activate <- function(x, inv = FALSE) {
  if (inv) {
    log(x / (1 - x))
  }
  else {
    1 / (1 + exp(-x))
  }
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
    error_msg("X and Y should have corresponding number of rows")
  }
  else {
    xTx <- t(X) %*% X
    # TODO: standardization of the inner layer
    # I'm not sure whether standardization is necessary in (9).
    # Q: Standardization in general, it's advised to look into that.
    #    Do we need to apply the (x-mean(x))/sd(x) formula, or fit values into (0, 1)?
    lambdaOmega <- vector_to_diag(lambda * diag(xTx))
    inversed <- ginv(xTx + lambdaOmega)
    
    xY <- t(X) %*% activate(Y, inv = TRUE)
    gamma <- inversed %*% xY
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
    error_msg("W1 should be a MxM matrix")
  }
  else if (!check_matrix_shape(B1, c(1, M))) {
    error_msg("B1 should be a 1xM matrix")
  }
  else {
    instance$M <- M
    instance$W1 <- W1
    instance$B1 <- B1
    instance$W2 <- get_W2(M)
    instance$B2 <- get_B2(M)
    instance$is_fitted <- FALSE
    instance
  }
}

# Get output of the chosen layer of neural network, given set X.
run_layer.stcn <- function(I = ? list,
                           X = ? numeric,
                           layer = ? integer) {
  if (!is.matrix(X) | any(dim(X)[2] != I$M)) {
    error_msg("[X] should be a matrix with M columns")
  }
  else if (!is.element(layer, 1:2)) {
    error_msg("[layer] should be 1 or 2, there are 2 layers total in STCN")
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
    activate(t(apply(unbiased, 1, function(r) r + B)))
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
  if (!I$is_fitted) {
    error_msg("Model hasn't been fitted")
  }
  else {
    run_layer(I, run_layer(I, X, 1), 2)
  }
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
  if (!is.matrix(Y) | any(dim(Y) != c(nrows, I$M))) {
    error_msg("[Y] should be a matrix with M columns and same number of rows as [X]")
  }
  else if (lambda < 0) {
    error_msg("[lambda] should be a single value that is greater or equal to 0")
  }
  else {
    H <- run_layer(I, X, 1)
    phi <- cbind(H, rep(1, nrows))
    
    gamma <- gamma_training(phi, Y, lambda)
    I$W2 <- gamma[1:I$M,]
    I$B2 <- t(as.matrix(gamma[I$M + 1,]))
    
    mae <- get_MAE(run_layer(I, H, 2), Y)
    list(I, mae)
  }
}

# Fit the data to train the model.
# data - pair, input and output
# lambda - ridge regularization penalty.
fit.stcn <- function(I = ? list,
                     data = ? list,
                     lambda = ? numeric) {
  Imae <- iteration(I, data[[1]], data[[2]], lambda)
  I <- Imae[[1]]
  I$is_fitted <- TRUE
  list(I, Imae[[2]])
}
