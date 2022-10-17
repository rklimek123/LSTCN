# Long Short-Term Cognitive Network, ensemble of STCN blocks.
# Describes a S3 class lstcn, with some complimentary functions.
library(types)
source('stcn.R')

# Determine the initial B1 matrix of the first STCN block.
get_B1 <- function(M = ? integer) {
  t(as.matrix(rep(0, M)))
}

# Prepare time-series data in a form of K*T by N matrix
# to a list of length T, consisting of K by N*L matrices (K by M).
prepare_ts <- function(Xs = ? numeric,
                       no_patches = ? integer,
                       steps_ahead = ? integer) {
  dimXs <- dim(Xs)
  if (!is.matrix(Xs) | dimXs[1] %% no_patches != 0) {
    "Xs should be a K*T by N matrix"
  }
  else {
    T_ <- no_patches
    K <- dimXs[1] %/% T_
    N <- dimXs[2]
    L <- steps_ahead
    
    chopped <- lapply(1:T_, function(i)
      Xs[(K * i):(K * (i+1) - 1),]
    )
    
    # Expand one time-series block to a form digestible by STCN.
    expand_X <- function(X) {
      # Q: Is this how we are supposed to do it?
      filled <- rbind(X, matrix(rep(0, (L - 1) * N), nrow = L - 1))
      expanded <- sapply(1:K, function(i) {
        chunk <- filled[i:(i+L-1),]
        as.vector(t(chunk))
      })
      expanded
    }
    
    lapply(chopped, expand_X)
  }
}


# Create an empty instance of LSTCN.
lstcn.new <- function() {
  lstcn_obj <- list()
  class(lstcn_obj) <- "lstcn"
  lstcn_obj
}


# Configure the starting parameters of LSTCN.
# no_patches - T_
# steps_in_patch - K
# no_features - N
# steps_ahead - L
init.lstcn <- function(instance = ? list,
                       no_patches = ? integer,
                       steps_in_patch = ? integer,
                       no_features = ? integer,
                       steps_ahead = ? integer) {
  # Q: The last block wouldn't have an expected output to fit into.
  #    Do I think correctly?
  blocks <- rep(stcn.new(), no_patches - 1)
  
  append(instance,
         T_ = no_patches,
         K = steps_in_patch,
         N = no_features,
         L = steps_ahead,
         blocks = blocks,
         is_fitted = FALSE)
}


# Predict on a trained model.
predict.lstcn <- function(I = ? list,
                          Xs = ? numeric) {
  # Q: What to predict? How to predict? So many questions!
  # TODO: everything
}


# Fit the data to train the model.
# Xs - time-series data, used for input and output for blocks.
#      Xs is a K*T by N matrix, so it needs to be prepared before.
# lambda - ridge regularization penalty,
# W0 - W1 matrix for the first block
#      (before-hand knowledge supplied by experts).
fit.lstcn <- function(I = ? list,
                      Xs = ? numeric,
                      lambda = ? numeric,
                      W0 = ? numeric) {
  M <- I$N * I$L
  if (!check_matrix_shape(W0, c(M, M))) {
    "W0 should be a M by M matrix"
  }
  else {
    ts_blocks <- prepare_ts(Xs, I$T_, I$L)
    
    W1 <- W0
    B1 <- get_B1(M)
    for (i in 1:length(I$blocks)) {
      initialized <- init(I$blocks[[i]],
                          I$K, M,
                          W1, B1)
      fitted_block <- fit(initialized,
                          ts_blocks[[i]],
                          ts_blocks[[i + 1]],
                          lambda)
      W1 <- fitted_block$W2
      B1 <- fitted_block$B2
      I$blocks[[i]] <- fitted_block
    }
    I
  }
}


