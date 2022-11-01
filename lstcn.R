# Long Short-Term Cognitive Network, ensemble of STCN blocks.
# Describes a S3 class lstcn, with some complimentary functions.
library(types)

source('errors.R')
source('gen_shared.R')
source('stcn.R')

# Determine the initial B1 matrix of the first STCN block.
get_B1 <- function(M = ? integer) {
  t(as.matrix(rep(0, M)))
}

# For each step, add L - 1 next steps.
add_next_steps <- function(data, steps_ahead) {
  N <- ncol(data)
  L <- steps_ahead
  total <- nrow(data)
  
  padded <- rbind(data, matrix(0, nrow = L - 1, ncol = N))
  t(apply(
      matrix(1:total), 1, function(i)
        as.vector(t(padded[i:(i + L - 1),]))
  ))
}

# Prepare time-series data in a form of K*T*L by N matrix
# to a list of length T, comprised of lists
# of pairs of K by N*L matrices (K by M),
# such that first element of the pair is the input
# and the second one is the expected output.
prepare_ts <- function(Xs = ? numeric,
                       no_patches = ? integer,
                       steps_ahead = ? integer) {
  dimXs <- dim(Xs)
  if (!is.matrix(Xs)
      | dimXs[1] %% no_patches != 0
      | dimXs[1] %% steps_ahead != 0) {
    error_msg("Xs should be a K*L*T by N matrix")
  }
  else {
    T_ <- no_patches
    total_steps <- dimXs[1]
    K <- total_steps %/% T_ %/% L
    N <- dimXs[2]
    L <- steps_ahead
    
    expanded <- add_next_steps(Xs, L)
    
    # Separate the data by strides.
    by_strides <- lapply(0:(L - 1), function(stride)
      expanded[seq(1 + stride, total_steps, L),])
    
    # Extract inputs and outputs.
    inputs <- lapply(by_strides, function(matr) matr[-nrow(by_strides),])
    outputs <- lapply(by_strides, function(matr) matr[-1,])
    
    # Partition inputs and outputs by time-patches.
    get_by_patches <- function(data) {
      lapply(1:T_, function(t)
        lapply(data, function(stride_data)
          stride_data[(K*(t - 1) + 1):min(K*t, nrow(stride_data)),]
        )
      )
    }
    
    patches_input <- get_by_patches(inputs)
    patches_output <- get_by_patches(outputs)
    
    # Zip to pairs of input and output.
    zip_lists <- function(l1, l2) {
      lapply(1:length(l2), function(i)
        lapply(1:length(l2[[i]]), function(j)
          list(l1[[i]][j], l2[[i]][j])
        )
      )
    }
    
    zip_lists(patches_input, patches_output)
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
# no_features - N
# steps_ahead - L
init.lstcn <- function(instance = ? list,
                       no_patches = ? integer,
                       no_features = ? integer,
                       steps_ahead = ? integer) {
  blocks <- rep(stcn.new(), no_patches)
  
  instance$T_ <- no_patches
  instance$N <- no_features
  instance$L <- steps_ahead
  instance$blocks <- blocks
  instance$is_fitted <- FALSE
  instance
}


# Fit the data to train the model.
# :param Xs: time-series data, used for input and output for STCN blocks.
#            Xs is a K*L*T by N matrix, it needs to be prepared before.
# :param lambda: Ridge regularization penalty,
# :param W0: W1 matrix for the first STCN block
#            (before-hand knowledge supplied by experts).
# :return: Prediction for L next steps after the supplied data.
fit.lstcn <- function(I = ? list,
                      Xs = ? numeric,
                      lambda = ? numeric,
                      W0 = ? numeric) {
  M <- I$N * I$L
  if (!check_matrix_shape(W0, c(M, M))) {
    error_msg("W0 should be a M by M matrix")
  }
  else if (ncol(Xs) != I$N) {
    error_msg("Xs should have the predetermined number of features (columns).")
  }
  else {
    ts_patches <- prepare_ts(Xs, I$T_, I$L)
    
    W1 <- W0
    B1 <- get_B1(M)
    for (i in 1:length(I$blocks)) {
      initialized <- init(I$blocks[[i]],
                          M, W1, B1)
      fitted_block <- fit(initialized,
                          ts_patches[[i]],
                          lambda)
      W1 <- get_updated_W1(fitted_block)
      B1 <- get_updated_B1(fitted_block)
      I$blocks[[i]] <- fitted_block
    }
    I$is_fitted <- TRUE
    I
  }
}


# Predict on a trained model.
predict.lstcn <- function(I = ? list,
                          Xs = ? numeric) {
  if (!I$is_fitted) {
    error_msg("Model hasn't been fitted")
  }
  else {
    last_steps <- matrix(Xs[(nrow(Xs) - L + 1):nrow(Xs),], nrow = 1, byrow = TRUE)
    predict(I$blocks[[I$T_]], last_steps)
  }
}
