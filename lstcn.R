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
arrange_next_steps <- function(data, steps_ahead) {
  N <- ncol(data)
  L <- steps_ahead
  total_L <- nrow(data) %/% L
  
  #padded <- rbind(data, matrix(0, nrow = L - 1, ncol = N))
  
  t(apply(
      matrix(1:(total_L - 1)), 1, function(i)
        as.vector(t(data[(L*(i - 1) + 1):(L * i),]))
  ))
}

# Prepare time-series data in a form of (T*K) by N matrix
# to a list of length T, with pairs of K by N*L matrices (K by M),
# such that first element of the pair is the input
# and the second one is the expected output.
prepare_ts <- function(Xs = ? numeric,
                       no_patches = ? integer,
                       steps_ahead = ? integer) {
  dimXs <- dim(Xs)
  if (!is.matrix(Xs)
      | (dimXs[1] - steps_ahead) %% (no_patches * steps_ahead) != 0) {
    error_msg("Xs should be a (T*K*L + L) by N matrix")
  }
  else {
    T_ <- no_patches
    L <- steps_ahead
    K <- (dimXs[1] - steps_ahead) %/% T_ %/% L
    N <- dimXs[2]
    
    expanded <- arrange_next_steps(Xs, L)
    
    # Extract input and output.
    X <- expanded[-nrow(expanded),]
    Y <- expanded[-1,]
    
    # Partition inputs and outputs by time-patches.
    get_by_patches <- function(data) {
      lapply(1:T_, function(t)
        data[(K*(t - 1) + 1):min(K*t, nrow(data)),]
      )
    }
    
    patches_input <- get_by_patches(X)
    patches_output <- get_by_patches(Y)
    
    # Zip to pairs of input and output.
    zip_lists <- function(l1, l2) {
      lapply(1:length(l1), function(i)
          list(l1[[i]], l2[[i]])
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
  blocks <- rep(list(stcn.new()), no_patches)
  
  instance$T_ <- no_patches
  instance$N <- no_features
  instance$L <- steps_ahead
  instance$blocks <- blocks
  instance$is_fitted <- FALSE
  instance
}


# Fit the data to train the model.
# :param Xs: time-series data, used for input and output for STCN blocks.
#            Xs is a (K*L*T + 1) by N matrix, it needs to be prepared before.
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
    # Collecting subsequent training errors.
    fit_errors <- vector("numeric", length(I$blocks))
    
    ts_patches <- prepare_ts(Xs, I$T_, I$L)
    
    W1 <- W0
    B1 <- get_B1(M)
    for (i in 1:length(I$blocks)) {
      initialized <- init(I$blocks[[i]],
                          M, W1, B1)
      fit_out <- fit(initialized,
                   ts_patches[[i]],
                   lambda)
      fitted_block <- fit_out[[1]]
      fit_errors[i] <- fit_out[[2]]
      
      W1 <- get_updated_W1(fitted_block)
      B1 <- get_updated_B1(fitted_block)
      I$blocks[[i]] <- fitted_block
    }
    I$is_fitted <- TRUE
    list(I, fit_errors)
  }
}


# Predict on a trained model and return Mean Absolute Errors for each prediction.
predict.lstcn <- function(I = ? list,
                          Xs = ? numeric) {
  if (!I$is_fitted) {
    error_msg("Model hasn't been fitted")
  }
  else {
    prepared <- prepare_ts(Xs, 1, I$L)[[1]]
    iX <- prepared[[1]]
    oX <- prepared[[2]]
    
    outputs <- predict(I$blocks[[I$T_]], iX)
    get_MAE(outputs, oX)
  }
}
