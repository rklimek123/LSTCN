# Long Short-Term Cognitive Network, ensemble of STCN blocks.
# Describes a S3 class lstcn, with some complimentary functions.
library(types)
source('stcn.R')

# Determine the initial W1 matrix of the first STCN block.
get_W1 <- function(M = ? integer) {
  diag(M, M)
}

# Determine the initial B1 matrix of the first STCN block.
get_B1 <- function(M = ? integer) {
  t(as.matrix(rep(0, M)))
}


# Create an empty instance of LSTCN.
lstcn.new <- function() {
  lstcn_obj <- list()
  class(lstcn_obj) <- "lstcn"
  lstcn_obj
}


# Configure the starting parameters of LSTCN.
# no_patches - T
# steps_in_patch - K
# no_features - N
# steps_ahead - L
init.lstcn <- function(instance = ? list,
                       no_patches = ? integer,
                       steps_in_patch = ? integer,
                       no_features = ? integer,
                       steps_ahead = ? integer) {
  blocks <- rep(stcn.new(), no_patches)
  
  append(instance,
         Ts = no_patches,
         K = steps_in_patch,
         N = no_features,
         L = steps_ahead,
         blocks = blocks,
         is_fitted = FALSE)
}


# Predict on a trained model.
predict.stcn <- function(I = ? list,
                         X = ? numeric) {
  # TODO: everything
}


# Fit the data to train the model.
# X - input data,
# Y - expected output,
# lambda - ridge regularization penalty.
fit.stcn <- function(I = ? list,
                     X = ? numeric,
                     Y = ? numeric,
                     lambda = ? numeric) {
  # NEXT IN LINE TO DO
  # TODO: everything
}


