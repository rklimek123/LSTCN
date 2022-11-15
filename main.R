# LSTCN implementation from paper:
# "Long short-term cognitive networks"
#     https://doi.org/10.1007/s00521-022-07348-5
#     Authors:
#         Gonzalo Nápoles
#         Isel Grau
#         Agnieszka Jastrzębska
#         Yamisleydi Salgueiro
# 
# Implementation:
#     Rafał Klimek
# 
# Warsaw University of Technology,
# Faculty of Mathematics and Information Science
# 2022W Introduction to Machine Learning course project.

library(pracma)
library(types)
source('interpolate.R')
source('lstcn.R')
source('ts_smooth.R')

get_W0 <- function(data, steps_ahead,
                   smoothing_window,
                   lambda, sigma) {
  with_steps <- arrange_next_steps(data, steps_ahead)
  
  X <- mov_avg_smooth(
          with_steps[-nrow(with_steps),],
          smoothing_window)
  
  Y <- mov_avg_smooth(
          with_steps[-1,],
          smoothing_window)
  
  base <- gamma_training(X, Y, lambda)
  
  apply(base, 1, function(row) {
    sapply(row, function(val) rnorm(1, mean=val, sd=sigma))
  })
}

# Trim a matrix to a number of rows divisible by A*B,
# with B extra steps on top of that.
# Trims starting rows, keeping the latter intact.
trim_to_multiple <- function(data,
                             A = ? integer,
                             B = ? integer) {
  # Trim data to multiples of (A * B) (and extra B steps)
  # to a form digestible by the model.
  excess_rows <- (nrow(data) - B) %% (A * B)
  
  # Additional B steps to the left taken, because we want to keep the last row out
  # of the training process, only using it as a gold-standard output reference,
  # hence the "+ B" part.
  as.matrix(data[(excess_rows + 1):nrow(data),])
}

run_lstcn <- function(data,
                      no_patches = ? integer,
                      predict_steps = ? integer,
                      lambda = ? numeric,
                      smoothing_window = ? integer,
                      sigma = ? numeric) {
  I <- lstcn.new()
  I <- init(I, no_patches, ncol(data), predict_steps)
  
  training_data <- trim_to_multiple(data, no_patches, predict_steps)
  fit_data <- fit_min_max_even(training_data)
  
  set.seed(4*8*15*16*23*42)
  
  W0 <- get_W0(fit_data, predict_steps,
               smoothing_window,
               lambda, sigma)
  Imaes <- fit(I, fit_data, lambda, W0)
  
  # If needed, can print out or return the whole errors vector.
  list(Imaes[[1]], mean(Imaes[[2]]))
}

# Run tests with the method described in chapter 5 of the paper.
# data - consecutive time-steps in each row, N columns (number of features).
# return: 
run_test <- function(data, to_predict) {
  L <- to_predict
  
  interpolated <- interpolate_matrix(data)
  fit_data <- fit_min_max_even(interpolated)
  
  total_steps <- nrow(fit_data)
  
  # 20% of data is for testing.
  for_testing <- total_steps %/% 5
  
  training_steps <- as.matrix(
    fit_data[1:(total_steps - for_testing),]
  )
  test_steps <- as.matrix(
    fit_data[(total_steps - for_testing + 1):total_steps,]
  )
  test_steps <- trim_to_multiple(test_steps, 1, L)
  
  bestT <- NaN
  bestLambda <- NaN
  bestError <- Inf
  
  for (T_ in 1:10) {
    for (lambda_ in 10**c(-3:-1, 1:3)) {
      cat("Run LSTCN for", T_, "time-patches, lambda =", lambda_, "\n")
      
      mMae <- run_lstcn(training_steps,
                        no_patches = T_,
                        predict_steps = L,
                        lambda = lambda_,
                        smoothing_window = 100,
                        sigma = 0.05)
      cat("Training MAE:", mMae[[2]], "\n")
      
      m <- mMae[[1]]
      MAE <- predict(m, test_steps)
      cat("Mean Absolute Error:", MAE, "\n\n")
      if (MAE < bestError) {
        bestError <- MAE
        bestT <- T_
        bestLambda <- lambda_
      }
    }
  }
  
  cat("Test finished")
  cat("Error:", bestError, ", T =", bestT, ", lambda =", bestLambda)
}
