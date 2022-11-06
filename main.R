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

run_lstcn <- function(data,
                      no_patches = ? integer,
                      predict_steps = ? integer,
                      lambda = ? numeric,
                      smoothing_window = ? integer,
                      sigma = ? numeric) {
  I <- lstcn.new()
  I <- init(I, no_patches, ncol(data), predict_steps)
  
  # Trim data to multiples of T*L for training the model.
  excess_rows <- nrow(data) %% (no_patches * predict_steps)
  
  # Additional L steps to the left taken, because we want to keep the last row out
  # of the training process, as its output will be the result of the prediction.
  training_data <- data[max(excess_rows - predict_steps + 1, 1):nrow(data),]
  
  min_max_coeff <- get_begin_end_for_minmax(training_data)
  min_data <- min_max_coeff[1]
  max_data <- min_max_coeff[2]
  fit_data <- fit_min_max(training_data)
  
  set.seed(4*8*15*16*23*42)
  
  W0 <- get_W0(fit_data, predict_steps,
               smoothing_window,
               lambda, sigma)
  I <- fit(I, fit_data, lambda, W0)
  unfit_min_max(predict(I, fit_data), min_data, max_data)
}
