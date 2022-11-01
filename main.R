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
  with_steps <- add_next_steps(data, steps_ahead)
  partitioned <- with_steps[seq(1, nrow(data), steps_ahead),]
  X <- mov_avg_smooth(
          partitioned[-nrow(partitioned),],
          smoothing_window)
  
  Y <- mov_avg_smooth(
          partitioned[-1,],
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
  training_data <- data[1:(nrow(data) - excess_rows),]
  
  W0 <- get_W0(data, predict_steps,
               smoothing_window,
               lambda, sigma)
  I <- fit(I, data, lambda, W0)
  predict(I, data)
}
