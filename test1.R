source('main.R')

data <- matrix(1:3000, ncol=3, byrow=TRUE)

run_lstcn(data,
          no_patches = 3,
          predict_steps = 2,
          lambda = 0.05,
          smoothing_window = 3,
          sigma = 1)
