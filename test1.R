source('main.R')

data <- matrix(1:30000015, ncol=3, byrow=TRUE)

# debug(run_lstcn)(data,
#           no_patches = 3,
#           predict_steps = 2,
#           lambda = 0.05,
#           smoothing_window = 3,
#           sigma = 1)

#debug(run_lstcn)
run_lstcn(data,
          no_patches = 10,
          predict_steps = 2,
          lambda = 0.05,
          smoothing_window = 100,
          sigma = 1)

