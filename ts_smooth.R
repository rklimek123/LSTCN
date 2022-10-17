# Time-series smoothing using moving average method.
library(types)

mov_avg_smooth <- function(X = ? numeric,
                           window = ? integer) {
  prefix_sum <- rbind(rep(0, dim(X)[2]), X)
  nrow_prefix_sum <- dim(prefix_sum)[1]
  for (i in 2:nrow_prefix_sum) {
    prefix_sum[i,] <- prefix_sum[i,] + prefix_sum[i - 1,]
  }
  
  smoothed <- prefix_sum[2:nrow_prefix_sum,]
  for (i in 2:nrow_prefix_sum) {
    begin_range <- max(1, i - window)
    smoothed[i,] <-
      (prefix_sum[i,] - prefix_sum[begin_range,]) / (i - begin_range)
  }
  
  smoothed
}
