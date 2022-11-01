# Time-series smoothing using moving average method.
library(types)

mov_avg_smooth <- function(X = ? numeric,
                           window = ? integer) {
  prefix_sum <- rbind(rep(0, ncol(X)), X)
  nrow_prefix_sum <- nrow(prefix_sum)
  for (i in 2:nrow_prefix_sum) {
    prefix_sum[i,] <- prefix_sum[i,] + prefix_sum[i - 1,]
  }
  
  smoothed <- matrix(0, nrow=nrow(X), ncol=ncol(X))
  for (i in 2:nrow_prefix_sum) {
    begin_range <- max(1, i - window)
    smoothed[i - 1,] <-
      (prefix_sum[i,] - prefix_sum[begin_range,]) / (i - begin_range)
  }
  
  smoothed
}
