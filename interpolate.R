library(pracma)
library(types)

# Interpolate using nearest neighbor method where value is NA.
interpolate_vector <- function(V = ? numeric) {
  x_total = 1:(length(V))
  yi <- interp1(x_total, V, method = "nearest")
  yi
}

interpolate_matrix <- function(M = ? numeric) {
  apply(M, 2, interpolate_vector)
}
