calculate_dp_values <- function(true_value, eps_values, sensitivity, K) {
  # Calculate private f value K times
  dp_f_values <- list()
  for (k in 1:K) {
    # Add laplace noise to value with different privacy budget
    dp_f <- sapply(eps_values, function(eps) LaplaceMechanism(true.values = true_value,
                                                              eps = eps,
                                                              sensitivities = sensitivity))
    dp_f_values <- append(dp_f_values, list(dp_f))
  }
  # Combine into matrix
  dp_f_matrix <- do.call(rbind, dp_f_values)
  dp_f_matrix
}
