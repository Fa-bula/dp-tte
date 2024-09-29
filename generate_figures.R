source("KM_estimate.R")
library(DPpack)

# Define the values for N, lambda, and p_cens
N_values <- c(100, 200)
lambda_values <- c(0.3)
p_cens_values <- c(0.3)
eps_values <- seq(0.01, 0.5, by = 0.025)
K <- 100

# Plot options
# Colors for scatter plot
col1 <- "#4DAF4A"
col2 <- "#E41A1C"
col3 <- "#377EB8"
# Size of points for scatter plot
cex = 0.6

# Create a data frame with all combinations using expand.grid
params <- expand.grid(N = N_values, lambda = lambda_values, p_cens = p_cens_values)

# Function to apply function f to generated data.frame for each combination
generate_and_apply <- function(N, lambda, p_cens, f, sensitivity) {
  # Generate random survival times using exponential distribution
  time <- rexp(N, rate = lambda)
  
  # Generate random event indicators
  event <- rbinom(N, size = 1, prob = 1 - p_cens)
  
  # Combine survival times and event indicators into a data frame
  df <- data.frame(
    time = time,
    event = event
  )
  
  # Calculate value of a function
  true_f_value = f(df)
  
  # Calculate private f value K times
  dp_f_values <- list()
  for (k in 1:K) {
    # Add laplace noise to value with different privacy budget
    dp_f <- sapply(eps_values, function(eps) LaplaceMechanism(true.values = true_f_value,
                                                              eps = eps,
                                                              sensitivities = sensitivity))
    dp_f_values <- append(dp_f_values, list(dp_f))
  }
  # Combine into matrix
  dp_f_matrix <- do.call(rbind, dp_f_values)
  q1_values <- apply(dp_f_matrix, 2, function(x) quantile(x, probs = 0.25))
  median_values <- apply(dp_f_matrix, 2, function(x) quantile(x, probs = 0.5))
  q3_values <- apply(dp_f_matrix, 2, function(x) quantile(x, probs = 0.75))
 
  plot(x = eps_values, y = median_values, ylim=c(0.3, 1.3),
       main = paste("N = ", N),
       xlab = expression(epsilon), ylab = "Private KM estimate",
       col = col2, pch = 19, cex = cex)
  
  points(x = eps_values, y = q1_values, col = col3, pch = 19, cex = cex)
  points(x = eps_values, y = q3_values, col = col1, pch = 19, cex = cex)
  abline(h = true_f_value, col = "black", lty = 2)
  
  legend("topright",                    # Position of the legend
         legend = c("Q3", "Median", "Q1"),
         col = c(col1, col2, col3),
         pch = 19,
         cex = 0.8)
}

# Apply the function to each row of the params data frame
df_list <- apply(params, 1, function(x) 
    generate_and_apply(x[1], x[2], x[3], calculate_km_estimate, 1 / x[1]))


