source("KM_estimate.R")
library(DPpack)

# Define the values for N, lambda, and p_cens
N_values <- c(100, 200)
lambda_values <- c(0.3)
p_cens_values <- c(0.3)
eps_values <- seq(0.01, 1, by = 0.01)
K <- 10

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
  
  dp_f_values <- numeric(length(eps_values))
  for (k in 1:K) {
    # Add laplace noise to value with different privacy budget
    dp_f_values <- dp_f_values + sapply(eps_values, function(eps) LaplaceMechanism(true.values = true_f_value,
                                                                     eps = eps,
                                                                     sensitivities = sensitivity))
  }
  dp_f_values <- dp_f_values / K
  
  plot(x = eps_values, y = dp_f_values, ylim=c(0, 1),
       main = paste("Scatter Plot ", N), 
       xlab = expression(epsilon), ylab = "Private KM estimate",
       col = "blue", pch = 16)
  abline(h = true_f_value, col = "black", lty = 2)
}

# Apply the function to each row of the params data frame
df_list <- apply(params, 1, function(x) 
    generate_and_apply(x[1], x[2], x[3], calculate_km_estimate, 1 / x[1]))


