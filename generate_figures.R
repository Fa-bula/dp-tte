source("functions.R")
library(DPpack)

# Rate parameter in exp distribution
lambda_values <- c(1)
# Probability of censoring
p_cens_values <- c(0.3)
# Number of iterations for calculation of DP values
K <- 500

# Plot options
# Colors for scatter plot
col1 <- "#4DAF4A"
col2 <- "#E41A1C"
col3 <- "#377EB8"
# Size of points for scatter plot
cex = 0.6

# Function to apply function f to generated data.frame for each combination
generate_and_apply <- function(N, lambda, p_cens, t_duration, f, sensitivity, ylab, ylim) {
  # Generate random survival times using exponential distribution
  time <- rexp(N, rate = lambda)
  
  # Generate random event indicators
  event <- rbinom(N, size = 1, prob = 1 - p_cens)
  
  # Censoring by end of trial
  le_trial_duration <- time <= t_duration
  time <- ifelse(time > t_duration, t_duration, time)
  event <- event & le_trial_duration
  
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
  # Calculate Q1, median and Q3 among all private values
  q1_values <- apply(dp_f_matrix, 2, function(x) quantile(x, probs = 0.25))
  median_values <- apply(dp_f_matrix, 2, function(x) quantile(x, probs = 0.5))
  q3_values <- apply(dp_f_matrix, 2, function(x) quantile(x, probs = 0.75))
  # Calculate relative range
  rel_range <- (q3_values - q1_values) / true_f_value
  index <- which(rel_range < rel_range_threshold)[1]
  print(index)
  plot(x = eps_values, y = median_values, ylim=ylim,
       main = paste("N = ", N, ", Trial Duration = ", t_duration),
       xlab = expression(epsilon), ylab = ylab,
       col = col2, pch = 19, cex = cex)
  
  points(x = eps_values, y = q1_values, col = col3, pch = 19, cex = cex)
  points(x = eps_values, y = q3_values, col = col1, pch = 19, cex = cex)
  abline(h = true_f_value, col = "black", lty = 2)
  abline(v = eps_values[index], col = "blue", lty = 1)
  legend("topright",
         legend = c("Q3", "Median", "Q1"),
         col = c(col1, col2, col3),
         pch = 19,
         cex = 0.8)
  
  legend("bottomright",
         legend = paste("Relative interquartile range <", rel_range_threshold),
         pch = "|",
         col = "blue",
         horiz = TRUE,
         bty = "n")
  
  legend("bottomleft",
         legend = paste("True value of endpoint"),
         lty = 2,
         col = "black",
         bty = "n")
}

# Population counts
N_values <- c(100, 200)
# Trial duration
t_duration_values <- c(2)
# Privacy budget
eps_values <- seq(0.05, 1, by = 0.025)
rel_range_threshold <- 0.1
# Create a data frame with all combinations using expand.grid
params <- expand.grid(N = N_values, lambda = lambda_values, 
                      p_cens = p_cens_values, t_duration = t_duration_values)

df_list <- apply(params, 1, function(x)
  generate_and_apply(N=x[1], lambda=x[2], p_cens=x[3], t_duration=x[4],
                     f=calculate_km_estimate, sensitivity=1 / x[1], 
                     ylab="Private KM estimate", ylim=c(0.4, 0.55)))

# Population counts
N_values <- c(100)
# Trial duration
t_duration_values <- c(2, 5)
# Privacy budget
eps_values <- seq(1, 25, by = 0.5)
rel_range_threshold <- 0.25
# Create a data frame with all combinations using expand.grid
params <- expand.grid(N = N_values, lambda = lambda_values, 
                      p_cens = p_cens_values, t_duration = t_duration_values)

df_list <- apply(params, 1, function(x)
  generate_and_apply(N=x[1], lambda=x[2], p_cens=x[3], t_duration=x[4],
                     f=calculate_median_survival_time, sensitivity=x[4], 
                     ylab="Private median survival estimate", ylim=c(-5, 5)))


