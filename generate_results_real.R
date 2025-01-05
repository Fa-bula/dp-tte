library(dplyr)
library(DPpack)

# Ensure your custom functions and parameters are sourced
source("functions.R")
source("params.R")

# Trial duration values
t_duration_values <- seq(600, 1000, by = 10)
# Privacy budget
eps_values <- 1

# Read and preprocess the real data
df0 <- read.csv("cancer.csv") %>% select(c("event", "time"))

# Initialize an empty data frame to store results
results <- data.frame(
  t_duration = numeric(),
  q1 = numeric(),
  median = numeric(),
  q3 = numeric(),
  true_rmst = numeric()
)

# Loop through each trial duration value
for (t_duration in t_duration_values) {
  time <- df0$time
  event <- df0$event
  
  # Censoring by end of trial
  within_trial_duration <- time <= t_duration
  time <- ifelse(time > t_duration, t_duration, time)
  event <- event & within_trial_duration
  
  # Combine survival times and event indicators into a data frame
  df <- data.frame(
    time = time,
    event = event
  )
  
  # Calculate true restricted mean survival time
  true_rmst <- calculate_rmean_survival_time(df)
  
  # Differentially private computation
  dp_matrix <- calculate_dp_values(true_rmst, eps_values, (t_duration - 1) / nrow(df), K)
  
  # Calculate quantiles
  quantiles <- unname(quantile(dp_matrix, probs = c(0.25, 0.5, 0.75)))
  
  # Append results to the results data frame
  results <- rbind(results, data.frame(
    t_duration = t_duration,
    q1 = quantiles[1],
    median = quantiles[2],
    q3 = quantiles[3],
    true_rmst = true_rmst
  ))
}

plot(x = results$t_duration, y = results$median,
     main = "Private RMST values",
     xlab = "Trial Duration", ylab = "RMST",
     col = col2, pch = 19, cex = cex)
points(x = results$t_duration, y = results$q1, col = col3, pch = 19, cex = cex)
points(x = results$t_duration, y = results$q3, col = col1, pch = 19, cex = cex)
legend("bottom",
       legend = c("Q3", "Median", "Q1"),
       col = c(col1, col2, col3),
       pch = c(19, 19, 19),
       bty = "n",
       cex = 0.8,
       horiz = TRUE)


