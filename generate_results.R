library(DPpack)
library(kableExtra)
library(dplyr)

source("functions.R")
source("generate_data.R")
source("calculate_dp_values.R")
source("create_summary_table.R")
source("plot_results.R")
source("params.R")

generate_results <- function(N, lambda, p_cens, t_duration, f, sensitivity, ylab, ylimdelta) {
  # Step 1: Generate data
  df <- generate_data(N, lambda, p_cens, t_duration)
  
  # Step 2: Calculate true and DP values
  true_f_value <- f(df)
  dp_matrix <- calculate_dp_values(true_f_value, eps_values, sensitivity, K)
  
  # Step 3: Create summary table
  summary_result <- create_summary_table(dp_matrix, true_f_value, eps_values, rel_range_threshold)
  summary_table <- summary_result$summary_table
  index <- summary_result$index
  
  # Step 4: Print summary table
  reduced_table <- summary_table %>%
    slice(seq(1, n(), by = 3)) %>%
    select(TrueValue, Epsilon, Q1, Median, Q3, RelativeRange)
  
  kable_table <- kable(reduced_table, caption = ylab, align = "c") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                  full_width = FALSE) %>%
    collapse_rows(columns = 1, latex_hline = "major", valign = "top")
  print(kable_table)
  
  # Step 5: Plot results
  plot_results(eps_values, summary_table$Median, summary_table$Q1, summary_table$Q3,
               true_f_value, ylimdelta, 
               paste("N = ", N, ", Trial Duration = ", t_duration), ylab, index)
}

# PART 1: KM estimate
layout(matrix(c(1, 2, 3), nrow = 3, byrow = TRUE), heights = c(3, 3, 1))
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
  generate_results(N=x[1], lambda=x[2], p_cens=x[3], t_duration=x[4],
                     f=calculate_km_estimate, sensitivity=1 / x[1], 
                     ylab="Private KM estimate", ylimdelta=0.1))
plot.new()

legend("center",
       legend = c("Q3", "Median", "Q1", 
                  paste("Relative IQR <", rel_range_threshold),
                  paste("True value of endpoint")),
       col = c(col1, col2, col3, "blue", "black"),
       pch = c(19, 19, 19, NA, NA),
       lty = c(NA, NA, NA, 1, 2),
       bty = "n",
       cex = 0.8,
       horiz = TRUE)

# PART 2: Median survival time
layout(matrix(c(1, 2, 3), nrow = 3, byrow = TRUE), heights = c(3, 3, 1))
# Population counts
N_values <- c(100)
# Trial duration
t_duration_values <- c(2, 5)
# Privacy budget
eps_values <- seq(1, 30, by = 0.5)
rel_range_threshold <- 0.25
# Create a data frame with all combinations using expand.grid
params <- expand.grid(N = N_values, lambda = lambda_values,
                      p_cens = p_cens_values, t_duration = t_duration_values)

df_list <- apply(params, 1, function(x)
  generate_results(N=x[1], lambda=x[2], p_cens=x[3], t_duration=x[4],
                     f=calculate_median_survival_time, sensitivity=x[4],
                     ylab="Private median survival time", ylimdelta=2))
plot.new()

legend("center",
       legend = c("Q3", "Median", "Q1",
                  paste("Relative IQR <", rel_range_threshold),
                  paste("True value of endpoint")),
       col = c(col1, col2, col3, "blue", "black"),
       pch = c(19, 19, 19, NA, NA),
       lty = c(NA, NA, NA, 1, 2),
       bty = "n",
       cex = 0.8,
       horiz = TRUE)

# PART 3: RMST
layout(matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, byrow = TRUE), heights = c(3, 3, 1))
# Population counts
N_values <- c(100, 200)
# Trial duration
t_duration_values <- c(2, 5)
# Privacy budget
eps_values <- seq(0.025, 0.8, by = 0.025)
rel_range_threshold <- 0.1
# Create a data frame with all combinations using expand.grid
params <- expand.grid(N = N_values, lambda = lambda_values,
                      p_cens = p_cens_values, t_duration = t_duration_values)

df_list <- apply(params, 1, function(x)
  generate_results(N=x[1], lambda=x[2], p_cens=x[3], t_duration=x[4],
                     f=calculate_rmean_survival_time, sensitivity=(x[4] - 1) / x[1],
                     ylab="Private restricted mean survival time", ylimdelta=0.6))

plot.new()

legend("center",
       legend = c("Q3", "Median", "Q1"),
       col = c(col1, col2, col3),
       pch = c(19, 19, 19),
       bty = "n",
       cex = 0.8,
       horiz = TRUE)

plot.new()

legend("center",
       legend = c(paste("Relative IQR <", rel_range_threshold),
                  paste("True value of endpoint")),
       col = c("blue", "black"),
       pch = c(NA, NA),
       lty = c(1, 2),
       bty = "n",
       cex = 0.8,
       horiz = TRUE)
