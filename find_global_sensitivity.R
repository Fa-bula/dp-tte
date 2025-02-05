source("functions.R")

# Iterates over all possible time-to-event data sets for num_trt treatment groups
# with N subjects in each treatment group
find_global_sensitivity <- function(num_trt, N, max_aval, f) {
  all_vectors <- expand.grid(replicate(num_trt * N, 1:max_aval, simplify = FALSE))
  differences <- sapply(1:nrow(all_vectors), function(i) {
    # Construct data set df
    time <- unlist(all_vectors[i, ])
    event <- rep(1, N * num_trt)
    trt <- rep(1:num_trt, each = N)
    df <- data.frame(time, event, trt)
    result <- tryCatch(
      {
        f_value <- f(df, 1)
        f_value_without_first <- f(df[-1, ], 1)
        difference <- abs(f_value - f_value_without_first)
        return(difference)
      },
      error = function(e) {
        cat("An error occurred: ", conditionMessage(e), "\n")
        # Perform additional error handling or return a default value
        return(NA)
      }
    )
  })
  return(max(as.numeric(differences), na.rm = TRUE))
}

# cat(paste("Global sensitivity of", deparse(substitute(calculate_km_estimate)),
#             find_global_sensitivity(1, 4, 2, calculate_km_estimate)))

# cat(paste("Global sensitivity of", deparse(substitute(calculate_hazard_ratio)),
#           find_global_sensitivity(num_trt=2, N=4, max_aval=2, calculate_hazard_ratio)))

for (x in c(2,3,4,5,6)) {
  for (y in c(1, 2, 3, 4,5,6)) {
    print(paste('N=', x, 'max_aval=', y))
    cat(paste("Global sensitivity of", deparse(substitute(calculate_rmean_survival_time)),
              find_global_sensitivity(num_trt=1, N=x, max_aval=y, calculate_rmean_survival_time)))
    print('')
  }
}
