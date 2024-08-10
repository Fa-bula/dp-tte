source("KM_estimate.R")

# Iterates over all possible time-to-event data sets for num_trt treatment groups
# with N subjects in each treatment group
find_global_sensitivity <- function(num_trt, N, max_aval, f) {
  all_vectors <- expand.grid(replicate(num_trt * N, 0:max_aval, simplify = FALSE))
  differences <- sapply(1:nrow(all_vectors), function(i) {
    # Construct data set df
    event_times <- unlist(all_vectors[i, ])
    cnsr <- rep(1, N * num_trt)
    trt <- rep(1:num_trt, each = N)
    df <- data.frame(event_times, cnsr, trt)
    f_value <- f(df, 1)
    f_value_without_first <- f(df[-1, ], 1)
    difference <- f_value - f_value_without_first
    return(difference)
  })
  return(max(as.numeric(differences), na.rm = TRUE))
}

cat(paste("Global sensitivity of", deparse(substitute(calculate_km_estimate)),
            find_global_sensitivity(1, 7, 2, calculate_km_estimate)))