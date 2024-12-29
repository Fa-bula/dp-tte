generate_data <- function(N, lambda, p_cens, t_duration) {
  # Generate random survival times using exponential distribution
  time <- rexp(N, rate = lambda)
  
  # Generate random event indicators
  event <- rbinom(N, size = 1, prob = 1 - p_cens)
  
  # Censoring by end of trial
  within_trial_duration <- time <= t_duration
  time <- ifelse(time > t_duration, t_duration, time)
  event <- event & within_trial_duration
  
  # Combine survival times and event indicators into a data frame
  data.frame(
    time = time,
    event = event
  )
}
