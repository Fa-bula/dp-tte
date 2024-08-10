# Load required library
library(survival)

# Function to calculate KM estimate at a specific time point
calculate_km_estimate <- function(df, timepoint) {
  # Fit the Kaplan-Meier survival curve
  fit <- survfit(Surv(event_times, cnsr) ~ 1, data = df)
  
  # Calculate the Kaplan-Meier estimate at specific timepoint
  return(summary(fit, times = timepoint)$surv)
}