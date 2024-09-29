library(survival)

# Function to calculate KM estimate at a specific time point
calculate_km_estimate <- function(df, timepoint=1) {
  # Fit the Kaplan-Meier survival curve
  fit <- survfit(Surv(time, event) ~ 1, data = df)
  
  # Calculate the Kaplan-Meier estimate at specific timepoint
  return(summary(fit, times = timepoint)$surv)
}

calculate_hazard_ratio <- function(df, timepoint=1) {
  # Create a survival object
  surv_obj <- Surv(time = df$time, event = df$event)
  # Fit Cox proportional hazards model
  cox_model <- coxph(formula = surv_obj ~ trt, data = df)
  # Extract hazard ratio
  hazard_ratio <- exp(coef(cox_model))
  return(hazard_ratio)
}

calculate_logrank_pvalue <- function(df, timepoint=1) {
  # Performing Log-rank test on df
  diff <- survdiff(Surv(
    time = time,
    event = event,
    type = 'right'
  ) ~ trt,
  data = df)
  
  logrank_pval = pchisq(diff$chisq, length(diff$n) - 1, lower.tail = FALSE)
  return(logrank_pval)
}

calculate_median_survival_time <- function(df) {
  # Create a survival object
  surv_obj <- Surv(time = df$time, event = df$event)
  
  # Fit the Kaplan-Meier estimator
  km_fit <- survfit(surv_obj ~ 1)
  
  # Extract median survival time
  median_survival_time <- summary(km_fit)$table["median"]
  names(median_survival_time) <- NULL
  
  # Return median survival time
  return(median_survival_time)
}