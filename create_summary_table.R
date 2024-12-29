create_summary_table <- function(dp_matrix, true_value, eps_values, rel_range_threshold) {
  q1_values <- apply(dp_matrix, 2, function(x) quantile(x, probs = 0.25))
  median_values <- apply(dp_matrix, 2, quantile, probs = 0.5)
  q3_values <- apply(dp_matrix, 2, quantile, probs = 0.75)
  
  rel_range <- (q3_values - q1_values) / true_value
  index <- which(rel_range < rel_range_threshold)[1]
  
  summary_table <- data.frame(
    TrueValue = round(rep(true_value, length(eps_values)), 2),
    Epsilon = round(eps_values, 2),
    Q1 = round(q1_values, 2),
    Median = round(median_values, 2),
    Q3 = round(q3_values, 2),
    RelativeRange = round(rel_range, 2)
  )
  
  list(summary_table = summary_table, index = index)
}
