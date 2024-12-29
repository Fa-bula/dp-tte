plot_results <- function(eps_values, median_values, q1_values, q3_values, true_value, ylimdelta, main, ylab, index) {
  plot(x = eps_values, y = median_values,
       xlim = c(0, max(eps_values)),
       ylim = c(true_value - ylimdelta, true_value + ylimdelta),
       main = main,
       xlab = expression(epsilon), ylab = ylab,
       col = col2, pch = 19, cex = 0.6)
  points(x = eps_values, y = q1_values, col = col3, pch = 19, cex = 0.6)
  points(x = eps_values, y = q3_values, col = col1, pch = 19, cex = 0.6)
  abline(h = true_value, col = "black", lty = 2)
  abline(v = eps_values[index], col = "blue", lty = 1)
}
