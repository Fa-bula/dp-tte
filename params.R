# Rate parameter in exp distribution
lambda_values <- c(1)
# Probability of censoring
p_cens_values <- c(0.3)
# Number of iterations for calculation of DP values
K <- 1000

# Margins for plots
par(mar = c(5, 5, 2, 2))
# Colors for scatter plot
col1 <- "#4DAF4A"
col2 <- "#E41A1C"
col3 <- "#377EB8"
# Size of points for scatter plot
cex = 0.6