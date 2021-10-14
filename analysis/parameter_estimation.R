source("analysis/acc_data_aggregation.R")

interdaily_stability <- function(acc, p = 3600) {
  # The formula for IS (interdaily stability) is:
  # (n * \Sigma{k=1}{p}(x(k) - x)^2) / (p * \Sigma{i=1}{n}(x(i) - x)^2)
  # Where n is the number of observations, x(k) are the epoch means, x is the overall mean,
  # and x(i) are the individual observations.
  
  # However, since the data has already been standardized i.e. mean = 0, sd = 1; we have x = 0.
  # Therefore the formula for IS can be written as:
  # (n * \Sigma{k=1}{p} x(k)^2) / (p * \Sigma{i=1}{n} x(i)^2)

  x_i <- acc$data$acceleration
  n <- length(x_i)
  
  x_k <- epoch_data_fast(acc, sample_rate = p)$data$acceleration
  p <- length(x_k)
  
  IS <- (n * sum(x_k ** 2))/ (p * sum(x_i ** 2))
  
  return(IS)
}

intradaily_stability <- function(acc, p = 3600) {
  # The formula for IV (intradaily stability) is:
  # (n * \Sigma{k=1}{p}(x(i) - x(i-1))^2) / (n-1 * \Sigma{i=1}{n}(x(i) - x)^2)
  # Where x_i are the individual observations and x is the overall mean.
  
  # As before, since the data has already been standardized i.e. mean = 0, sd = 1; we have x = 0.
  # Therefore the formula for IS can be written as:
  # (n * \Sigma{k=1}{p}(x(i) - x(i-1))^2) / (n-1 * \Sigma{i=1}{n}(x(i))^2)
  
  x_i <- acc$data$acceleration
  n <- length(x_i)
  
  IV <- (n * sum((x_i[2:n] - x_i[1:n-1]) ** 2))/ ((n-1) * sum(x_i ** 2))
  
  return(IV)
}
