source("acc_data_aggregation.R")
library(lubridate)

interdaily_stability <- function(df, sample_rate, p = 3600, k = sample_rate) {
  # The formula for IS (interdaily stability) is:
  # (n * \Sigma{k=1}{p}(x(k) - x)^2) / (p * \Sigma{i=1}{n}(x(i) - x)^2)
  # Where n is the number of observations, x(k) are the epoch means, x is the overall mean,
  # and x(i) are the individual observations of the average 24 hour pattern.
  average_pattern <- get_average_pattern(df)
  x_i <- epoch_data(average_pattern, current_sample_rate = sample_rate, desired_sample_rate = k)$acceleration
  n <- length(x_i)
  x <- mean(x_i)
  x_k <- epoch_data(average_pattern, current_sample_rate = sample_rate, desired_sample_rate = p)$acceleration
  p <- length(x_k)
  IS <- (n * sum((x_k - x) ** 2)) / (p * sum((x_i - x) ** 2))
  
  return(IS)
}

intradaily_variability <- function(df, sample_rate, p = 3600) {
  # The formula for IV (intradaily stability) is:
  # (n * \Sigma{k=1}{n}(x(i) - x(i-1))^2) / (n-1 * \Sigma{i=1}{n}(x(i) - x)^2)
  # Where x_i are the individual observations and x is the overall mean.

  x_i <- epoch_data(df, current_sample_rate = sample_rate, desired_sample_rate = p)$acceleration
  x <- mean(x_i)
  n <- length(x_i)
  
  IV <- (n * sum((x_i[2:n] - x_i[1:n-1]) ** 2))/ ((n-1) * sum((x_i - x) ** 2))
  
  return(IV)
}

get_average_hourly_pattern <- function(df) {
  average_pattern <- df %>%
    group_by(hour = hour(date_time), minute = minute(date_time), second = second(date_time)) %>%
    summarise(acceleration = mean(acceleration)) %>% 
    mutate(date_time = second + 60 * minute + 3600 * hour)
  return(average_pattern[,c("date_time", "acceleration")])
}

relative_amplitude <- function(df, sample_rate) {
  samples_per_hour <- 60*60/sample_rate
  average_acceleration <- get_average_hourly_pattern(df)$acceleration
  M10 <- max(ma(average_acceleration, order = 10 * samples_per_hour, centre = FALSE), na.rm = TRUE)
  L5 <- min(ma(average_acceleration, order = 5 * samples_per_hour), na.rm = TRUE)
  (M10 - L5)/(M10+L5)
}