source("analysis/acc_data_aggregation.R")
library(lubridate)

interdaily_stability <- function(acc, p = 3600) {
  # The formula for IS (interdaily stability) is:
  # (n * \Sigma{k=1}{p}(x(k) - x)^2) / (p * \Sigma{i=1}{n}(x(i) - x)^2)
  # Where n is the number of observations, x(k) are the epoch means, x is the overall mean,
  # and x(i) are the individual observations of the average 24 hour pattern.
  average_pattern <- get_average_pattern(acc)
  x_i <- average_pattern$data$acceleration
  n <- length(x_i)
  x <- mean(x_i)
  x_k <- epoch_data_fast(average_pattern, sample_rate = p)$data$acceleration
  p <- length(x_k)
  IS <- (n * sum((x_k - x) ** 2)) / (p * sum((x_i - x) ** 2))
  
  return(IS)
}

intradaily_variability <- function(acc, p = 3600) {
  # The formula for IV (intradaily stability) is:
  # (n * \Sigma{k=1}{n}(x(i) - x(i-1))^2) / (n-1 * \Sigma{i=1}{n}(x(i) - x)^2)
  # Where x_i are the individual observations and x is the overall mean.

  x_i <- epoch_data_fast(acc, sample_rate = p)$data$acceleration
  x <- mean(x_i)
  n <- length(x_i)
  
  IV <- (n * sum((x_i[2:n] - x_i[1:n-1]) ** 2))/ ((n-1) * sum((x_i - x) ** 2))
  
  return(IV)
}

get_average_pattern <- function(acc) {
  average_pattern <- acc$data %>%
    group_by(hour = hour(date_time), minute = minute(date_time), second = second(date_time)) %>%
    summarise(acceleration = mean(acceleration)) %>% 
    mutate(date_time = second + 60 * minute + 3600 * hour)
  list(data = average_pattern[,c("date_time", "acceleration")], meta = acc$meta) 
}

relative_amplitude <- function(acc) {
  #This algorithm may have to change as there is ambiguity in the definitions of M10 and L5
  # given in the paper by van Someren.
  samples_per_hour <- 60*60/acc$meta$sample_rate
  average_acceleration <- get_average_pattern(acc)$data$acceleration
  M10 <- max(ma(average_acceleration, order = 10 * samples_per_hour, centre = FALSE), na.rm = TRUE)
  L5 <- min(ma(average_acceleration, order = 5 * samples_per_hour), na.rm = TRUE)
  (M10 - L5)/(M10+L5)
}