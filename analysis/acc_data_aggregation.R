library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(forecast)

read_acc <- function(file){
  extract_meta_data <- function(data){
    meta_data <- colnames(data)[1]
    dates <- str_match_all(meta_data, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}")[[1]]
    start <- ymd_hms(dates[1], tz = "GMT")
    end <- ymd_hms(dates[2], tz = "GMT")
    sample_rate <- str_match(meta_data, "([0-9]+) seconds")[2] %>%
      as.numeric()
    return(list(start = start, end = end, sample_rate = sample_rate))
  }
  
  add_time_and_tidy <- function(data, meta){
    data <- data %>%
      mutate(date_time = seq(meta$start, meta$end, by = meta$sample_rate)) %>%
      rename(
        acceleration = colnames(data)[1]
      ) %>%
      select(date_time, acceleration, imputed)
    return(data)
  }
  
  data <- read_csv(file)
  meta <- extract_meta_data(data)
  data <- add_time_and_tidy(data, meta)
  return(list(data = data, meta = meta))
}

epoch_data_slow <- function(data, method = "mean", sample_rate = 60){
  df <- data$data
  meta <- data$meta
  epoch_date_time <- seq(range(df$date_time)[1], range(df$date_time)[2], by = sample_rate)
  n_rep <- sample_rate / meta$sample_rate
  df <- df %>%
    mutate(
      epochs = rep(epoch_date_time, each = n_rep) 
    ) %>%
    group_by(epochs) %>%
    summarise(
      acceleration = case_when(
        method == "mean" ~ mean(acceleration),
        method == "sum" ~ sum(acceleration) 
      )
    )
  data$data <- df
  data$meta$sample_rate <- sample_rate
  return(data)
}

epoch_data <- function(df, current_sample_rate, desired_sample_rate = 60, method = mean){
  k <- desired_sample_rate/current_sample_rate
  n <- length(df$acceleration)
  cuts <- split(df$acceleration, rep(1:ceiling(n/k), each=k)[1:n])
  
  date_time <- seq(range(df$date_time)[1], range(df$date_time)[2], by = desired_sample_rate)
  acceleration <- sapply(cuts, method)
  df <- data.frame(date_time, acceleration)

  return(df)
}

get_moving_average <- function(data, k) {
  data$data %>% mutate(
    moving_average = ma(acceleration, k),
  )
}
