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
  
  data <- read_csv(file, show_col_types = FALSE)
  meta <- extract_meta_data(data)
  data <- add_time_and_tidy(data, meta)
  return(list(data = data, meta = meta))
}

epoch_data <- function(data, meta, method = "mean", sample_rate = 60){
  epoch_date_time <- seq(range(data$date_time)[1], range(data$date_time)[2], by = sample_rate)
  n_rep <- sample_rate / meta$sample_rate
  data <- data %>%
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
  return(data)
}

epoch_data2 <- function(data, meta, method = "mean", sample_rate = 60){
  k <- sample_rate/meta$sample_rate
  acc <- data$acceleration
  n <- length(acc)
  cuts <- split(acc, rep(1:ceiling(n/k), each=k)[1:n])
  sapply(cuts, mean)
}

get_moving_average <- function(data, k) {
  data$data %>% mutate(
    moving_average = ma(acceleration, k),
  )
}
