library(readr)
library(dplyr)
library(stringr)
library(lubridate)

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
      mutate(time = seq(meta$start, meta$end, by = meta$sample_rate)) %>%
      rename(
        acceleration = colnames(data)[1]
      ) %>%
      select(time, acceleration, imputed)
    return(data)
  }
  
  data <- read_csv(file)
  meta <- extract_meta_data(data)
  data <- add_time_and_tidy(data, meta)
  return(list(data = data, meta = meta))
}

aggregate_by_minute <- function(data, meta, statistics = "mean"){
  times_min <- seq(range(data$time)[1], range(data$time)[2], by = "1 min")
  n_rep <- 60 / meta$sample_rate
  data <- data %>%
    mutate(
      time_group = rep(times_min, each = n_rep) 
    ) %>%
    group_by(time_group) %>%
    summarise(
      acceleration = case_when(
        statistics == "mean" ~ mean(acceleration),
        statistics == "sum" ~ sum(acceleration) 
      )
    )
  return(data)
}
