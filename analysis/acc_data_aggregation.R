library(tidyverse)
library(lubridate)

s1$data <- read_csv("data/acc_sample_data_001.csv")

extract_meta_data <- function(data) {
  meta_data <- colnames(data$data)[1]
  dates <- str_match_all(meta_data, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}")[[1]]
  start <- ymd_hms(dates[1], tz="GMT")
  end <- ymd_hms(dates[2], tz = "GMT")
  sample_rate <- str_match(meta_data, "([0-9]+) seconds")[2]
  list(start, end, sample_rate)
}

mean_epoch <- function(data, current_freq, desired_freq) {
  
}