library(nparACT)
library(testthat)
source("analysis/parameter_estimation.R")

acc1 <- read_acc("data/acc_sample_data_001.csv", standardise = FALSE)

acc1_mod <- acc1$data %>%
  mutate(time = format(date_time, format = "%H:%M:%S"),
         date = date(date_time)) %>%
  select(date, time, acceleration)

results <- nparACT_base("acc1_mod", 1/acc1$meta$sample_rate, cutoff = 0, plot = F)

RA <- relative_amplitude(acc1)
IS <- interdaily_stability(acc1, k = 60)
IV <- intradaily_variability(acc1)

test_that("IS", expect_equal(IS, results[[1]], tolerance = 1e-2))
test_that("IV", expect_equal(IV, results[[2]], tolerance = 1e-2))
test_that("RA", expect_equal(RA, results[[3]], tolerance = 1e-2))

# Testing different time scale (min/hour)
sample_rate2 <- 60 * 1
acc2_mod <- epoch_data_fast(acc1, sample_rate = sample_rate2)$data %>%
  mutate(time = format(date_time, format = "%H:%M:%S"),
         date = date(date_time)) %>%
  select(date, time, acceleration)

acc2_data <- acc2_mod %>%
  mutate(date_time = as_datetime(paste(date, time))) %>%
  select(date_time, acceleration)

acc2 <- list(
  data = acc2_data, meta = list(
    start = min(acc2_data$date_time), 
    end = max(acc2_data$date_time), 
    sample_rate = sample_rate2))
results <- nparACT_base("acc2_mod", 1/sample_rate2, cutoff = 0, plot = F)

RA <- relative_amplitude(acc2)
IS <- interdaily_stability(acc2, k = sample_rate2)
IV <- intradaily_variability(acc2)

test_that("IS", expect_equal(IS, results[[1]], tolerance = 1e-2))
test_that("IV", expect_equal(IV, results[[2]], tolerance = 1e-2))
test_that("RA", expect_equal(RA, results[[3]], tolerance = 1e-2))
