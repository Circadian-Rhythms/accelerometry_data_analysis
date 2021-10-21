library(nparACT)
library(testthat)
source("analysis/parameter_estimation.R")

acc1 <- read_acc("data/acc_sample_data_001.csv", standardise = FALSE)

acc_mod <- acc1$data %>%
  mutate(time = format(date_time, format = "%H:%M:%S"),
         date = date(date_time)) %>%
  select(date, time, acceleration)

results <- nparACT_base("acc_mod", 1/5, 0)

ra <- RA(acc1)
IS <- interdaily_stability(acc1)
IV <- intradaily_stability(acc1)

test_that("IS", expect_equal(IS, results[[1]], tolerance = 1e-2))
test_that("IV", expect_equal(IV, results[[2]], tolerance = 1e-2))
test_that("RA", expect_equal(ra, results[[3]], tolerance = 1e-2))
