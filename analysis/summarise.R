source("analysis/acc_data_aggregation.R")
library(tidyr)
library(forecast)
library(ggplot2)

acc1 <- read_acc("./data/sample1.csv")
acc2 <- read_acc("./data/sample2.csv")

data_mean <- aggregate_by_minute(acc1$data, acc1$meta)
ggplot(data_mean, aes(x = time_group, y = acceleration)) +
  geom_line()

data_sum <- epoch_data(acc1$data, acc1$meta, statistics = "sum")
ggplot(data_sum, aes(x = time_group, y = acceleration)) +
  geom_line()

data_ma <- acc1$data %>%
  mutate(
    ma3 = ma(acceleration, 3),
    ma60 = ma(acceleration, 60)
  )
ggplot(data_ma, aes(x = time)) +
  geom_line(aes(y = acceleration), colour = "grey30", size = 1.2) +
  geom_line(aes(y = ma3), color = "red", alpha = 0.6) +
  geom_line(aes(y = ma60), color = "blue", alpha = 0.6)

data_log <- acc1$data %>%
  mutate(
    acceleration = log(acceleration + 1),
    ma3 = ma(acceleration, 3),
    ma60 = ma(acceleration, 60)
  )
ggplot(data_log, aes(x = time, y = ma60)) +
  geom_line()
