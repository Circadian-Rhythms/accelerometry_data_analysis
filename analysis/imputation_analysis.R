source("analysis/acc_data_aggregation.R")
library(ggplot2)
library(visdat)

acc1 <- read_acc("./data/sample1.csv")

acc_min <- epoch_data(acc1$data, acc1$meta)

acc_min %>%
  mutate(imputed = ifelse(imputed == 1, NA, imputed)) %>%
  select(imputed) %>%
  vis_miss()

data <- acc1$data %>%
  mutate(
    line_group = cumsum(imputed != lag(imputed, default = first(imputed)))
  )

ggplot(data, aes(x = date_time, y = acceleration, color = factor(imputed), 
                    group = line_group)) +
  geom_line()

ggplot(data, aes(x = date_time, y = log(acceleration + 1), color = factor(imputed), 
                    group = line_group)) +
  geom_line()

