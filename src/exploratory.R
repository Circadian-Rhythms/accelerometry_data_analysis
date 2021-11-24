library(readr)
library(dplyr)

profile <- read_rds("./data/profile.rds")

summary(profile$age)
summary(profile$work_hours)

table(profile$chronotype) %>%
  prop.table()
