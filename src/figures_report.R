library(readr)
library(dplyr)
library(ggplot2)
source("./accelerometry_data_analysis/src/plot_functions.R")

profile <- read_rds("./data/profile.rds")

profile <- profile %>%
  mutate(
    chronotype = case_when(
      chronotype == 1 ~ "Morning",
      chronotype == 2 ~ "More morning than evening",
      chronotype == 3 ~ "More evening than morning",
      chronotype == 4 ~ "Evening"
    ) %>%
      factor(ordered = T),
    depressed = case_when(
      depressed == 1 ~ "Not at all",
      depressed == 2 ~ "Several days",
      depressed == 3 ~ "More than half the days",
      depressed == 4 ~ "Nearly every day"
    )
  )

# Chronotype distribution
ggplot(data = profile, aes(x = chronotype)) +
  geom_bar() +
  theme_linedraw() +
  xlab("Chronotype") +
  ylab("Count")
ggsave("images/report/chronotype_counts.png", width = 9)

# Chronotype and depression
bar_fill(
  profile, depressed, chronotype, x_label = "Depression", 
  fill_label = "Chronotype", 
  x_order = c("Not at all", "Several days", "More than half the days", "Nearly every day"))
ggsave("images/report/depressed_chrono.png", width = 9)

# Chronotype and age
boxplot_group(
  profile, chronotype, age, chronotype, x_label = "Chronotype", y_label = "Age")
ggsave("images/report/age_chrono.png", width = 9)
