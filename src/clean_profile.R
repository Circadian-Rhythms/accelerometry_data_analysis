library(dplyr)
library(readr)
library(DataExplorer)
library(ggplot2)

profile <- read_csv("./data/profile_data.csv")

png("./images/clean/missing_distribution_before.png", width = 720)
plot_missing(profile)
dev.off()

# Adding NA level and transform variables as factor
profile <- profile %>%
  filter(!chronotype %in% c(-1, -3)) %>%
  mutate(
    shift_work = ifelse(is.na(shift_work), "NA", shift_work),
    work_hours = ifelse(is.na(work_hours), -1, work_hours),
    night_shift_work = ifelse(is.na(night_shift_work), "NA", night_shift_work)
  ) %>%
  mutate_at(
    c("sex", "ethnic", "chronotype", "insomnia", "shift_work", 
      "night_shift_work", "smoking", "alcohol", "employment", "loneliness", 
      "depressed", "gp_depressed"), as.factor)

png("./images/clean/missing_distribution_after.png", width = 720)
plot_missing(profile)
dev.off()

# Percentage of missing data
profile_clean <- profile[complete.cases(profile), ]
1 - nrow(profile_clean) / nrow(profile)

# Creating urbanicity variable
ggplot(profile_clean, 
       aes(y = (..count..) / sum(..count..), x = as.factor(population_density))) +
  geom_bar()
ggsave("./images/clean/population_density.png", width = 10)

table(profile_clean$population_density)

profile_clean <- profile_clean %>%
  mutate(
    population_density_cat = case_when(
      population_density %in% c(1, 2, 3, 4) ~ "england/wales sparse",
      population_density %in% c(13, 14) ~ "scotland small town",
      population_density %in% c(16, 17, 18) ~ "scotland rural",
      population_density %in% c(11) ~ "scotland large urban",
      population_density %in% c(12) ~ "scotland other urban",
      population_density %in% c(5) ~ "england/wales urban less sparse",
      population_density %in% c(6) ~ "england/wales town/fringe less sparse",
      population_density %in% c(7) ~ "england/wales village less sparse",
      population_density %in% c(8) ~ "england/wales hamlet/isolated dwelling less sparse",
      population_density %in% c(9) ~ "postcode not linkable"
    )
  )
table(profile_clean$population_density_cat)

ggplot(profile_clean, 
       aes(y = (..count..) / sum(..count..), x = as.factor(population_density_cat))) +
  geom_bar() +
  coord_flip()
ggsave("./images/clean/population_density_cat.png", width = 10)

profile_clean <- profile_clean %>%
  mutate(
    urbanization = profile_clean$population_density_cat
  )

profile_clean$population_density
profile_clean$inv_dist_major_road
profile_clean$traffic_major_road
profile_clean$inv_dist_road
profile_clean$traffic_road
profile_clean$noise_pollution

write_rds(profile_clean, file = "./data/profile.rds")
