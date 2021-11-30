library(dplyr)
library(readr)
library(DataExplorer)
library(ggplot2)
library(factoextra)
library(FactoMineR)
library(RColorBrewer)

profile <- read_csv("./data/profile_data.csv")

png("./images/clean/missing_distribution_before.png", width = 720)
plot_missing(profile)
dev.off()

# Adding NA level and transform variables as factor
profile_filtered <- profile %>%
  filter(
    !chronotype %in% c(-1, -3) & !ethnic %in% c(-1, -3) & insomnia != -3 &
      !shift_work %in% c(-1, -3) & !night_shift_work %in% c(-1, -3) &
      smoking != -3 & alcohol != -3 & employment != -3 & 
      !loneliness %in% c(-1, -3) & !depressed %in% c(-1, -3) & 
      !gp_depressed %in% c(-1, -3)) %>%
  mutate(
    ethnic = case_when(
      ethnic %in% c(1001, 1002, 1003) ~ 1,
      ethnic %in% c(2001, 2002, 2003, 2004) ~ 2,
      ethnic %in% c(3001, 3002, 3003, 3004) ~ 3,
      ethnic %in% c(4001, 4002, 4003) ~ 4,
      T ~ ethnic
    ),
    shift_work = ifelse(is.na(shift_work), 1, shift_work),
    work_hours = ifelse(is.na(work_hours), 0, work_hours),
    night_shift_work = ifelse(is.na(night_shift_work), 1, night_shift_work)
  ) %>%
  mutate_at(
    c("sex", "ethnic", "chronotype", "insomnia", "shift_work", 
      "night_shift_work", "smoking", "alcohol", "employment", "loneliness", 
      "depressed", "gp_depressed"), as.factor)
# Include effect of space (coordinates)

# Percentage of missing data
1 - nrow(profile_filtered) / nrow(profile)

png("./images/clean/missing_distribution_after.png", width = 720)
plot_missing(profile_filtered)
dev.off()

# Percentage of missing data
profile_clean <- profile_filtered[complete.cases(profile_filtered), ]
1 - nrow(profile_clean) / nrow(profile_filtered)
1 - nrow(profile_clean) / nrow(profile)
dim(profile_filter)

# Creating urbanization variable
## PCA to find optimal linear combination of numerical variables of urbanization 
profile_clean %>%
  mutate(
    maj_road = inv_dist_major_road * traffic_major_road,
    road = inv_dist_road * traffic_road
  ) %>%
  select(
    maj_road, inv_dist_major_road, traffic_major_road,
    road, inv_dist_road, traffic_road
  ) %>%
  plot_correlation(type = "c")
ggsave("images/clean/correlation_road_variables.png", width = 9)

pca_data <- profile_clean %>%
  mutate(
    maj_road = inv_dist_major_road * traffic_major_road,
    road = inv_dist_road * traffic_road
  ) %>%
  select(maj_road, road, noise_pollution) %>%
  mutate_all(function(x) (x - mean(x) / sd(x)))

pca_urb <- PCA(pca_data, graph = F)
get_eigenvalue(pca_urb)

png("./images/clean/pca_components.png", width = 720)
fviz_pca_var(pca_urb, col.var = "cos2", gradient.cols = brewer.pal(3, "YlOrRd"),
             repel = TRUE)
dev.off()

## Population density distribution
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
    urbanization = pca_urb$ind$coord[, 1]
  )

write_rds(profile_clean, file = "./data/profile.rds")
