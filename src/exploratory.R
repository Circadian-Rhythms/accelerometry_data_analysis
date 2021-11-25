library(readr)
library(dplyr)
library(tidyr)
library(DataExplorer)
library(ggplot2)

profile <- read_rds("./data/profile.rds")

profile <- profile %>%
  select(-eid) %>%
  mutate(
    dep_gp = paste(depressed, gp_depressed, sep = "_") %>%
      factor(),
    employ_shift = paste(employment, shift_work, sep = "_") %>%
      factor(),
    employ_night = paste(employment, night_shift_work, sep = "_") %>%
      factor()
  )

# Number of levels for categorical data
profile %>%
  select_if(is.factor) %>%
  summarise_all(function(x) length(unique(x))) %>%
  pivot_longer(-0, names_to = "variable", values_to = "n_levels")

# Distributions of categorical variables
plot_cat <- plot_bar(profile, by = "chronotype")
for (i in 1:length(plot_cat)){
  ggsave(paste0("images/exploratory/dist_cat_", i, ".png"), plot_cat[[i]], 
         width = 15) 
}

# Distributions of quantitative variables
plot_quant <- plot_histogram(profile)
for (i in 1:length(plot_quant)){
  ggsave(paste0("images/exploratory/dist_quant_", i, ".png"), plot_quant[[i]], 
         width = 9) 
}

# Distributions of quantitative variables by chronotype
plot_quant_chrono <- plot_boxplot(profile, by = "chronotype")
for (i in 1:length(plot_quant_chrono)){
  ggsave(paste0("images/exploratory/dist_quant_chrono_", i, ".png"), plot_quant_chrono[[i]], 
         width = 12) 
}

# Correlation
plot_correlation <- plot_correlation(profile, type = "c")
ggsave("images/exploratory/correlation.png", plot_correlation, width = 9)
