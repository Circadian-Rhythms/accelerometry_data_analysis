library(dplyr)
library(ggplot2)

bar_fill <- function(data, x, fill, x_label, fill_label, x_order){
  data <- data %>%
    group_by({{ x }}, {{ fill }}) %>%
    summarise(value = n()) %>%
    mutate(pct = paste0((round(value / sum(value) * 100, 2))," %"))
  
  p <- ggplot(data, aes(!!enquo(x), value, fill = !!enquo(fill))) +
    geom_bar(stat = "identity", position = "fill") +
    geom_text(aes(label = pct), position = position_fill(vjust = 0.5), size = 3) +
    scale_x_discrete(limits = x_order) +
    coord_flip() +
    theme_linedraw() +
    theme(legend.position = "bottom") +
    labs(x = x_label, y = "Percentage", fill = fill_label)
  return(p)
}

boxplot_group <- function(data, x, y, group, x_label, y_label){
  p <- ggplot(data, aes(!!enquo(x), !!enquo(y), group = !!enquo(group))) +
    geom_boxplot() +
    coord_flip() +
    theme_linedraw() +
    labs(x = x_label, y = y_label)
  return(p)
}
