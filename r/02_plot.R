library(ggplot2)
library(tidyr)

source("r/01_data_cleaning.R")
source("r/plotting_functions.R")

head(data)
melted_data <- data |>
  gather(key =  "variable",
         value = "measurement", starts_with("log_ratio"))

ggplot(data = melted_data, aes(x = day_no, y = measurement, color = col_no)) +
  facet_grid(variable ~ col_no, scales = "free_y",
             labeller = labeller(variable = variable_labeller(), col_no = column_labeller()), switch = "y") +
  annotate("segment", x = -Inf, xend = Inf, y = 0, yend = 0, colour = "#999", linewidth = 1)+
  geom_boxplot() +
  color_column() + theme_boxplot() +
  xlab("Days") + ylab("log ratio of dayX to day0") +
  scale_x_discrete(labels = day_labeller) +
  theme(axis.ticks = element_line())

# For free y axis labels to show the variation within HIX columns you can use
#ggh4x::facet_grid(variable ~ col_no, scales = "free_y", independent = "y",
