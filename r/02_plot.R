library(ggplot2)
library(tidyr)
library(dplyr)

source("r/01_data_cleaning_wReservoir.R")
source("r/plotting_functions.R")
if(!dir.exists("output/plots")) {dir.create("output/plots")}

# Columns only: the reservoir (C0) is a single observation per day, so it has no
# distribution to draw, and facet_labels below assumes a 3-column grid.
# day_no is a character vector here (bind_rows collapses the factor levels), so
# order it by day_number to get the sampling days on the x axis in time order.
melted_data <- data_all_wReservoir |>
  filter(col_no != "C0") |>
  mutate(col_no = droplevels(col_no),
         day_no = factor(day_no, levels = unique(day_no[order(day_number)]))) |>
  gather(key =  "variable",
         value = "measurement", all_of(variables)) |>
  mutate(variable = as.factor(variable))

c_plot <- ggplot(data = melted_data, aes(x = day_no, y = measurement, color = col_no)) +
  facet_grid(variable ~ col_no, scales = "free_y",
             labeller = labeller(variable = variable_labeller()), switch = "y") +
  geom_boxplot() +
  color_column() + theme_boxplot() +
  xlab("Days") + ylab("Variable") +
  scale_x_discrete(labels = day_labeller) +
  theme(axis.ticks = element_line())

# For free y axis labels to show the variation within HIX columns you can use
#ggh4x::facet_grid(variable ~ col_no, scales = "free_y", independent = "y",

facet_labels <- data.frame(
  col_no = factor(rep(levels(melted_data$col_no), times = length(unique(melted_data$variable)))),  # Columns (left-to-right)
  variable = factor(rep(levels(melted_data$variable), 3, each = 3)),  # Rows (top-to-bottom)
  label = paste0("(",letters[1:24], ")")
)

final_c_plot <- c_plot +
  geom_text(data = facet_labels, aes(x = levels(melted_data$day_no)[2],  # Leftmost position
                                     y = Inf,
                                     label = label),
                                     inherit.aes = FALSE, hjust = 1,  vjust = 1.6, size = 4)

pdf('output/plots/DOC_and_optical.pdf', width = 11, height = 14, pointsize = 14)
plot(final_c_plot)
dev.off()



