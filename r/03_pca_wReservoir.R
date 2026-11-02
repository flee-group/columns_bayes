# PCA + trajectory arrows WITH the reservoir as a single fixed source point.
#
# The four daily reservoir measurements (Day0/Day3/Day10/Day17) are AVERAGED into
# ONE reservoir observation, which enters prcomp() as a single ordinary row. That
# single reservoir score is then reused as the start point of every day's arrow
# (C0 -> C1 -> C2 -> C3), so all four day-segments start from the same point. As a
# single score it has no standard error, so it carries no error bar in the plot.

library(ggplot2)
library(data.table)
library(dplyr)

source("r/plotting_functions.R")
data_all_wReservoir <- readRDS("data/cleaned_data_wReservoir.rds")

# ---- pca data, mirroring r/pca.R lines 7-17 (now keeping the C0 reservoir) ----
pca_data_wReservoir <- data.table::data.table(data_all_wReservoir)
pca_data_wReservoir <- pca_data_wReservoir[!(is.na(bix) & is.na(fi) & is.na(hix) & is.na(a254))]
pca_data_wReservoir[is.na(pca_data_wReservoir)] <- 0
pca_data_wReservoir$day_no <- factor(pca_data_wReservoir$day_no,
                          levels = c("Day0", "Day1", "Day2","Day3", "Day7", "Day9", "Day10", "Day12", "Day14", "Day17"))
pca_data_wReservoir <- pca_data_wReservoir |>
  filter(day_no != "Day9") |>
  mutate(suva254 = a254/(DOC/1000)) |>
  select(-c("a254", "DOC", "DN")) |>
  #filter(day_no %in% c("Day0", "Day3", "Day10", "Day17"))|> # remove all the other days for reduced data
  # keep only these Day3 replicates, BUT always keep the reservoir (C0) row
  filter(day_no != "Day3" | replicate %in% c("L", "N", "E", "J") | col_no == "C0")

# columns that are NOT variables fed to the PCA
meta_cols_wReservoir <- c("replicate", "day_no", "col_no", "day_number")
pca_vars <- setdiff(names(pca_data_wReservoir), meta_cols_wReservoir)

# ---- average the 4 daily reservoir rows into ONE reservoir measurement ----
pca_cols_wReservoir  <- pca_data_wReservoir[col_no != "C0"]
pca_res_daily_wReservoir <- pca_data_wReservoir[col_no == "C0"]   # 4 daily rows
reservoir_avg_wReservoir <- pca_res_daily_wReservoir[
  , lapply(.SD, mean, na.rm = TRUE), .SDcols = pca_vars]

# ---- PCA: all columns + the single averaged reservoir row ----
input_vars_wReservoir <- rbind(pca_cols_wReservoir[, ..pca_vars],
                               reservoir_avg_wReservoir[, ..pca_vars])
pca_active <- prcomp(input_vars_wReservoir, center = TRUE, scale. = TRUE)
summary(pca_active)

# metadata aligned to the PCA input rows (numeric-only rbind avoids factor clashes)
meta_wReservoir <- rbind(
  pca_cols_wReservoir[, .(replicate = as.character(replicate),
                          day_no    = as.character(day_no),
                          col_no    = as.character(col_no))],
  data.table(replicate = "Reservoir", day_no = NA_character_, col_no = "C0"))

scores_active <- as.data.table(as.data.frame(pca_active$x))
scores_active[, `:=`(replicate = meta_wReservoir$replicate,
                     day_no    = meta_wReservoir$day_no,
                     col_no    = meta_wReservoir$col_no)]
loadings_active <- pca_active$rotation
explained_variance <- pca_active$sdev^2 / sum(pca_active$sdev^2)


# ============================================================================
# Plots. Same ggplot code as r/pca.R (the average_arrows_facet and loadings_plot
# blocks). The arrow builder gains a leading C0 -> Column 1 segment, and because
# there is exactly ONE reservoir score it is reused for every day.
# ============================================================================
day_levels <- c("Day0", "Day1", "Day2","Day3", "Day7", "Day10", "Day12", "Day14","Day17")

# the single averaged reservoir score
res_score <- scores_active[col_no == "C0"]

# column averages per day / column
avg_cols <- scores_active[col_no != "C0", .(
  PC1 = mean(PC1, na.rm = TRUE),
  PC2 = mean(PC2, na.rm = TRUE)
), by = .(day_no, col_no)]

# reservoir position: identical for every day (all C0 arrows share this start)
avg_res <- data.table(day_no = day_levels, col_no = "C0",
                      PC1 = res_score$PC1, PC2 = res_score$PC2)

avg_positions <- rbind(avg_cols, avg_res)
avg_positions[, day_no := factor(day_no, levels = day_levels)]

# Build arrows: C0 -> Column 1 -> Column 2 -> Column 3
average_arrow_data <- avg_positions[, .(
  x_start = c(PC1[col_no == "C0"], PC1[col_no == "Column 1"], PC1[col_no == "Column 2"]),
  y_start = c(PC2[col_no == "C0"], PC2[col_no == "Column 1"], PC2[col_no == "Column 2"]),
  x_end   = c(PC1[col_no == "Column 1"], PC1[col_no == "Column 2"], PC1[col_no == "Column 3"]),
  y_end   = c(PC2[col_no == "Column 1"], PC2[col_no == "Column 2"], PC2[col_no == "Column 3"]),
  segment = c("C0to1", "1to2", "2to3")
), by = day_no]

# Error bars come from the columns only -> the reservoir (single point) has none
summary_pca_scores <- scores_active[col_no != "C0"] |>
  group_by(day_no, col_no) |>
  summarise(across(where(is.numeric), list(
    mean = ~ mean(.x, na.rm = TRUE),
    sd = ~ plotrix::std.error(.x, na.rm = TRUE)
  ), .names = "{.col}_{.fn}"), .groups = "drop") |>
  mutate(day_no = factor(day_no, levels = day_levels))

PCAloadings <- as.data.frame(loadings_active)
PCAloadings$Variables <- rownames(PCAloadings)

# Group the optical indices by what they actually indicate in THIS dataset:
#  - Terrestrial / aged, aromatic carbon: hix (humification), suva254 (aromaticity),
#    and fi (fluorescence index) -- note all FI values here are ~1.0-1.2, i.e. the
#    terrestrial band (the microbial endmember is ~1.8), so FI is NOT a microbial signal.
#  - Fresh / autochthonous: bix (freshness) is the only genuine freshness index.
#  - Molecular size: E2_E3 and SR are size/MW proxies that load in OPPOSITE
#    directions in this PCA, so their size signal is unreliable here.
# These group names must match the names of carbon_source_colors below exactly,
# or the group silently loses its colour in the loadings plot.
carbon_source_groups <- c(
  hix     = "Terrestrial / aged",
  suva254 = "Terrestrial / aged",
  fi      = "Terrestrial / aged",
  bix     = "Fresh / autochthonous",
  E2_E3   = "Molecular size",
  SR      = "Molecular size")
PCAloadings$carbon_source <- factor(
  carbon_source_groups[PCAloadings$Variables],
  levels = c("Terrestrial / aged", "Fresh / autochthonous", "Molecular size"))

# Display labels only (the data columns keep their raw names, which pca_vars still
# needs for column selection). These are plotmath expressions, rendered by the
# geom_text(parse = TRUE) below: SUVA[254] -> 254 subscript, E2*':'*E3 -> literal colon.
pretty_var_labels <- c(bix = "BIX", fi = "FI", hix = "HIX",
                       E2_E3 = "E2*':'*E3", SR = "SR", suva254 = "SUVA[254]")
PCAloadings$Variables <- pretty_var_labels[PCAloadings$Variables]

# Colourblind-safe set, distinct from the other plots (orange / teals / greens / navy):
# brown for terrestrial-aged, blue for fresh-autochthonous, grey for the ambiguous size axis.
carbon_source_colors <- c("Terrestrial / aged"          = "#8c510a",
                          "Fresh / autochthonous"       = "#2166ac",
                          "Molecular size"  = "#999999")

loadings_plot <- ggplot() +
  geom_segment(data = PCAloadings,
               aes(x = 0, y = 0, xend = PC1 * 4, yend = PC2 * 4, colour = carbon_source),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"), linewidth = 1) +
  geom_text(data = PCAloadings,
            aes(x = PC1 * 4.4, y = PC2 * 4, label = Variables, colour = carbon_source),
            size = 3.5, fontface = "bold", show.legend = FALSE, parse = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_colour_manual(name = "Index group", values = carbon_source_colors) +
  coord_cartesian(clip = "off") +
  labs(x = paste0("PC1 (", round(explained_variance[1], 3)*100, "%)"),
       y = paste0("PC2 (", round(explained_variance[2], 3)*100, "%)"),
       title = "PCA Loadings") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")

average_arrows_facet <- ggplot(data = average_arrow_data, aes(colour = day_no)) +
  geom_errorbar(data = summary_pca_scores, aes(x = PC1_mean, y = PC2_mean,
                                        xmin = PC1_mean - PC1_sd,
                                        xmax = PC1_mean + PC1_sd), alpha = 0.2) +
  geom_errorbar(data = summary_pca_scores, aes(x = PC1_mean, y = PC2_mean,
                                               ymin = PC2_mean - PC2_sd,
                                               ymax = PC2_mean + PC2_sd), alpha = 0.2)+
  geom_segment(aes(x = x_start, y = y_start,
                   xend = x_end, yend = y_end),
               arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
               linewidth = 1) +
  labs(x = paste0("PC1 (", round(explained_variance[1], 3)*100, "%)"),
       y = paste0("PC2 (", round(explained_variance[2], 3)*100, "%)"),
       color = "Day",  title = "PCA Scores") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom",
        legend.location = "panel") +
  scale_colour_manual(values = day_colors(day_levels))

# kept as a list so r/trajectory_analysis_wReservoir.R can reference figs_active$...
figs_active <- list(average_arrows_facet = average_arrows_facet,
                    loadings_plot = loadings_plot)

ggsave("output/plots/PCA_wReservoir_active_arrows.pdf",
       plot = average_arrows_facet, width = 8, height = 6)
