# 4-state trajectory analysis WITH the reservoir: C0 -> C1 -> C2 -> C3.
#
# The reservoir is added as state 0. There is a SINGLE averaged reservoir score
# (one row in the PCA); it is reused as state 0 for every replicate, so every
# trajectory starts from the same point. Segments become S1 = C0->C1,
# S2 = C1->C2, S3 = C2->C3.
#
# Run r/03_pca_wReservoir.R first (provides scores_active / figs_active).

library(ecotraj)
library(vegan)
library(data.table)
library(dplyr)
library(tidyr)

# PCs used for all trajectory lengths/angles: the first 4 (~85% of the variance, see the
# scree/eigenvalue plots in 03)
PC_COLS <- paste0("PC", 1:4)
SURVEY_MAP <- c("C0" = 0, "Column 1" = 1, "Column 2" = 2, "Column 3" = 3)
DAY_LEVELS <- c("Day0", "Day1", "Day2","Day3", "Day7", "Day10", "Day12", "Day14","Day17")
DAY_COLORS <- day_colors(DAY_LEVELS)   # from r/plotting_functions.R (sourced by 03)

# x-axis labels "Day0\n(n=4)": number of trajectories per day in the plotted data
day_n_labels <- function(day_no) {
  n <- table(day_no)
  stats::setNames(paste0(names(n), "\n(n=", n, ")"), names(n))
}

# Build a 4-state ecotraj trajectory object from a PCA scores data.table.
# The single averaged reservoir (C0) score is reused as state 0 for every replicate.
build_trajectories_wReservoir <- function(scores_dt) {
  col_scores <- scores_dt[col_no != "C0",
                          c(PC_COLS, "replicate", "day_no", "col_no"), with = FALSE]
  # replicate letters are reused across days, so a trajectory is one replicate on one day
  col_scores[, traj_id := paste(day_no, replicate, sep = "_")]
  # drop incomplete trajectories (e.g. Day2 B has no Column 1): with a missing column,
  # ecotraj would shift the remaining segments into the wrong S1/S2/S3 slots
  complete_ids <- col_scores[, .N, by = traj_id][N == 3L, traj_id]
  col_scores <- col_scores[traj_id %in% complete_ids]
  res_score <- scores_dt[col_no == "C0", ..PC_COLS]   # one averaged reservoir row

  rep_day <- unique(col_scores[, .(traj_id, replicate, day_no)])
  # reuse the single reservoir score as the state-0 start for EVERY replicate
  res_dup <- cbind(rep_day, res_score[rep(1L, nrow(rep_day))])
  res_dup[, col_no := "C0"]

  augmented <- rbind(col_scores, res_dup, use.names = TRUE)
  augmented[, survey := SURVEY_MAP[as.character(col_no)]]
  setorder(augmented, traj_id, survey)

  d <- vegdist(as.matrix(augmented[, ..PC_COLS]), method = "euclidian")
  list(
    traj = defineTrajectories(d, augmented$traj_id, augmented$survey),
    rep_day = rep_day)
}

# ---- primary analysis: the ACTIVE pca scores ----
bt_active <- build_trajectories_wReservoir(scores_active)
x <- bt_active$traj
rep_day_active <- bt_active$rep_day

## Trajectory lengths (now S1 = C0->C1, S2 = C1->C2, S3 = C2->C3)
trajectories <- data.frame(
  traj_id = rownames(trajectoryLengths(x, relativeToInitial = FALSE)),
  trajectoryLengths(x, relativeToInitial = FALSE))
trajectories <- merge(trajectories, rep_day_active, by = "traj_id")
trajectories$day_no <- factor(trajectories$day_no, levels = DAY_LEVELS)

## Net change (Sturbois et al. 2021): direct C0 -> C3 distance in the PC_COLS space.
## relativeToInitial = TRUE gives distances from state 0 to each later state; Lt1_t4 is C0 -> C3.
net_change <- trajectoryLengths(x, relativeToInitial = TRUE)
trajectories$NetChange <- net_change[trajectories$traj_id, "Lt1_t4"]

mean_trajectory <- trajectories |>
  group_by(day_no) |>
  summarise(mean_path = mean(Path),
            S1 = mean(S1),
            S2 = mean(S2),
            S3 = mean(S3),
            NetChange = mean(NetChange))

# Long data for the side by side boxplots (three segments + the direct C0 -> C3 distance)
trajectories_long <- trajectories |>
  pivot_longer(cols = c(S1, S2, S3, NetChange),
               names_to = "Metric",
               values_to = "Value") |>
  mutate(Metric = factor(Metric, levels = c("S1", "S2", "S3", "NetChange")))

# y-axis break: squeeze the empty band between the bulk of the lengths and the single
# Day7 outlier into a short gap marked "//" (coord transform, so box stats are unaffected)
Y_BREAK <- c(9.6, 11.1)
stopifnot(!any(trajectories_long$Value > Y_BREAK[1] & trajectories_long$Value < Y_BREAK[2]))
axis_gap_trans <- function(lo, hi, gap = 0.6) {
  scales::trans_new("axis_gap",
    transform = function(y) ifelse(y <= lo, y,
                  ifelse(y >= hi, y - (hi - lo) + gap, lo + (y - lo) * gap / (hi - lo))),
    inverse   = function(y) ifelse(y <= lo, y,
                  ifelse(y >= lo + gap, y + (hi - lo) - gap, lo + (y - lo) * (hi - lo) / gap)))
}

length_plot <- ggplot(data = trajectories_long, aes(x = day_no, y = Value, fill = Metric)) +
  geom_boxplot() +
  labs(x = "Day", y = "Trajectory Lengths", fill = "Path") +
  scale_fill_manual(name = "Segment",
                    values = c("S1" = "#D0F1BF", "S2" = "#86AF83", "S3" = "#4E7B4A",
                               "NetChange" = "#1f3d1c"),
                    labels = c("S1" = "C0 to C1", "S2" = "C1 to C2", "S3" = "C2 to C3",
                               "NetChange" = "C0 to C3 (direct)")) +
  scale_x_discrete(labels = day_n_labels(trajectories$day_no)) +
  scale_y_continuous(breaks = c(2.5, 5, 7.5, 12)) +
  coord_transform(y = axis_gap_trans(Y_BREAK[1], Y_BREAK[2]), clip = "off") +
  annotate("text", x = c(-Inf, Inf), y = mean(Y_BREAK), label = "//", size = 5) +
  theme_bw() +
  theme(legend.position = "bottom")

### Angles (now two pairs: S1.S2 and S2.S3)
trajectory_angles <- data.frame(
  traj_id = rownames(trajectoryAngles(x)), trajectoryAngles(x))
trajectory_angles <- merge(trajectory_angles, rep_day_active, by = "traj_id")
trajectory_angles$day_no <- factor(trajectory_angles$day_no, levels = DAY_LEVELS)

mean_angles <- trajectory_angles |>
  group_by(day_no) |>
  summarise(
    S1.S2 = mean(circular::circular(S1.S2, units = "degrees", type = "angles")),
    S2.S3 = mean(circular::circular(S2.S3, units = "degrees", type = "angles")))

# Turning angles as drawn in the PCA scores plot: between the day-averaged arrows
# (average_arrow_data from 03) in the PC1-PC2 plane only. Same definition as ecotraj
# (0 = straight on, 180 = full reversal), but on the averaged 2-D path, so it differs from
# the per-replicate 4-D (PC1-PC4) means above.
turning_angle <- function(u, v) {
  acos(pmin(pmax(sum(u * v) / sqrt(sum(u^2) * sum(v^2)), -1), 1)) * 180 / pi
}
plot_angles_2d <- average_arrow_data[, {
  seg <- function(s) c(x_end[segment == s] - x_start[segment == s],
                       y_end[segment == s] - y_start[segment == s])
  .(S1.S2 = turning_angle(seg("C0to1"), seg("1to2")),
    S2.S3 = turning_angle(seg("1to2"), seg("2to3")))
}, by = day_no]

angle_comparison <- merge(
  as.data.frame(plot_angles_2d),
  data.frame(day_no = mean_angles$day_no,
             S1.S2 = as.numeric(mean_angles$S1.S2),
             S2.S3 = as.numeric(mean_angles$S2.S3)),
  by = "day_no", suffixes = c("_2D", "_4D")) |>
  mutate(day_no = factor(day_no, levels = DAY_LEVELS)) |>
  arrange(day_no)
print(angle_comparison)

angle_comparison_long <- angle_comparison |>
  pivot_longer(-day_no, names_to = c("AnglePair", ".value"), names_sep = "_")

angle_2d_4d_plot <- ggplot(angle_comparison_long, aes(x = `2D`, y = `4D`, fill = day_no)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "gray50") +
  geom_point(shape = 21, size = 3, colour = "black") +
  facet_wrap(~ AnglePair,
             labeller = as_labeller(c("S1.S2" = "C0-C1-C2", "S2.S3" = "C1-C2-C3"))) +
  scale_fill_manual(values = DAY_COLORS, name = NULL) +
  scale_x_continuous(limits = c(0, 180), breaks = seq(0, 180, 45)) +
  scale_y_continuous(limits = c(0, 180), breaks = seq(0, 180, 45)) +
  coord_equal() +
  labs(x = "Turning angle in PCA plot (PC1-PC2, day-averaged arrows)",
       y = "Mean turning angle (PC1-PC4, per replicate)") +
  theme_bw() +
  theme(legend.position = "bottom")

angles_long <- trajectory_angles |>
  pivot_longer(cols = c(S1.S2, S2.S3),
               names_to = "AnglePair",
               values_to = "Angle")

angle_plot <- ggplot(data = angles_long) +
  geom_boxplot(aes(x = day_no, y = Angle, fill = AnglePair)) +
  theme_bw() +
  labs(x = "Day", y = "Trajectory Angle", fill = "Segment pair") +
  scale_fill_manual(values = c("S1.S2" = "#ec8a3c", "S2.S3" = "#b8322a"),
                    labels = c("S1.S2" = "C0-C1-C2", "S2.S3" = "C1-C2-C3")) +
  scale_x_discrete(labels = day_n_labels(trajectory_angles$day_no)) +
  theme(legend.position = "bottom")

#### Directionality (one value per trajectory, unchanged)
trajectory_directionality <- data.frame(
  traj_id = names(trajectoryDirectionality(x)),
  trajectoryDirectionality = trajectoryDirectionality(x))
trajectory_directionality <- merge(trajectory_directionality, rep_day_active, by = "traj_id")
trajectory_directionality$day_no <- factor(trajectory_directionality$day_no, levels = DAY_LEVELS)

directionality_plot <- ggplot(data = trajectory_directionality) +
  geom_boxplot(aes(x = day_no, y = trajectoryDirectionality, fill = day_no)) +
  theme_bw() +
  labs(x = "Day", y = "Trajectory Directionality") +
  scale_fill_manual(values = DAY_COLORS) +
  guides(fill = "none")

# Combined summary table (now with Segment 3 and both angle pairs)
combined_table <- merge(mean_trajectory, mean_angles, by = "day_no") |>
  arrange(factor(day_no, levels = DAY_LEVELS)) |>
  transmute(
    Day = day_no,
    `Segment 1 \n Length` = signif(S1, 2),
    `Segment 2 \n Length` = signif(S2, 2),
    `Segment 3 \n Length` = signif(S3, 2),
    `Total \n length` = signif(mean_path, 2),
    `C0 to C3 \n distance` = signif(NetChange, 2),
    `C0-C1-C2 \n angle` = signif(S1.S2, 3),
    `C1-C2-C3 \n angle` = signif(S2.S3, 2))
print(combined_table)


## Grouped figure: reuse the arrow plot from 03_pca_wReservoir.R + lengths/angles
library(patchwork)

combined_plot_active <- (figs_active$average_arrows_facet + figs_active$loadings_plot) /
  (length_plot + angle_plot + directionality_plot) +
  patchwork::plot_layout(heights = c(1, 0.5))

# PCA arrows | loadings side by side (top), smaller lengths | angles plots (bottom)
combined_plot_table_active <- (figs_active$average_arrows_facet + figs_active$loadings_plot) /
  (length_plot + angle_plot) +
  # bottom row panels fixed at 10.9 cm; the top row takes the rest of the page (~12.6 cm)
  patchwork::plot_layout(heights = unit(c(1, 10.9), c("null", "cm")))

pdf("output/plots/trajectory_wReservoir_active.pdf", width = 14, height = 10)
plot(combined_plot_active)
dev.off()

pdf("output/plots/trajectory_wReservoir_active_table.pdf", width = 12, height = 13 - 2 / 2.54)
plot(combined_plot_table_active)
dev.off()

ggsave("output/plots/angles_2D_vs_4D.pdf", plot = angle_2d_4d_plot, width = 8, height = 5)
