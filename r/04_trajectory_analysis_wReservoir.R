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

PC_COLS <- paste0("PC", 1:6)
SURVEY_MAP <- c("C0" = 0, "Column 1" = 1, "Column 2" = 2, "Column 3" = 3)
DAY_LEVELS <- c("Day0", "Day1", "Day2","Day3", "Day7", "Day10", "Day12", "Day14","Day17")
DAY_COLORS <- day_colors(DAY_LEVELS)   # from r/plotting_functions.R (sourced by 03)

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

mean_trajectory <- trajectories |>
  group_by(day_no) |>
  summarise(mean_path = mean(Path),
            S1 = mean(S1),
            S2 = mean(S2),
            S3 = mean(S3))

# Long data for the side by side boxplots (now three segments)
trajectories_long <- trajectories |>
  pivot_longer(cols = c(S1, S2, S3),
               names_to = "Metric",
               values_to = "Value")

length_plot <- ggplot(data = trajectories_long, aes(x = day_no, y = Value, fill = Metric)) +
  geom_boxplot() +
  labs(x = "Day", y = "Trajectory Lengths", fill = "Path") +
  scale_fill_manual(name = "Segment",
                    values = c("S1" = "#D0F1BF", "S2" = "#86AF83", "S3" = "#4E7B4A"),
                    labels = c("S1" = "C0 to C1", "S2" = "C1 to C2", "S3" = "C2 to C3")) +
  theme_bw()

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

angles_long <- trajectory_angles |>
  pivot_longer(cols = c(S1.S2, S2.S3),
               names_to = "AnglePair",
               values_to = "Angle")

angle_plot <- ggplot(data = angles_long) +
  geom_boxplot(aes(x = day_no, y = Angle, fill = AnglePair)) +
  theme_bw() +
  labs(x = "Day", y = "Trajectory Angle", fill = "Angle pair") +
  scale_fill_manual(values = c("S1.S2" = "#c0d8d8", "S2.S3" = "#1f6f6f"),
                    labels = c("S1.S2" = "C0-C1-C2", "S2.S3" = "C1-C2-C3"))

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
    `C0-C1-C2 \n angle` = signif(S1.S2, 3),
    `C1-C2-C3 \n angle` = signif(S2.S3, 2))
print(combined_table)


## Grouped figure: reuse the arrow plot from 03_pca_wReservoir.R + the table
library(patchwork)
library(ggpubr)

table_plot <- ggpubr::ggtexttable(combined_table, rows = NULL,
                                  theme = ggpubr::ttheme("light"))

combined_plot_active <- (figs_active$average_arrows_facet + figs_active$loadings_plot) /
  (length_plot + angle_plot + directionality_plot) +
  patchwork::plot_layout(heights = c(1, 0.5))

# 3-section layout: PCA arrows | loadings side by side (top), lengths/angles table (bottom)
combined_plot_table_active <- (figs_active$average_arrows_facet + figs_active$loadings_plot) /
  table_plot +
  patchwork::plot_layout(heights = c(1.5, 1))

pdf("output/plots/trajectory_wReservoir_active.pdf", width = 14, height = 10)
plot(combined_plot_active)
dev.off()

pdf("output/plots/trajectory_wReservoir_active_table.pdf", width = 12, height = 10)
plot(combined_plot_table_active)
dev.off()
