library(ecotraj)

entities <- pca_data$replicate
surveys <- as.numeric(substr(pca_data$col_no, 8, 8))

# Matrice of Euclidian distanced d between "states
d <- vegdist(wine.pca$x[, 1:6], method = "euclidian")

# Define trajectories
x <- defineTrajectories(d, entities, surveys)

replicate_days <- unique(pca_data[,c("day_no", "replicate")])
unique_days <- sort(unique(replicate_days$day_no))
day_colors <- c("#f1a226", "#9fc8c8", "#54a1a1", "#1f6f6f")
x_Day0 <- subsetTrajectories(x, 
                             site_selection = as.vector(replicate_days[replicate_days[,day_no == "Day0"]]$replicate))

# ============ CREATE SEPARATE PLOTS FOR EACH DAY ============
par(mfrow = c(2, 2))  # 2x2 grid for 4 days

for(i in 1:length(unique_days)) {
  day <- unique_days[i]
  
  # Get replicates for this day
  day_replicates <- as.character(replicate_days[replicate_days$day_no == day, ]$replicate)
  
  # Get row indices for this day in pca_data
  day_indices <- which(pca_data$day_no == day)
  
  # Get PCA coordinates for this day using YOUR ORIGINAL PCA
  pca_day <- wine.pca$x[day_indices, 1:2]
  
  # Create plot with original PCA coordinates
  plot(pca_day[, 1], pca_day[, 2], type = "n",
       main = paste("Trajectories:", day),
       xlab = "PC1 (28%)", ylab = "PC2 (21%)")
  
  # Draw trajectories manually with the color for this day
  for(rep in day_replicates) {
    # Get the rows for this replicate within day_indices
    rep_rows <- which(pca_data$replicate == rep & pca_data$day_no == day)
    
    # Draw arrows connecting columns 1->2->3
    for(j in 1:(length(rep_rows)-1)) {
      arrows(wine.pca$x[rep_rows[j], 1], wine.pca$x[rep_rows[j], 2],
             wine.pca$x[rep_rows[j+1], 1], wine.pca$x[rep_rows[j+1], 2],
             col = day_colors[i],  # Use the color for this day
             lwd = 2, length = 0.1)
    }
  }
}

par(mfrow = c(1, 1))


trajectory_scores <- trajectoryPCoA(x)[["points"]][,1:6]

## Trajectory metrics
### Changes in ecological states
#### Trajectory Lenght - Changes in the ecological state
trajectories <- data.frame(replicate = rownames(trajectoryLengths(x, relativeToInitial = FALSE)), trajectoryLengths(x, relativeToInitial = FALSE))
trajectories <- merge(trajectories, unique(pca_data[,c(1,2)]), by = "replicate")

mean_trajectory <- trajectories |>
  group_by(day_no) |>
  summarise(mean_path = mean(Path), 
            S1 = mean(S1),
            S2 = mean(S2))

# Long data for the side by side boxplots
trajectories_long <- trajectories |>
  pivot_longer(cols = c(S1, S2), 
               names_to = "Metric", 
               values_to = "Value")

# Plot to show path lengths of each arrow
length_plot <- ggplot(data = trajectories_long, aes(x = day_no, y = Value, fill = Metric)) +
  geom_boxplot() +
  labs(x = "Day", y = "Trajectory Lengths", fill = "Path") +
  scale_fill_manual(name = "Segment",
                    values = c("S1" = "#D0F1BF", "S2" = "#86AF83"),
                    labels = c("S1" = "C1 to C2", "S2" = "C2 to C3")) +
  theme_bw()

ggplot(data = mean_trajectory) +
  geom_point(aes(x = day_no, y = mean_path))+
  theme_bw()

ggplot(data = trajectories) +
  geom_boxplot(aes(x = day_no, y =Path))+
  theme_bw()

#### Trajectory length internal variation 
# This is not a very important metric for me since it compares only the change within the chains to other days. 
trajectory_variation <- data.frame(replicate = rownames(trajectoryInternalVariation(x)), trajectoryInternalVariation(x))
trajectory_variation <- merge(trajectory_variation, unique(pca_data[,c(1,2)]), by = "replicate")

mean_trajectory_variation <- trajectory_variation |>
  group_by(day_no) |>
  summarise(mean_variation = mean(internal_variance))

ggplot(data = trajectory_variation) +
  geom_boxplot(aes(x = day_no, y = internal_variance, fill = day_no)) +
  theme_bw() +
  scale_fill_manual(values = c("#f1a226", "#9fc8c8", "#54a1a1", "#1f6f6f")) + 
  xlab("Day") + ylab("Trajectory lenght internal variance")

ggplot(data = mean_trajectory_variation) +
  geom_point(aes(x = day_no, y = mean_variation)) +
  theme_bw()

### Changes in direction
#### angles
trajectory_angles <- data.frame(replicate = rownames(trajectoryAngles(x)), trajectoryAngles(x))
trajectory_angles <- merge(trajectory_angles, unique(pca_data[,c(1,2)]), by = "replicate")

mean_angles <- trajectory_angles |>
  group_by(day_no) |>
  summarise(mean_angle = mean(circular::circular(S1.S2, units = "degrees", type = "angles")))

angle_plot <- ggplot(data = trajectory_angles) +
  geom_boxplot(aes(x = day_no, y = S1.S2, fill = day_no)) +
  theme_bw() + theme(legend.position = "none") +
  labs(x = "Day", y = "Trajectory Angle") +
  scale_fill_manual(values = c("#f1a226", "#9fc8c8", "#54a1a1", "#1f6f6f"))

ggplot(data = mean_angles) +
  geom_point(aes(x = day_no, y = mean_angle))

#### Trajectory Directionality
trajectory_directionality <- data.frame(replicate = names(trajectoryDirectionality(x)), 
                                        trajectoryDirectionality = trajectoryDirectionality(x))

trajectory_directionality <- merge(trajectory_directionality, unique(pca_data[,c(1,2)]), by = "replicate")

mean_directionality <- trajectory_directionality |>
  group_by(day_no) |>
  summarise(mean_angle = mean(circular::circular(trajectoryDirectionality, type = "directions")))

directionality_plot <- ggplot(data = trajectory_directionality) +
  geom_boxplot(aes(x = day_no, y = trajectoryDirectionality, fill = day_no)) +
  theme_bw() +
  labs(x = "Day", y = "Trajectory Directionality") +
  scale_fill_manual(values = c("#f1a226", "#9fc8c8", "#54a1a1", "#1f6f6f"))


#### Trajectory Shifts 
trajectoryShifts(x)
trajectoryShifts(subsetTrajectories(x, c("E","J")))


#### Trajectory Convergence
trajectoryConvergence(x, type = "pairwise.symmetric")
trajectoryConvergence(x, type = "pairwise.asymmetric")

trajectoryConvergencePlot(x, type = "both", radius = 2)
trajectoryConvergencePlot(subsetTrajectories(x, c("D","H", "K", "M")), type = "both", radius = 2)
trajectoryConvergencePlot(subsetTrajectories(x, c("E","J", "L", "N")), type = "both", radius = 2)


trajectoryConvergence(x, type = "multiple")

#### Centering Trajectories
Ds <- segmentDistances(x)$Dseg
mMDS <- smacof::mds(Ds)
mMDS

xret <- mMDS$conf
plot(xret, xlab="axis 1", ylab = "axis 2", asp=1, pch=21,
     bg=c(rep("black",3), rep("red",3), rep("blue",3)), 
     xlim=c(-1.5,1), ylim=c(-1,1.5))
text(xret, labels=rep(paste0("s",1:3),3), pos=1)
legend("topleft", pt.bg=c("black","red","blue"), pch=21, bty="n", legend=c("Trajectory 1", "Trajectory 2", "Trajectory 3"))

#### Distances Between Trajectories
trajectoryDistances(x, distance.type = "Hausdorff")
trajectoryDistances(x, distance.type = "SPD")
trajectoryDistances(x, distance.type = "DSPD", symmetrization = NULL)


#### Dynamic Variation: Maybe interesting with the replicates
# How much variations are there in a set of replicates within a day?

dynamicVariation(x)
dynamicVariation(subsetTrajectories(x, c("D","H", "K", "M")))
dynamicVariation(subsetTrajectories(x, c("E","J", "L", "N")))
dynamicVariation(subsetTrajectories(x, c("A","G", "I", "P")))
dynamicVariation(subsetTrajectories(x, c("B","C", "F")))

proc <- procrustes(trajectory_pcoa, pca_pts)

# Combine into one table
combined_table <- data.frame(
  Day = rownames(lengths),
  `Segment 1 \n Length` = signif(mean_trajectory$S1, 2),
  `Segment 2 \n Length` = signif(mean_trajectory$S2, 2),
  `Total \n length` = signif(mean_trajectory$mean_path, 2),
  `Segment \n angle` = signif(mean_angles[,2], 2),
  check.names = FALSE  # Keep spaces in column names
)

# Display the table
print(combined_table)


## grouped plot
library(patchwork)
(average_arrows_facet+loadings_plot)/(length_plot+angle_plot+directionality_plot)

library(patchwork)
library(ggpubr)

# Create the table as a ggplot object
table_plot <- ggtexttable(combined_table, rows = NULL, 
                          theme = ttheme("light"))

# Combine with patchwork
(average_arrows_facet + loadings_plot) / table_plot + 
  plot_layout(heights = c(1.5, 1)) 
                                      
# Trajectory analysis on the pure data
d_pure <- vegdist(scale(pca_data[, !c("replicate", "day_no", "col_no", "columnID", "day_number")], center = TRUE, scale = TRUE), method = "euclidian")
entities <- as.character(pca_data$replicate)
surveys <- as.numeric(substr(pca_data$col_no, 8, 8))

# Define trajectories
x_pure <- defineTrajectories(d_pure, entities, surveys)
plot_from_raw <- trajectoryPCoA(x_pure)

#angles
trajectory_angles_from_raw <- data.frame(replicate = rownames(trajectoryAngles(x_pure)), trajectoryAngles(x_pure))
trajectory_angles_from_raw <- merge(trajectory_angles_from_raw, unique(pca_data[,c(1,2)]), by = "replicate")

mean_angles_raw <- trajectory_angles_from_raw |>
  group_by(day_no) |>
  summarise(mean_angle = mean(circular::circular(S1.S2, units = "degrees", type = "angles")))

#trajectory length
trajectory_length_from_raw <- data.frame(replicate = rownames(trajectoryLengths(x_pure, relativeToInitial = FALSE)), trajectoryLengths(x_pure, relativeToInitial = FALSE))
trajectory_length_from_raw <-merge(trajectory_length_from_raw, unique(pca_data[,c(1,2)]), by = "replicate")

mean_trajectory_raw <- trajectory_length_from_raw |>
  group_by(day_no) |>
  summarise(mean_path = mean(Path), 
            S1 = mean(S1),
            S2 = mean(S2))

# plotting by colour
scores_raw <- cbind(trajectoryPCoA(x_pure)[["points"]][,1:6], pca_data[,1:3])

# Then build arrows from averaged positions
arrow_data_all <- scores_raw[, .(
  x_start = c(V1[col_no == "Column 1"], V1[col_no == "Column 2"]),
  y_start = c(V2[col_no == "Column 1"], V2[col_no == "Column 2"]),
  x_end   = c(V1[col_no == "Column 2"], V1[col_no == "Column 3"]),
  y_end   = c(V2[col_no == "Column 2"], V2[col_no == "Column 3"]),
  segment = c("1to2", "2to3")
), by = .(day_no, replicate)]

explained_variance_pcoa <- abs(plot_from_raw$eig)/sum(abs(plot_from_raw$eig))

arrows_per_replicate <- ggplot(data = arrow_data_all, aes(colour = day_no)) +
  geom_segment(aes(x = x_start, y = y_start, 
                   xend = x_end, yend = y_end),
               arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
               linewidth = 1) +
  labs(x = paste0("PCoA1 (", round(explained_variance_pcoa[1], 3)*100, "%)"),
       y = paste0("PC2 (", round(explained_variance_pcoa[2], 3)*100, "%)"),
       color = "Day",  title = "PCA Scores") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom",
        legend.location = "panel") +
  scale_colour_manual(values = (c("#f1a226", "#c0d8d8","#7dc5c5","#1f6f6f"))) 

# Combine into one table
combined_table_raw <-  merge(
  mean_trajectory_raw[, c("day_no", "S1", "S2", "mean_path")],
  mean_angles_raw,
  by = "day_no"
) |>
  rename(`Day` = day_no,
         `Segment \n angle` = mean_angle, 
         `Segment 1 \n Length` = S1,
         `Segment 2 \n Length` = S2,
         `Total \n length` = mean_path) |>
  arrange(Day) |>
  mutate(across(where(is.numeric), ~ signif(.x, 2))) 

combined_table_raw$Day <- factor(combined_table_raw$Day,
       levels = c("Day0", "Day1", "Day2","Day3", "Day7", "Day9", "Day10", "Day12", "Day14", "Day17"), 
       labels = c("0", "1", "2", "3", "7", "9", "10", "12", "14", "17"))

# Create the table as a ggplot object
table_plot_raw <- ggpubr::ggtexttable(combined_table_raw, rows = NULL, theme = ggpubr::ttheme("light"))

# Combine with patchwork
combined_plot <- (arrows_per_replicate) / table_plot_raw + 
  patchwork::plot_layout(heights = c(1.5, 1)) 

pdf('output/plots/PcoA_and_table.pdf', width = 4.3, height = 5.9)
plot(combined_plot)
dev.off()
