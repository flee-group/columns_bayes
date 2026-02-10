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
## Trajectory metrics
### Changes in ecological states
#### Trajectory Lenght - Changes in the ecological state
trajectories <- data.frame(replicate = rownames(trajectoryLengths(x, relativeToInitial = FALSE)), trajectoryLengths(x, relativeToInitial = FALSE))
trajectories <- merge(trajectories, unique(pca_data[,c(1,2)]), by = "replicate")

mean_trajectory <- trajectories |>
  group_by(day_no) |>
  summarise(mean_path = mean(Path))

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
  summarise(mean_angle = mean(S1.S2))

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
  summarise(mean_angle = mean(trajectoryDirectionality))

directionality_plot <- ggplot(data = trajectory_directionality) +
  geom_boxplot(aes(x = day_no, y = trajectoryDirectionality, fill = day_no)) +
  theme_bw() +
  labs(x = "Day", y = "Trajectory Directionality") +
  scale_fill_manual(values = c("#f1a226", "#9fc8c8", "#54a1a1", "#1f6f6f"))

ggplot(data = mean_directionality) +
  geom_point(aes(x = day_no, y = mean_angle))

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

#### Centering the trajectories
# Get centered trajectories
centered_x <- centerTrajectories(x)

# Get PCoA coordinates from the centered distance matrix
pcoa_centered <- cmdscale(centered_x$d, k = 2)

# Define colors for each day
unique_days <- sort(unique(pca_data$day_no))
day_colors <- c("#f1a226", "#9fc8c8", "#54a1a1", "#1f6f6f")
names(day_colors) <- as.character(unique_days)

# Get replicate-day mapping
replicate_days <- unique(pca_data[, c("day_no", "replicate")])

# ============ COMBINED PLOT WITH ALL CENTERED TRAJECTORIES ============
entities <- pca_data$replicate
surveys <- as.numeric(substr(pca_data$col_no, 8, 8))
unique_entities <- unique(entities)

# Create the plot
plot(pcoa_centered[, 1], pcoa_centered[, 2], 
     type = "n",
     xlab = "PCoA 1 (centered)", 
     ylab = "PCoA 2 (centered)",
     main = "Centered Trajectories: All Days Combined")

# Draw trajectories colored by day
for(ent in unique_entities) {
  # Get indices for this entity
  idx <- which(entities == ent)
  
  # Get day for this entity to determine color
  day <- as.character(pca_data$day_no[idx[1]])
  col <- day_colors[day]
  
  # Draw arrows connecting the points (Column 1 -> 2 -> 3)
  for(i in 1:(length(idx)-1)) {
    arrows(pcoa_centered[idx[i], 1], pcoa_centered[idx[i], 2],
           pcoa_centered[idx[i+1], 1], pcoa_centered[idx[i+1], 2],
           col = col,
           lwd = 2, 
           length = 0.1)
  }
  
  # Optional: Add points at each position
  points(pcoa_centered[idx, 1], pcoa_centered[idx, 2], 
         pch = 19, 
         col = col, 
         cex = 0.5)
}

# Add legend
legend("topright", 
       legend = unique_days,
       col = day_colors,
       lty = 1, 
       lwd = 2,
       title = "Day",
       bty = "n")

#### Get average days
# Matrice of Euclidian distanced d between "states
averaged_pca_matrix <- pca_scores |>
  group_by(day_no, col_no) |>
  summarise(across(PC1:PC6, mean, .names = "{.col}"),
            .groups = "drop")|>
  as.data.frame()

d_averaged <- vegdist(as.matrix(averaged_pca_matrix[, 3:8]), method = "euclidean")

entities <- averaged_pca_matrix$day_no
surveys <- as.numeric(substr(averaged_pca_matrix$col_no, 8, 8))

# Define trajectories
trajectories_averaged <- defineTrajectories(d_averaged, entities, surveys)
centered_x <- centerTrajectories(x_centered)

unique_days <- sort(unique(replicate_days$day_no))
day_colors <- c("#f1a226", "#9fc8c8", "#54a1a1", "#1f6f6f")

# Get PCoA coordinates from the centered distance matrix
pcoa_averaged_centered <- cmdscale(centered_x$d, k = 6)

# Create the plot
plot(pcoa_averaged_centered[, 1], pcoa_averaged_centered[, 2], 
     type = "n",
     xlab = "PCoA 1 (centered)", 
     ylab = "PCoA 2 (centered)",
     main = "Centered Trajectories: All Days Combined")

# Draw trajectories colored by day
for(ent in unique_days) {
  # Get indices for this entity
  idx <- which(entities == ent)
  
  # Get day for this entity to determine color
  day <- as.character(averaged_pca_matrix$day_no[idx[1]])
  col <- day_colors[day]
  
  # Draw arrows connecting the points (Column 1 -> 2 -> 3)
  for(i in 1:(length(idx)-1)) {
    arrows(pcoa_averaged_centered[idx[i], 1], pcoa_averaged_centered[idx[i], 2],
           pcoa_averaged_centered[idx[i+1], 1], pcoa_averaged_centered[idx[i+1], 2],
           col = col,
           lwd = 2, 
           length = 0.1)
  }
  
  # Optional: Add points at each position
  points(pcoa_averaged_centered[idx, 1], pcoa_averaged_centered[idx, 2], 
         pch = 19, 
         col = col, 
         cex = 0.5)
}

# Add legend
legend("topright", 
       legend = unique_days,
       col = day_colors,
       lty = 1, 
       lwd = 2,
       title = "Day",
       bty = "n")


## grouped plot
patchwork::(average_arrows_facet+loadings_plot)/(length_plot+angle_plot+directionality_plot)

