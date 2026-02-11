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
ggplot(data = trajectories_long, aes(x = day_no, y = Value, fill = Metric)) +
  geom_boxplot() +
  labs(x = "Day", y = "Value", fill = "Path") +
  scale_fill_manual(name = "Segment",
                    values = c("S1" = "#f1a226", "S2" = "#c0d8d8"),
                    labels = c("S1" = "C1 to C2", "S2" = "C2 to C3")) +
  theme_bw()

ggplot(data = mean_trajectory) +
  geom_point(aes(x = day_no, y = mean_path))

#### Trajectory length internal variation 
# This is not a very important metric for me since it compares only the change within the chains to other days. 
trajectory_variation <- data.frame(replicate = rownames(trajectoryInternalVariation(x)), trajectoryInternalVariation(x))
trajectory_variation <- merge(trajectory_variation, unique(pca_data[,c(1,2)]), by = "replicate")

mean_trajectory_variation <- trajectory_variation |>
  group_by(day_no) |>
  summarise(mean_variation = mean(internal_variance))

ggplot(data = trajectory_variation) +
  geom_boxplot(aes(x = day_no, y = internal_variance))

ggplot(data = mean_trajectory_variation) +
  geom_point(aes(x = day_no, y = mean_variation))


### Changes in direction
#### angles
trajectory_angles <- data.frame(replicate = rownames(trajectoryAngles(x)), trajectoryAngles(x))
trajectory_angles <- merge(trajectory_angles, unique(pca_data[,c(1,2)]), by = "replicate")

mean_angles <- trajectory_angles |>
  group_by(day_no) |>
  summarise(mean_angle = mean(S1.S2))

ggplot(data = trajectory_angles) +
  geom_boxplot(aes(x = day_no, y = S1.S2))

ggplot(data = mean_angles) +
  geom_point(aes(x = day_no, y = mean_angle))

#### Trajectory Directionality
trajectory_directionality <- data.frame(replicate = names(trajectoryDirectionality(x)), 
                                        trajectoryDirectionality = trajectoryDirectionality(x))
trajectory_directionality <- merge(trajectory_directionality, unique(pca_data[,c(1,2)]), by = "replicate")

mean_directionality <- trajectory_directionality |>
  group_by(day_no) |>
  summarise(mean_angle = mean(trajectoryDirectionality))

ggplot(data = trajectory_directionality) +
  geom_boxplot(aes(x = day_no, y = trajectoryDirectionality))

ggplot(data = mean_directionality) +
  geom_point(aes(x = day_no, y = mean_angle))

#### Trajectory Shifts 

#### Dynamic Variation: Maybe interesting with the replicates
# How muc variations are there in a set of replicates within a day?

