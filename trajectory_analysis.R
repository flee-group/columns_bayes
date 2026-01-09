library(ecotraj)

entities <- pca_data$replicate
surveys <- as.numeric(substr(pca_data$col_no, 8, 8))

# Matrice of Euclidian distanced d between "states
d <- vegdist(wine.pca$x[, 1:6], method = "euclidian")

# Define trajectories
x <- defineTrajectories(d, entities, surveys)

x23s <- subsetTrajectories(x, 
                           site_selection = c("A"))

# display trajectories
plot <- trajectoryPlot(x, lwd = 1, sites = 
               survey.labels = FALSE)
legend("topright", , 
       legend=c("Entity 1", "Entity 2", "Entity 3"), bty="n", lty=1, lwd = 2)
