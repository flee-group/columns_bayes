library(ggplot2)
library(FactoMineR)
library(vegan)

# pca calculations
pca_data <- data.table::data.table(data_all)
pca_data <-pca_data[!(is.na(bix) & is.na(fi) & is.na(hix) & is.na(a254))]
pca_data[is.na(pca_data)] <- 0
pca_data$day_no <- factor(pca_data$day_no, 
                          levels = c("Day00", "Day1", "Day2","Day3", "Day7", "Day9", "Day10", "Day12", "Day14", "Day17"))

wine.pca <- prcomp(pca_data[, !c("replicate", "day_no", "col_no", "columnID", "day_number")], scale. = TRUE)
summary(wine.pca)

pca_results <- cbind(pca_data, wine.pca$x)

## plots
PCAloadings <- data.frame(Variables = rownames(wine.pca$rotation), wine.pca$rotation)

ggplot(data = pca_results, aes(x = PC1, y = PC2)) +
  geom_point(aes(fill = day_no, color = day_no, shape = col_no),  size = 4) +
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC1), yend = (PC2)), arrow = arrow(length = unit(1, "picas")), color = "black") +
  annotate("text", x = (PCAloadings$PC1 * 0), y = (PCAloadings$PC2 * 0),
           label = PCAloadings$Variables)


# Weighted PCA with FactoMineR
weighted_pca <- PCA(pca_data[, c("bix", "fi", "hix", "a254", "E2_E3", "SR", "DOC", "DN")], 
                    row.w = pca_data$weight,  # vector of row weights
                    scale.unit = TRUE,      # standardize variables
                    graph = FALSE)

# Extract scores (individual coordinates) for the biplot
pca_scores <- as.data.frame(weighted_pca$ind$coord)
pca_scores$replicate <- pca_data$replicate
pca_scores$day_no <- pca_data$day_no
pca_scores$col_no <- pca_data$col_no

# Convert to data.table for easier manipulation
pca_scores_dt <- as.data.table(pca_scores)

# Extract loadings (variable coordinates) for arrows
PCAloadings <- as.data.frame(weighted_pca$var$coord)
PCAloadings$Variables <- rownames(PCAloadings)

# Create the biplot
ggplot(data = pca_scores, aes(x = Dim.1, y = Dim.2)) +
  geom_point(aes(fill = day_no, color = day_no, shape = col_no), size = 4) +
  geom_segment(data = PCAloadings, 
               aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2), 
               arrow = arrow(length = unit(1, "picas")), 
               color = "black") +
  annotate("text", 
           x = PCAloadings$Dim.1 * 1.1, 
           y = PCAloadings$Dim.2 * 1.1,
           label = PCAloadings$Variables) +
  labs(x = paste0("PC1 (", round(weighted_pca$eig[1,2], 1), "%)"),
       y = paste0("PC2 (", round(weighted_pca$eig[2,2], 1), "%)")) +
  theme_bw()


# Create arrows: Column 1 → Column 2 → Column 3
arrow_data <- pca_scores_dt[, .(
  x_start = c(Dim.1[col_no == "Column 1"], Dim.1[col_no == "Column 2"]),
  y_start = c(Dim.2[col_no == "Column 1"], Dim.2[col_no == "Column 2"]),
  x_end = c(Dim.1[col_no == "Column 2"], Dim.1[col_no == "Column 3"]),
  y_end = c(Dim.2[col_no == "Column 2"], Dim.2[col_no == "Column 3"]),
  segment = c("1to2", "2to3")
), by = .(replicate, day_no)]

# Create the faceted plot
ggplot() +
  # Add ellipses for each day (will show one per facet)
  stat_ellipse(data = pca_scores,
               aes(x = Dim.1, y = Dim.2, 
                   fill = day_no),
               geom = "polygon",
               color = "black",
               alpha = 0.2,
               level = 0.95,
               linewidth = 0.8,
               show.legend = FALSE) +
  # Draw arrows for each replicate-day
  geom_segment(data = arrow_data, 
               aes(x = x_start, y = y_start, 
                   xend = x_end, yend = y_end,
                   color = replicate),
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               linewidth = 0.8,
               alpha = 0.7) +
  # Add points for each column
  geom_point(data = pca_scores, 
             aes(x = Dim.1, y = Dim.2, 
                 color = replicate, 
                 shape = col_no),
             size = 2.5) +
  # Add reference lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  # Facet by day
  facet_wrap(~day_no, ncol = 3) +
  # Labels with variance explained
  labs(x = paste0("PC1 (", round(weighted_pca$eig[1, 2], 1), "%)"),
       y = paste0("PC2 (", round(weighted_pca$eig[2, 2], 1), "%)"),
       color = "Replicate",
       shape = "Column") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "right")
# Create the loadings plot
loadings_plot <- ggplot() +
  # Draw loading arrows
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = Dim.1 * 4, yend = Dim.2 * 4),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"), linewidth = 1) +
  # Add variable labels
  geom_text(data = PCAloadings, aes(x = Dim.1 * 4.3, y = Dim.2 * 4.3, label = Variables),
            size = 4, fontface = "bold") +
  # Add reference lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  # Labels with variance explained
  labs(x = paste0("PC1 (", round(weighted_pca$eig[1, 2], 1), "%)"),
       y = paste0("PC2 (", round(weighted_pca$eig[2, 2], 1), "%)"),
       title = "PCA Loadings Plot") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))

