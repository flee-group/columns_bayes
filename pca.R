library(ggplot2)
library(FactoMineR)

# pca calculations
pca_data <- data.table::data.table(data_all)
pca_data <-pca_data[!(is.na(bix) & is.na(fi) & is.na(hix) & is.na(a254))]
pca_data[is.na(pca_data)] <- 0

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
pca_scores$day_no <- pca_data$day_no
pca_scores$col_no <- pca_data$col_no

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
