library(ggplot2)
library(FactoMineR)
library(vegan)
library(data.table)

# pca calculations
pca_data <- data.table::data.table(data_all)
pca_data <- pca_data[!(is.na(bix) & is.na(fi) & is.na(hix) & is.na(a254))]
pca_data[is.na(pca_data)] <- 0
pca_data$day_no <- factor(pca_data$day_no, 
                          levels = c("Day0", "Day1", "Day2","Day3", "Day7", "Day9", "Day10", "Day12", "Day14", "Day17"))
pca_data <- pca_data |>
  filter(day_no != "Day9") |>
  mutate(suva254 = a254/DOC) |>
  select(-c("a254", "DOC", "DN")) |>
  filter(day_no %in% c("Day0", "Day3", "Day10", "Day17"))|> # remove all the other days for reduced data
  filter(day_no != "Day3" | replicate %in% c("L", "N", "E", "J")) # jsut keeping these replicates from DAy 3


wine.pca <- prcomp(pca_data[, !c("replicate", "day_no", "col_no", "columnID", "day_number")], scale. = TRUE)
summary(wine.pca)

pca_results <- cbind(pca_data, wine.pca$x)

## plots
PCAloadings <- data.frame(Variables = rownames(wine.pca$rotation), wine.pca$rotation)

ggplot(data = pca_results, aes(x = PC1, y = PC2)) +
  geom_point(aes(fill = day_no, color = day_no, shape = col_no),  size = 4) +
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC1)*1.5, yend = (PC2)*1.5), arrow = arrow(length = unit(1, "picas")), color = "black") +
  annotate("text", x = (PCAloadings$PC1 * 2), y = (PCAloadings$PC2 * 2),
           label = PCAloadings$Variables)

# Weighted PCA with FactoMineR

#day_col_counts <- pca_data[, .N, by = .(day_no, col_no)]
#pca_data[, weight := 1 / .N, by = .(day_no, col_no)]
#pca_data[, weight := weight / sum(weight) * .N]

#weighted_pca <- FactoMineR::PCA(pca_data[, c("bix", "fi", "hix", "suva254", "E2_E3", "SR")], 
 #                   row.w = pca_data$weight,  # vector of row weights
  #                  scale.unit = TRUE,      # standardize variables
   #                 graph = FALSE)

# Extract scores (individual coordinates) for the biplot
pca_scores <- as.data.frame(wine.pca$x)
#pca_scores <- as.data.frame(weighted_pca$ind$coord)
pca_scores$replicate <- pca_data$replicate
pca_scores$day_no <- pca_data$day_no
pca_scores$col_no <- pca_data$col_no

# Convert to data.table for easier manipulation
pca_scores_dt <- as.data.table(pca_scores)

# Extract loadings (variable coordinates) for arrows
#PCAloadings <- as.data.frame(weighted_pca$var$coord)
PCAloadings <- as.data.frame(wine.pca$rotation)
PCAloadings$Variables <- rownames(PCAloadings)
explained_variance <- wine.pca$sdev^2 / sum(wine.pca$sdev^2)



# Create the biplot
ggplot(data = pca_scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(fill = day_no, color = day_no, shape = col_no), size = 4) +
  geom_segment(data = PCAloadings, 
               aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(length = unit(1, "picas")), 
               color = "black") +
  annotate("text", 
           x = PCAloadings$PC1 * 1.1, 
           y = PCAloadings$PC2 * 1.1,
           label = PCAloadings$Variables) +
  labs(x = paste0("PC1 (", round(explained_variance[1], 2), "%)"),
       y = paste0("PC2 (", round(explained_variance[2], 2), "%)")) +
  theme_bw()


# Create arrows: Column 1 → Column 2 → Column 3
arrow_data <- pca_scores_dt[, .(
  x_start = c(PC1[col_no == "Column 1"], PC1[col_no == "Column 2"]),
  y_start = c(PC2[col_no == "Column 1"], PC2[col_no == "Column 2"]),
  x_end = c(PC1[col_no == "Column 2"], PC1[col_no == "Column 3"]),
  y_end = c(PC2[col_no == "Column 2"], PC2[col_no == "Column 3"]),
  segment = c("1to2", "2to3")
), by = .(replicate, day_no)]

arrow_data_subset <- arrow_data[day_no %in% c("Day0", "Day3", "Day10", "Day17")]

# Create the faceted plot
ggplot() +
  # Add ellipses for each day (will show one per facet)
  stat_ellipse(data = pca_scores,
               aes(x = PC1, y = PC2, 
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
             aes(x = PC1, y = PC2, 
                 color = replicate, 
                 shape = col_no),
             size = 2.5) +
  # Add reference lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  # Facet by day
  facet_wrap(~day_no, ncol = 3) +
  # Labels with variance explained
  labs(x = paste0("PC1 (", round(explained_variance[1], 3)*100, "%)"),
       y = paste0("PC2 (", round(explained_variance[2], 3)*100, "%)"),
       color = "Replicate",
       shape = "Column") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "right")

# Create the loadings plot
loadings_plot <- ggplot() +
  # Draw loading arrows
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = PC1 * 4, yend = PC2 * 4),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"), linewidth = 1) +
  # Add variable labels
  geom_text(data = PCAloadings, aes(x = PC1 * 4.3, y = PC2 * 4.3, label = Variables),
            size = 4, fontface = "bold") +
  # Add reference lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  # Labels with variance explained
  labs(x = paste0("PC1 (", round(explained_variance[1], 3)*100, "%)"),
       y = paste0("PC2 (", round(explained_variance[2], 3)*100, "%)"),
       title = "PCA Loadings Plot") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))


# Create the plot
arrow_facets <- ggplot(data = arrow_data) +
  # Draw arrows for each replicate-day
  facet_wrap(~day_no)+
  geom_segment(aes(x = x_start, y = y_start, 
                   xend = x_end, yend = y_end,
                   color = day_no),
               arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
               linewidth = 0.5,
               alpha = 1) +
  # Add reference lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  # Labels with variance explained
  labs(x = paste0("PC1 (", round(explained_variance[1], 3)*100, "%)"),
       y = paste0("PC2 (", round(explained_variance[2], 3)*100, "%)"),
       color = "Day") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "right")+
  scale_colour_manual(values = (c("#f1a226", "#e0c0b0", "#c0d8d8", "#9fc8c8","#7dc5c5",
                        "#65c2c2", "#54a1a1", "#428f8f","#307d7d","#1f6f6f")))

average_arrow_data <- arrow_data |>
  group_by(day_no, segment) |>
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

pca_scores_dt |>
  group_by(day_no, col_no) |>
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

average_arrows_facet <- ggplot(data = arrow_data) +
  # Draw arrows for each replicate-day
  facet_wrap(~day_no)+
  geom_segment(data = average_arrow_data, aes(x = x_start, y = y_start, 
                   xend = x_end, yend = y_end,
                   color = day_no),
               arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
               linewidth = 1) +
  # Add reference lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  # Labels with variance explained
  labs(x = paste0("PC1 (", round(explained_variance[1], 3)*100, "%)"),
       y = paste0("PC2 (", round(explained_variance[2], 3)*100, "%)"),
       color = "Day") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "right") +
  scale_colour_manual(values = (c("#f1a226", "#c0d8d8","#7dc5c5","#1f6f6f")))
  
arrow_data <- arrow_data |>
  mutate(length_x = x_end - x_start, 
         length_y = y_end - y_start)

ggplot(arrow_data) +
  facet_wrap(~day_no, scales = "free_y") +
  geom_boxplot(aes(x = segment, y = length_y))

ggplot(arrow_data) +
  facet_wrap(~segment, scales = "free_y") +
  geom_boxplot(aes(x = day_no, y = length_y, fill = day_no)) +
  scale_fill_manual(values = (c("#f1a226", "#e0c0b0", "#c0d8d8", "#9fc8c8","#7dc5c5",
                                  "#54a1a1", "#428f8f","#307d7d","#1f6f6f")))

ggplot(arrow_data) +
  facet_wrap(~segment, scales = "free_y") +
  geom_boxplot(aes(x = day_no, y = length_x, fill = day_no)) +
  scale_fill_manual(values = (c("#f1a226", "#e0c0b0", "#c0d8d8", "#9fc8c8","#7dc5c5",
                                "#54a1a1", "#428f8f","#307d7d","#1f6f6f")))

# Save the plot
ggsave("output/plots/weighted_PCA_just_arrows.pdf", width = 8, height = 6)
