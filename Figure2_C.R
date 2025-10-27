library(ggplot2)
library(ggrepel)
library(data.table)
library(smartsnp)

# Step 1: Read the individual lists from your text files
moderns_pca <- readLines("Kyrgyz/moderns.txt")
ancients_project <- readLines("Kyrgyz/ancients.txt")

# Step 2: Read the .ind file and identify the groups for PCA and projection
GR <- read.table("general_mod_anc_Kyrgyz.ind", header = FALSE, stringsAsFactors = FALSE)
colnames(GR) <- c("Name", "Gender", "Group")

# Create a logical vector to identify individuals for projection
# Match the individuals' names from the .ind file to your 'ancients_project' list
is_projected <- GR$Name %in% ancients_project

# Step 3: Run smart_pca with the specified subsets
sm.pca <- smart_pca(
  snp_data = "mod.anc.Kyrgyz.geno",
  sample_group = GR$Group,           # Use the original groups for coloring/labels
  sample_project = which(is_projected), # Tell smart_pca which individuals to project
  missing_value = 9,
  missing_impute = "mean",
  scaling = "drift",
  program_svd = "RSpectra",
  pc_axes = 2
)

# Step 4: Prepare data for plotting
smartpca.evec <- data.table(sm.pca$pca.sample_coordinates)
smartpca.evec[, Name := GR$Name]
smartpca.evec[, Group := GR$Group] # Assign correct groups

get_var_exp <- function(df) {
  pc_cols <- grep("^PC\\d+$", names(df), value = TRUE)
  vars <- sapply(pc_cols, function(nm) var(df[[nm]], na.rm = TRUE))
  100 * vars / sum(vars, na.rm = TRUE)
}
var_exp <- get_var_exp(smartpca.evec)
x_pct <- round(var_exp[1], 1)
y_pct <- round(var_exp[2], 1)

# Step 5: Separate data for plotting (PCA vs. Projected)
dat_pca <- smartpca.evec[Class == "PCA"]
dat_proj <- smartpca.evec[Class == "Projected"]

# Step 6: Central 95% trimming on the PCA data only 
if (nrow(dat_pca) > 0) {
  xq <- quantile(dat_pca$PC1, c(0.025, 0.9), na.rm = TRUE)
  yq <- quantile(dat_pca$PC2, c(0.025, 0.9), na.rm = TRUE)
  dat_pca <- dat_pca[PC1 >= xq[1] & PC1 <= xq[2] & PC2 >= yq[1] & PC2 <= yq[2]]
}
# Step 7: Label tables from trimmed data 
tm <- function(x) mean(x, trim = 0.1, na.rm = TRUE)
lab_modern <- dat_pca[, .(PC1 = tm(PC1), PC2 = tm(PC2)), by = Group]
lab_proj <- dat_proj[, .(PC1 = tm(PC1), PC2 = tm(PC2)), by = Group]

# Step 8: Plotting
p <- ggplot() +
  # Plot PCA individuals (moderns)
  geom_point(data = dat_pca,
             aes(PC1, PC2, color = Group, shape = Group),
             alpha = 0.6, size = 3.5) +
  # Plot projected individuals (ancients)
  geom_point(data = dat_proj,
             aes(PC1, PC2, color = Group, shape = Group),
             size = 3.5, stroke = 0.9) +
  scale_shape_manual(values = rep(21:25, 100), name = "Population") +
  scale_color_discrete(name = "Population") +
  geom_label_repel(data = lab_modern,
                   aes(PC1, PC2, label = Group, color = Group),
                   alpha = 0.7, size = 3.5, segment.color = NA,
                   max.overlaps = 200, show.legend = FALSE) +
  geom_label_repel(data = lab_proj,
                   aes(PC1, PC2, label = Group, color = Group),
                   alpha = 0.9, fontface = "bold", size = 3.5,
                   segment.color = NA, max.overlaps = 200, show.legend = FALSE) +
  labs(
    x = paste0("PC1 (", x_pct, "% variance explained)"),
    y = paste0("PC2 (", y_pct, "% variance explained)")
  ) +
  theme_bw() +
  theme(legend.position = "right")

p

#==============================================================================================

MODERN_GROUPS_TO_LABEL <- c("Central_Eurasian_populations", "Caucasus_and_Iran_populations", "South_Asian_populations", "Siberian_populations", "European_populations", "East_Asian_populations")

# Step 9: Plotting 
p <- ggplot() +
  # Plot PCA individuals (moderns) - Uniform gray points
  geom_point(data = dat_pca,
             aes(PC1, PC2),
             color = "gray60",
             shape = 16,
             alpha = 0.6, size = 3.8) +
  
  # Plot projected individuals (ancients) - Colored/Shaped points
  geom_point(data = dat_proj,
             aes(PC1, PC2, color = Group, shape = Group),
             size = 3.8, stroke = 0.9) +
  
  # Set up scales
  scale_shape_manual(values = rep(21:25, 100), name = "Population") +
  scale_color_discrete(name = "Population") +
  
  # Label Projected (Ancients) - UNCHANGED (Bold/Colored)
  geom_label_repel(data = lab_proj,
                   aes(PC1, PC2, label = Group, color = Group),
                   alpha = 0.9, fontface = "bold", size = 5.5,
                   segment.color = NA, max.overlaps = 200, show.legend = FALSE) +
  
  # Label Moderns (PCA) - MODIFIED: Only labels groups in MODERN_GROUPS_TO_LABEL
  geom_label_repel(data = lab_modern[Group %in% MODERN_GROUPS_TO_LABEL], # <-- FILTER APPLIED HERE
                   aes(PC1, PC2, label = Group), 
                   color = "black", # Neutral color for all filtered modern labels
                   alpha = 0.7, size = 5.5, segment.color = NA,
                   max.overlaps = 200, show.legend = FALSE) +
  
  labs(
    x = paste0("PC1 (", x_pct, "% variance explained)"),
    y = paste0("PC2 (", y_pct, "% variance explained)")
  ) +
  theme_bw() +
  theme(legend.position = "right",
        legend.title = element_text(size = 18), 
        legend.text = element_text(size = 16),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18) )

p
  
ggsave("updated_mod.anc.Kyrgyz.pca.png", plot = p, width = 20, height = 12, dpi = 300)
