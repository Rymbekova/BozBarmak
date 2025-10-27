cov <- as.matrix(read.table("/home/rymbekovaa95/pcangsd/56_50_Bot_final_for_PCA.cov"))

e<-eigen(cov)
ID<-read.table("/home/rymbekovaa95/pcangsd/sgdp_CA.info",head=T)

pdf("/home/rymbekovaa95/pcangsd/PCA_Eurasian_SGDP_Kyrgyz_Botai.pdf", width = 21, height = 21)
par(mar=c(6, 7, 4, 2) + 0.1)

e2 <- e$vectors[,1:2]
e2

# Calculate the percentage of variance explained
var <- (e$values/sum(e$values))*100

# Generate random shapes for individuals, larger than 25
set.seed(23)  # Optional: for reproducibility
num_individuals <- length(unique(ID$IND))
available_shapes <- 1:25  # The range of available shapes in R
random_pch <- sample(available_shapes, num_individuals, replace=TRUE)  # Sample with replacement for larger numbers

# Plot with randomized shapes for individuals
plot(e2, col=as.factor(ID$POP), pch=random_pch[as.numeric(as.factor(ID$IND))], cex=6, lwd = 4,
     xlab=paste0("PC1 (", round(var[1], 2), "% variance explained)"),
     ylab=paste0("PC2 (", round(var[2], 2), "% variance explained)"),
     main="",
     cex.lab=2.5,
     cex.axis=2)


# Get the unique individual IDs
unique_ind <- levels(as.factor(ID$IND))

# Get the corresponding colors for each individual (based on their POPulation)
# You need a separate data structure linking IND to POP if ID is not sorted
# Assuming a data frame 'ID' where the first column is IND and the second is POP
ind_pop_map <- ID[match(unique_ind, ID$IND), "POP"]
unique_col <- as.numeric(as.factor(ind_pop_map))

# Get the corresponding random shapes for each individual
unique_pch <- random_pch[as.numeric(as.factor(unique_ind))]

# Plot the legend using the individual data
legend("topright",
       legend = unique_ind,       # Labels are Individual IDs
       col = unique_col,         # Colors are based on Population
       pch = unique_pch,         # Shapes are the randomized shapes
       title = "Population",
       cex = 3,
       pt.cex = 3,
       pt.lwd = 3)

dev.off()
#-----------------------------------------------------------------------------------------------

#IBD

ibd_data <- data.frame(
  Pair = c("BB.K.12 & BB.K.15", "BB.K.10.1 & BB.K.12", "BB.K.6 & BB.K.10.1",
           "BB.K.10.1 & BB.K.15", "BB.K.6 & BB.K.12", "BB.K.14 & BB.K.15",
           "BB.K.10.1 & BB.K.11", "BB.K.6 & BB.K.15", "BB.K.12 & BB.K.14",
           "BB.K.6 & BB.K.11", "BB.K.10.1 & BB.K.14", "BB.K.6 & BB.K.14",
           "BB.K.11 & BB.K.14", "BB.K1 & BB.Obj.2", "BB.K.9 & BB.K.15",
           "BB.K1 & BB.K.6", "BB.K.6 & BB.K.9", "BB.K.11 & BB.K.12",
           "BB.Obj.2 & BB.K.6", "BB.K1 & BB.K.15", "BB.K1 & BB.K.12",
           "BB.K.9 & BB.K.10.1"),
  IBD_cM = c(1200, 1050, 980, 750, 680, 600, 520, 480, 400, 350, 300, 250,
             180, 120, 100, 80, 70, 60, 50, 40, 30, 20)
)
# Order the data for plotting (from smallest to largest IBD for horizontal bars)
ibd_data <- ibd_data[order(ibd_data$IBD_cM), ]

# Define the relationship thresholds
thresholds <- list(
  first_degree = c(2000, 3000), 
  second_degree = c(1200, 2000),
  third_degree = c(500, 1200),
  no_degree = c(0, 500)
)

# Define colors for the threshold ranges
range_colors <- c("First Degree Range" = "#E0FFE0", # Light Green
                  "Second Degree Range" = "#FFE0E0", # Light Red/Pink
                  "Third Degree Range" = "#FFFACD")  # Light Yellow

# --- PART 3: Create Multi-Panel PDF ---
pdf("/home/rymbekovaa95/pcangsd/PCA_IBD_Eurasian_SGDP_Kyrgyz_Botai.pdf", width = 30, height = 20) # Increased width for two plots

# Set up the layout: 1 row, 2 columns.
# widths = c(proportion_for_pca, proportion_for_ibd_plot)
# PCA will take about 60% of width, IBD plot about 40%. Adjust as needed.
layout(matrix(c(1, 2), nrow = 1, ncol = 2, byrow = TRUE), widths = c(0.6, 0.4))
base_cex <- 2.0

# --- Plot 1: PCA Plot ---
# Reset margins for the first plot (PCA) as they were changed by par() earlier
# c(bottom, left, top, right)
par(mar=c(6, 7, 4, 2)) # Adjusted for PCA specific needs

plot(e2, col=as.factor(ID$POP), pch=random_pch[as.numeric(as.factor(ID$IND))], cex=6, lwd = 4,
     xlab=paste0("PC1 (", round(var[1], 2), "% variance)"),
     ylab=paste0("PC2 (", round(var[2], 2), "% variance)"),
     main="",
     cex.lab=base_cex,
     cex.axis=base_cex
)

legend("topright", legend=levels(as.factor(ID$POP)), col=1:length(levels(as.factor(ID$POP))),
       pch=19, title.cex = base_cex * 1.3, cex = base_cex * 1.3, pt.cex=base_cex * 1.3, pt.lwd = 3, title="Population")


# --- Plot 2: IBD Bar Plot ---
par(mar=c(6, 18, 4, 2)) # Increased left margin for labels significantly

# Determine y-coordinates for bars
y_coords <- barplot(ibd_data$IBD_cM, horiz = TRUE, plot = FALSE) # Get bar positions without plotting

max_x_val <- max(ibd_data$IBD_cM, thresholds$first_degree[2]) + 100

plot(NULL, xlim = c(0, max_x_val),
     ylim = c(0, max(y_coords) + 0.7),
     ylab = "", xlab = "", yaxt = "n", xaxt = "n", bty = "n")

# Add shaded background rectangles for relationship ranges
# Use abline(v=...) for vertical lines if needed for exact boundaries on top of rects
rect(xleft = thresholds$third_degree[1], ybottom = 0,
     xright = thresholds$third_degree[2], ytop = max(y_coords) + 0.5,
     col = range_colors["Third Degree Range"], border = NA)
rect(xleft = thresholds$second_degree[1], ybottom = 0,
     xright = thresholds$second_degree[2], ytop = max(y_coords) + 0.5,
     col = range_colors["Second Degree Range"], border = NA)
rect(xleft = thresholds$first_degree[1], ybottom = 0,
     xright = thresholds$first_degree[2], ytop = max(y_coords) + 0.5,
     col = range_colors["First Degree Range"], border = NA)

# Add the bars
barplot(ibd_data$IBD_cM, names.arg = ibd_data$Pair, horiz = TRUE,
        col = "skyblue", cex.names = base_cex * 1.1, las = 1, # las=1 for horizontal labels
        border = NA, add = TRUE, axes = FALSE) # Add to existing plot, no new axes

# Add custom X-axis at the top
axis(side = 3, at = seq(0, 3000, by = 500), # Adjust max and interval as needed
     labels = seq(0, 3000, by = 500),
     cex.axis = base_cex, tcl = -0.5, pos = max(y_coords) + 0.7) # pos places axis at top of plot


dev.off()

