# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

ancestry_values <- data.frame(
  Individual = c("BB.K.14","BB.K.6", "BB.K.1","BB.K.15","BB.Obj.2", "BB.K.10.1", "BB.K.12", "BB.K.11"),
  Ancestry1 = c("WesternSiberian_IA", "WesternSiberian_IA", "WesternSiberian_IA",  "WesternSiberian_IA", "WesternSiberian_IA", "WesternSiberian_IA", "Kazakh_EIA",  "Cimmerian_IA" ) ,              
  Proportion1 = c(1, 0.91,  0.9, 0.9, 0.88, 0.88,  0.75, 0.85),
  Error1 = c(0.01, 0.03,  0.03,0.03, 0.05,  0.03, 0.03, 0.04),              
  Ancestry2 = c("Turkmen_BA", "Turkmen_BA","Turkmen_BA", "Turkmen_BA","Turkmen_BA",  "Bulgarian_LIA", "Bulgarian_LIA", "Turkmen_BA"),
  Proportion2 = c(0, 0.09, 0.1,    0.1, 0.12,   0.12,  0.25,   0.15), 
  Error2 = c(0.01, 0.03,  0.03,0.03,0.05,  0.03, 0.03, 0.04)
  )             


# 1. Pivot to long format
ancestry_long <- ancestry_values %>%
  pivot_longer(
    cols = c(Ancestry1, Ancestry2, Proportion1, Proportion2, Error1, Error2),
    names_to = c(".value", "Set"),
    names_pattern = "([A-Za-z]+)([12])"
  )

# 2. Manual stacking order
manual_order <- list(
  "BB.K.14" = c("WesternSiberian_IA", "Turkmen_BA"), 
  "BB.K.6" = c("WesternSiberian_IA", "Turkmen_BA"),
  "BB.K.15" = c("WesternSiberian_IA", "Turkmen_BA"),
  "BB.K.1" = c("WesternSiberian_IA","Turkmen_BA"), 
  "BB.Obj.2" = c("WesternSiberian_IA", "Turkmen_BA"),  
  "BB.K.11" = c("Cimmerian_IA", "Turkmen_BA"),
    "BB.K.12" = c("Kazakh_EIA", "Bulgarian_LIA"),
  "BB.K.10.1" = c("WesternSiberian_IA","Bulgarian_LIA")

 )

# 3. Apply manual stacking order and calculate positions
# Use a robust lookup within group_by/mutate to ensure pos is correct.
ancestry_long <- ancestry_long %>%
  group_by(Individual) %>%
  mutate(
    # Get the manual order vector for the current Individual
    current_order = list(manual_order[[as.character(first(Individual))]]),
    # Compute position index of this ancestry in the current_order (1 = bottom)
    pos = match(Ancestry, current_order[[1]])
  ) %>%
  # Arrange by the calculated position index
  arrange(pos, .by_group = TRUE) %>%
  mutate(
    bottom = cumsum(lag(Proportion, default = 0)),
    top = bottom + Proportion
  ) %>%
  ungroup()

# Preserve original individual order and assign coordinates for plotting
individual_levels <- ancestry_values$Individual
ancestry_long$Individual <- factor(ancestry_long$Individual, levels = individual_levels)

# Assign a numeric y-coordinate to each individual for plotting
y_coords <- setNames(seq_along(individual_levels) * 1.5, individual_levels)
ancestry_long$y_center <- y_coords[as.character(ancestry_long$Individual)]


# 4. Color palette for all 6 ancestries 
colors_map <- c(
  "Bulgarian_LIA"    = "#E69F00",
  "Kazakh_EIA" = "#56B4E9",
  "Cimmerian_IA"  = "#D55E00",
  "Turkmen_BA" = "#F0E442",
  "WesternSiberian_IA" = "#CC79A7"
)

# 5. Generate the Base R Plot using rect() 
# Initialize PNG device with good resolution (1200x700 pixels)
png("qpAdm_BB_Ancestry.png", width = 1200, height = 700, res = 100) 

# Set margins: Left for labels (8), Right for legend (8)
par(mar = c(5, 5.5, 4, 13) + 0.1)

# Set up the plot area
plot(
  NA, NA,
  xlim = c(0, 1),
  ylim = range(ancestry_long$y_center) + c(-0.75, 0.75),
  main = "qpAdm models for Boz Barmak individuals",
  xlab = "Ancestry proportion",
  ylab = "",
  yaxt = "n",
  bty = "n"
)

# Draw the segments (thick bars)
bar_width = 1.2 # Increased bar thickness
with(ancestry_long, {
  rect(
    xleft = bottom,
    ybottom = y_center - bar_width/2,
    xright = top,
    ytop = y_center + bar_width/2,
    col = colors_map[Ancestry],
    border = NA
  )
})

# Draw the Error Bars (using the top and Error values)
with(ancestry_long, {
  # Error bars are drawn from the 'top' position down by 'Error'
  arrows(
    x0 = top - Error,  # Start X position (bottom of error bar)
    y0 = y_center,     # Y position (center of the bar)
    x1 = top,          # End X position (top of error bar, aligned with bar end)
    y1 = y_center,
    angle = 90,        # Make the ends horizontal
    code = 1,          # Draw arrows at both ends (vertical line segment)
    length = 0.1,     # Length of the horizontal tick at the end
    lwd = 1.5,         # Thickness of the error bar
    col = "black"      # Black color for contrast
  )
})

# Add individual labels (Y-axis)
axis(
  side = 2,
  at = y_coords,
  labels = names(y_coords),
  las = 1,
  tick = FALSE,
  cex.axis = 1.2 # Larger text size
)

# Add Legend (Placed in the right margin)
legend(
  x = 0.99,
  y = max(ancestry_long$y_center) + 0.75,
  legend = names(colors_map),
  fill = colors_map,
  bty = "n",
  cex = 1.5,
  xpd = TRUE
)

# Close the device to save the file
dev.off()

