# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

ancestry_values <- data.frame(
  Individual = c("BB.K.1","BB.Obj.2", "BB.K.6", "BB.K.10.1","BB.K.12", "BB.K.14",   "BB.K.15", "BB.K.11"),
  Ancestry1 = c("Afanasievo", "Afanasievo", "Afanasievo",  "Afanasievo", "Afanasievo", "Afanasievo", "Afanasievo",  "Cimmerian_IA" ) ,              
  Proportion1 = c(0.708, 0.734,  0.753, 0.727, 0.712, 0.738,  0.727, 0.834),
  Error1 = c(0.015, 0.015,  0.014, 0.016, 0.015,  0.015, 0.016, 0.045),              
  Ancestry2 = c("DevilsCave_N", "DevilsCave_N","DevilsCave_N", "DevilsCave_N","DevilsCave_N",  "DevilsCave_N", "DevilsCave_N", "BMAC"),
  Proportion2 = c(0.292, 0.266, 0.247,    0.273, 0.288,   0.262,  0.273,   0.166), 
  Error2 = c(0.015, 0.015, 0.014, 0.016, 0.015,  0.015, 0.016, 0.045)
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
  "BB.K.11" = c("Cimmerian_IA","BMAC"),
  "BB.K.15" = c("Afanasievo","DevilsCave_N"),
  "BB.K.14" = c("Afanasievo", "DevilsCave_N"),
  "BB.K.12" = c("Afanasievo", "DevilsCave_N"),
  "BB.K10.1" = c("Afanasievo", "DevilsCave_N"), 
  "BB.K.9" = c("Afanasievo","DevilsCave_N"), 
  "BB.K.6" = c("Afanasievo", "DevilsCave_N"),
  "BB.Obj.2" = c("Afanasievo", "DevilsCave_N"),
  "BB.K.1" = c("Afanasievo", "DevilsCave_N")
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


# --- 4. Color palette for all 6 ancestries ---
colors_map <- c(
  "Cimmerian_IA"  = "#D55E00",
  "BMAC" = "#F0E442",
  "Afanasievo" = "#CC79A7", 
  "DevilsCave_N" = "#56B4E9"
)

# --- 5. Generate the Base R Plot using rect() ---
# Initialize PNG device with good resolution (1200x700 pixels)
png("qpAdm_BB_Ancestry.png", width = 1200, height = 700, res = 100) 

# Set margins: Left for labels (8), Right for legend (8)
par(mar = c(5, 5.5, 4, 13) + 0.1)

# 3a. Set up the plot area
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

# 3b. Draw the segments (thick bars)
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

# 3c. Draw the Error Bars (using the top and Error values)
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

# 3d. Add individual labels (Y-axis)
axis(
  side = 2,
  at = y_coords,
  labels = names(y_coords),
  las = 1,
  tick = FALSE,
  cex.axis = 1.2 # Larger text size
)

# 3e. Add Legend (Placed in the right margin)
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


