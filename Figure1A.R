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
