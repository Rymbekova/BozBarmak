# ------------------------------------------------------------------
# PART 0: Setup and Data Loading
# USE PRE_SAVED DATA RDS
# ------------------------------------------------------------------

# Install and Load necessary packages (run only if not installed)
# install.packages("devtools")
# devtools::install_github("bodkan/admixr", force = TRUE)
# install.packages(c("tidyverse", "ggplot2"))
library(admixr)
library(tidyverse) # Includes dplyr (for pipes) and ggplot2 (for plotting)

# Set PATH to AdmixTools executables
Sys.setenv(PATH = paste("/home/rymbekovaa95/Eurasian/AdmixTools/src", Sys.getenv("PATH"), sep = ":"))

# Paths to your genotype files:
# 1. Population-level data (BozBarmak as one population)
data_pop <- eigenstrat(prefix = "/home/rymbekovaa95/mod.anc.Kyrgyz.Mbuti")
# 2. Individual-level data (each BozBarmak individual as a separate 'pop')
data_indiv <- eigenstrat(prefix = "/home/rymbekovaa95/mod.anc.BB.Mbuti")

# Read populations from the INDIV .ind file for individual IDs
ind_data_indiv <- read.table("/home/rymbekovaa95/mod.anc.BB.Mbuti.ind",
                             header = FALSE, stringsAsFactors = FALSE)
colnames(ind_data_indiv) <- c("Individual_ID", "Sex", "Population")

# Identify test populations 
ind_data_pop <- read.table("/home/rymbekovaa95/mod.anc.Kyrgyz.Mbuti.ind",
                           header = FALSE, stringsAsFactors = FALSE)
all_pops_pop <- unique(ind_data_pop[, 3])
pops_to_test <- setdiff(all_pops_pop, c("BozBarmak", "Mbuti"))


# ------------------------------------------------------------------
# PART 1: Population-Level f3(BozBarmak; B, Mbuti)
# ------------------------------------------------------------------

message("Calculating Population-level f3...")

# Loop for Population-level f3
results_pop_list <- list()
for (pop in pops_to_test) {
  res <- f3(data = data_pop, A = "BozBarmak", B = pop, C = "Mbuti")
  results_pop_list[[pop]] <- res
}

library(tibble)

# Combine and format Population results
f3_pop_table <- do.call(rbind, results_pop_list) %>%
  as.data.frame() %>%
  rownames_to_column("Population_B") %>%
  mutate(Type = "Population Mean",
         Individual_ID = "BozBarmak") # Add a dummy ID for plotting

# Rename columns for clarity (admixr output uses V1, V2, V3, V4)
names(f3_pop_table)[3:6] <- c("f3_stat", "se", "Z", "p_value")

print("Population F3 Results:")
print(f3_pop_table)


# ------------------------------------------------------------------
# PART 2: Individual-Level f3(Individual; B, Mbuti) 
# ------------------------------------------------------------------

message("Calculating Individual-level f3...")

# Identify individual IDs (now population labels in the INDIV file)
# Assuming BozBarmak individuals have IDs starting with '28585'
bozbarmak_indiv_pops <- ind_data_indiv$Population[grep("28585", ind_data_indiv$Population)]

# Loop for Individual-level f3
results_indiv_list <- list()
total_tests <- length(bozbarmak_indiv_pops) * length(pops_to_test)
test_counter <- 0
for (pop_A in bozbarmak_indiv_pops) { 
  for (pop_B in pops_to_test) {
    
    test_counter <- test_counter + 1
    message(paste0("Running test ", test_counter, "/", total_tests, 
                   ": f3(", pop_A, "; ", pop_B, ", Mbuti)"))
    res <- f3(data = data_indiv, A = pop_A, B = pop_B, C = "Mbuti")
    test_name <- paste(pop_A, pop_B, sep = "_")
    results_indiv_list[[test_name]] <- res
  }
}


# Combine results into a data frame
f3_indiv_table <- do.call(rbind, lapply(names(results_indiv_list), function(nm) {
  df <- as.data.frame(results_indiv_list[[nm]])
  df$Test <- nm
  df
})) %>%
  tidyr::separate(
    col = Test,
    into = c("Individual_ID", "Population_B"),
    sep = "_",
    extra = "merge"
  ) %>%
  mutate(Type = "Individual")

# Rename columns
names(f3_indiv_table)[1:10] <- c("Individual_ID", "Population_B", "Outgroup", "f3_stat", "stderr", "Z", "nsnps", "ID", "B", "Type")
head(f3_indiv_table)

names(f3_pop_table)[1:6] <- c("Number", "Population_A", "Population_B", "Outgroup", "f3_stat", "stderr")
head(f3_pop_table)

# ------------------------------------------------------------------
# PART 3: Combine Data and Plotting
# ------------------------------------------------------------------

# Combine both tables for plotting
# Select and rename columns consistently before binding
f3_pop_plot <- f3_pop_table %>% select(Population_A, Population_B, f3_stat, stderr)
f3_indiv_plot <- f3_indiv_table %>% select(Individual_ID, Population_B, f3_stat, stderr)

combined_f3_data <- bind_rows(f3_pop_plot, f3_indiv_plot)

# Save the data for efficiency
saveRDS(combined_f3_data, file = "combined_f3_data.rds")
write.csv(combined_f3_data, "combined_f3_data.csv", row.names = FALSE)

#----------------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Separate population vs individual
f3_pop_plot <- combined_f3_data %>%
  filter(!is.na(Population_A)) %>%
  select(Population_B, f3_stat, stderr) %>%
  mutate(Type = "Population")

f3_indiv_plot <- combined_f3_data %>%
  filter(!is.na(Individual_ID)) %>%
  select(Population_B, f3_stat, Individual_ID) %>%
  mutate(Type = "Individual")

# Order Population_B by descending population f3_stat
pop_order <- f3_pop_plot %>%
  arrange(desc(f3_stat)) %>%
  pull(Population_B)

f3_pop_plot$Population_B <- factor(f3_pop_plot$Population_B, levels = pop_order)
f3_indiv_plot$Population_B <- factor(f3_indiv_plot$Population_B, levels = pop_order)

# Plot: vertical lines (mean ± SE) + mean points + individual points
ggplot() +
  # Population-level vertical line
  geom_linerange(data = f3_pop_plot,
                 aes(x = Population_B,
                     ymin = f3_stat - stderr,
                     ymax = f3_stat + stderr),
                 color = "blue", size = 1) +
  # Mean point
  geom_point(data = f3_pop_plot,
             aes(x = Population_B, y = f3_stat),
             color = "blue", size = 3) +
  # Individual points
  geom_point(data = f3_indiv_plot,
             aes(x = Population_B, y = f3_stat),
             color = "red", size = 2, alpha = 0.7,
             position = position_jitter(width = 0.1)) +
  theme_bw() +
  labs(x = "Population B",
       y = "f3 statistic",
       title = "BozBarmak f3: Population vs Individual") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#---------------------------------------------------------------------

f3_pop_plot_dot <- f3_pop_plot %>%
  filter(grepl("\\.", Population_B))

f3_indiv_plot_dot <- f3_indiv_plot %>%
  filter(grepl("\\.", Population_B))

# Order Pop_B by descending f3_stat
pop_order <- f3_pop_plot_dot %>%
  arrange(desc(f3_stat)) %>%
  pull(Population_B)

f3_pop_plot_dot$Population_B <- factor(f3_pop_plot_dot$Population_B, levels = pop_order)
f3_indiv_plot_dot$Population_B <- factor(f3_indiv_plot_dot$Population_B, levels = pop_order)

# Plot
p <- ggplot() +
  geom_linerange(data = f3_pop_plot_dot,
                 aes(y = reorder(Population_B, f3_stat),
                     xmin = f3_stat - stderr,
                     xmax = f3_stat + stderr),
                 color = "blue", size = 1) +
  geom_point(data = f3_pop_plot_dot,
             aes(y = reorder(Population_B, f3_stat), x = f3_stat),
             color = "blue", size = 3.5) +
  geom_point(data = f3_indiv_plot_dot,
             aes(y = reorder(Population_B, f3_stat), x = f3_stat),
             color = "red", size = 2, alpha = 0.7,
             position = position_jitter(height = 0.1)) +
  theme_bw() +
  labs(y = "Ancient Eurasian population",
       x = "f3 statistics score",
       title = "f3(BozBarmak; Ancient Eurasian Population; Mbuti)") +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) + 
  theme(plot.margin = margin(t = 20, r = 10, b = 18, l = 10))

p

ggsave(filename = "BozBarmak_f3_plot.png", plot = p, width = 7, height = 7, dpi = 300)
