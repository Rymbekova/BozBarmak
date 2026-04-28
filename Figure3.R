.libPaths(c("/lisc/scratch/admixlab/aigerim/Rlibs", .libPaths()))
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)

# --- 1. Load Sample Names ---
orig_names <- read.table("samplelist.txt", header = FALSE, col.names = "sample", stringsAsFactors = FALSE) %>% as_tibble()
new_names  <- read.table("samplelist_new.txt", header = FALSE, col.names = "sample_new", stringsAsFactors = FALSE) %>% as_tibble()
sample_map <- bind_cols(orig_names, new_names) %>%
  filter(!grepl("IGNORE", sample) & !grepl("IGNORE", sample_new))

# --- 2. Load K=5 Q file ---
data <- read.table("moderns_Kyrgyz_for_admixture.5.Q", header = FALSE, stringsAsFactors = FALSE) %>% as_tibble()
colnames(data) <- paste0("Q", seq_len(ncol(data)))
data$sample <- orig_names$sample

# --- 3. Filter and reshape ---
data_filtered <- data %>%
  filter(sample %in% sample_map$sample) %>%
  left_join(sample_map, by = "sample") %>%
  rowwise() %>%
  mutate(
    dominant_component = which.max(c_across(starts_with("Q"))),
    dominant_value     = max(c_across(starts_with("Q")))
  ) %>%
  ungroup()

# Separate BBK and non-BBK, sort non-BBK by dominant component then proportion
bbk_samples <- data_filtered %>%
  filter(grepl("BB\\.K\\.", sample_new))

non_bbk_samples <- data_filtered %>%
  filter(!grepl("BB\\.K\\.", sample_new)) %>%
  arrange(dominant_component, desc(dominant_value))

# Combine: BBK first, then non-BBK grouped by component
ordered_samples <- bind_rows(bbk_samples, non_bbk_samples)
ordered_sample_levels <- ordered_samples$sample

data_long <- ordered_samples %>%
  pivot_longer(cols = starts_with("Q"), names_to = "component", values_to = "value") %>%
  mutate(
    sample    = factor(sample, levels = ordered_sample_levels),
    component = factor(component, levels = paste0("Q", 1:5))
  )

x_labels <- setNames(ordered_samples$sample_new, ordered_samples$sample)

# --- 4. Plot ---
admixture_plot <- ggplot(data_long, aes(x = sample, y = value, fill = component)) +
  geom_bar(stat = "identity", width = 0.9) +
  scale_x_discrete(labels = x_labels) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 1, 0.25)) +
  scale_fill_brewer(palette = "Paired", name = "Ancestry", labels = paste0("K", 1:5)) +
  xlab("Sample") + ylab("Ancestry Proportion") +
  theme_bw() +
  theme(
    axis.text.x  = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9),
    axis.text.y  = element_text(size = 10),
    axis.title   = element_text(size = 12),
    legend.text  = element_text(size = 10),
    legend.title = element_text(size = 11),
    strip.background = element_blank(),
    legend.position  = "right"
  )

# --- 5. Save ---
ggsave("modern.Kyrgyz.K5.png", admixture_plot,
       width  = 16,
       height = 5,
       bg     = "white")
