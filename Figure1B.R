# IBD Network Plot
# Individuals: BB.K.1, BB.K.6, BB.K.9, BB.K.10.1, BB.K.11, BB.K.12, BB.K.14, BB.K.15, BB.Obj.2

library(igraph)
library(ggraph)
library(tidygraph)
library(ggplot2)

# ── Edge list with IBD values ──────────────────────────────────────────────────
edges <- data.frame(
  from = c("BB.K.12",  "BB.K.10.1", "BB.K.10.1", "BB.K.6",   "BB.K.6",
           "BB.K.14",  "BB.K.10.1", "BB.K.6",    "BB.K.12",  "BB.K.6",
           "BB.K.10.1","BB.K.6",    "BB.K.11"),
  to   = c("BB.K.15",  "BB.K.15",   "BB.K.12",   "BB.K.10.1","BB.K.12",
           "BB.K.15",  "BB.K.11",   "BB.K.15",   "BB.K.14",  "BB.K.11",
           "BB.K.14",  "BB.K.14",   "BB.K.14"),
  ibd  = c(1800, 1100, 1050, 700, 600,
           500,  400,  400,  300, 200,
           200,  100,   50)
)

# ── Build graph ────────────────────────────────────────────────────────────────
# Only include individuals that appear in at least one edge
all_nodes <- unique(c(edges$from, edges$to))

g <- graph_from_data_frame(edges, directed = FALSE, vertices = all_nodes)
E(g)$weight <- edges$ibd

tg <- as_tbl_graph(g) %>%
  activate(edges) %>%
  mutate(
    ibd        = weight,
    ibd_scaled = (ibd - min(ibd)) / (max(ibd) - min(ibd))
  )

set.seed(42)
lay <- create_layout(tg, layout = "fr", weights = ibd)
k11_idx <- which(lay$name == "BB.K.11")
# Get the bounding box of the current layout
x_range <- range(lay$x)
y_range <- range(lay$y)

lay$x[k11_idx] <- x_range[2] + diff(x_range) * 0.75   # push very far right
lay$y[k11_idx] <- y_range[1] - diff(y_range) * 0.25   # and slightly below center

# ── Colour palette ─────────────────────────────────────────────────────────────
node_col  <- "#87ceec"                    
edge_pal  <- c("#A8C8E8", "#1A3A6B")       # light steel → deep navy

# ── Plot ───────────────────────────────────────────────────────────────────────
p <- ggraph(lay) +
  
  geom_edge_link(
    aes(width = ibd_scaled, colour = ibd, alpha = ibd_scaled),
    lineend = "round"
  ) +
  scale_edge_width(range = c(0.4, 4), guide = "none") +
  scale_edge_alpha(range = c(0.25, 0.95), guide = "none") +
  scale_edge_colour_gradient(
    low  = edge_pal[1], high = edge_pal[2],
    name = "Total IBD (cM)",
    breaks = c(50, 500, 1000, 1500, 1800),
    guide = guide_edge_colourbar(title.position = "top", barwidth = 8)
  ) +
  
  geom_node_point(size = 10, colour = node_col, alpha = 0.9) +
  geom_node_point(size = 10, colour = "white",  alpha = 0.15) +

  geom_node_text(aes(label = name), size = 3.5, fontface = "bold",
                 colour = "#000000", vjust = 2.2) +
  
  theme_void(base_family = "sans") +
  theme(
    plot.background  = element_rect(fill = "#F7FAFD", colour = NA),
    panel.background = element_rect(fill = "#F7FAFD", colour = NA),
    plot.title       = element_text(size = 14, face = "bold",   colour = "#1A3A5C",
                                    margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 9,  colour = "#4A6080",
                                    margin = margin(b = 10)),
    plot.caption     = element_text(size = 7,  colour = "#8AAAC0",
                                    margin = margin(t = 8)),
    plot.margin      = margin(20, 20, 20, 20),
    legend.position  = "bottom",
    legend.title     = element_text(size = 9,  colour = "#1A3A5C", face = "bold"),
    legend.text      = element_text(size = 8,  colour = "#4A6080")
  )

p

ggsave("ibd_network.png", plot = p, width = 8, height = 7, dpi = 300,
       bg = "#F7FAFD")
