library(assertthat)
library(dplyr)
library(purrr)
library(igraph)
library(ggplot2)
library(ggraph)
library(ggmap)

#Adding random countries with ID, lon, lat and name
country_coords_txt <- "
 1     3.00000  28.00000       Algeria
 2    54.00000  24.00000           UAE
 3   139.75309  35.68536         Japan
 4    45.00000  25.00000 'Saudi Arabia'
 5     9.00000  34.00000       Tunisia
 6     5.75000  52.50000   Netherlands
 7   103.80000   1.36667     Singapore
 8   124.10000  -8.36667         Korea
 9    -2.69531  54.75844            UK
10    34.91155  39.05901        Turkey
11  -113.64258  60.10867        Canada
12    77.00000  20.00000         India
13    25.00000  46.00000       Romania
14   135.00000 -25.00000     Australia
15    10.00000  62.00000        Norway"
# nodes come from the above table and contain geo-coordinates for some
# randomly picked countries
nodes <- read.delim(text = country_coords_txt, header = FALSE,
                    quote = "'", sep = "",
                    col.names = c('id', 'lon', 'lat', 'name'))



set.seed(123)  # set random generator state for the same output

N_EDGES_PER_NODE_MIN <- 1
N_EDGES_PER_NODE_MAX <- 4
N_CATEGORIES <- 4

# edges: create random connections between countries (nodes)
edges <- map_dfr(nodes$id, function(id) {
  n <- floor(runif(1, N_EDGES_PER_NODE_MIN, N_EDGES_PER_NODE_MAX+1))
  to <- sample(1:max(nodes$id), n, replace = FALSE)
  to <- to[to != id]
  categories <- sample(1:N_CATEGORIES, length(to), replace = TRUE)
  weights <- runif(length(to))
  data_frame(from = id, to = to, weight = weights, category = categories)
})

edges <- edges %>% mutate(category = as.factor(category))


g <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)


edges_for_plot <- edges %>%
  inner_join(nodes %>% select(id, lon, lat), by = c('from' = 'id')) %>%
  rename(x = lon, y = lat) %>%
  inner_join(nodes %>% select(id, lon, lat), by = c('to' = 'id')) %>%
  rename(xend = lon, yend = lat)

assert_that(nrow(edges_for_plot) == nrow(edges))

nodes$weight = degree(g)


maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#596673")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))


country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "#CECECE", color = "#515151",
                               size = 0.15)
mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))


ggplot(nodes) + country_shapes +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                 color = category, size = weight),
             data = edges_for_plot, curvature = 0.33,
             alpha = 0.5) +
  scale_size_continuous(guide = FALSE, range = c(0.25, 2)) + # scale for edge widths
  geom_point(aes(x = lon, y = lat, size = weight),           # draw nodes
             shape = 21, fill = 'white',
             color = 'black', stroke = 0.5) +
  scale_size_continuous(guide = FALSE, range = c(1, 6)) +    # scale for node size
  geom_text(aes(x = lon, y = lat, label = name),             # draw text labels
            hjust = 0, nudge_x = 1, nudge_y = 4,
            size = 3, color = "white", fontface = "bold") +
  mapcoords + maptheme


#static node size
ggplot(nodes) + country_shapes +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                 color = category, size = weight),
             data = edges_for_plot, curvature = 0.33,
             alpha = 0.5) +
  scale_size_continuous(guide = FALSE, range = c(0.25, 2)) + # scale for edge widths
  geom_point(aes(x = lon, y = lat),                          # draw nodes
             shape = 21, size = 3, fill = 'white',
             color = 'black', stroke = 0.5) +
  geom_text(aes(x = lon, y = lat, label = name),             # draw text labels
            hjust = 0, nudge_x = 1, nudge_y = 4,
            size = 3, color = "white", fontface = "bold") +
  mapcoords + maptheme


#ggplot + ggraph

node_pos <- nodes %>%
  select(lon, lat) %>%
  rename(x = lon, y = lat)   # node positions must be called x, y
lay <- create_layout(g, 'manual',
                     node.positions = node_pos)
assert_that(nrow(lay) == nrow(nodes))

# add node degree for scaling the node sizes
lay$weight <- degree(g)
# We pass the layout lay and use ggraph’s geoms geom_edge_arc and geom_node_point for plotting:
  
ggraph(lay) + country_shapes +
  geom_edge_arc(aes(color = category, edge_width = weight,   # draw edges as arcs
                    circular = FALSE),
                data = edges_for_plot, curvature = 0.33,
                alpha = 0.5) +
  scale_edge_width_continuous(range = c(0.5, 2),             # scale for edge widths
                              guide = FALSE) +
  geom_node_point(aes(size = weight), shape = 21,            # draw nodes
                  fill = "white", color = "black",
                  stroke = 0.5) +
  scale_size_continuous(range = c(1, 6), guide = FALSE) +    # scale for node sizes
  geom_node_text(aes(label = name), repel = TRUE, size = 3,
                 color = "white", fontface = "bold") +
  mapcoords + maptheme+
  theme(legend.position = "top")
  coord_fixed()

?ggsave()
ggsave(filename = "world_network_plot.pdf", dpi = 300, width = 80, height = 30, units = c("cm"), limitsize = FALSE)
