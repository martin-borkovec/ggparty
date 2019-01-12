library(ggplot2)
library(gridExtra)

ggparty <- function(party) {
  plot_data <- get_plot_data(party)
  ggplot(data = plot_data,
         mapping = aes(x = x, y = y)) +
    xlim(c(0, 1)) +
    ylim(c(-0.5, 1)) +
    theme_void()
}

geom_edge_label_continuous <- function(mapping = aes(label = breaks,
                                                     x = x_edge,
                                                     y = y_edge),
                            ...) {
  do.call("geom_label", list(mapping, ...))
}

geom_edge_label_discrete <- function(mapping = aes(label = index,
                                          x = x_edge,
                                          y = y_edge),
                            ...) {
  do.call("geom_label", list(mapping, ...))
}

geom_node_terminal_label <- function(mapping = aes(label = info),
                                 ...) {
  do.call("geom_label", list(mapping, ...))
}

geom_node_inner <- function(mapping = aes(label = splitvar),
                            ...){
do.call("geom_label", list(mapping, ...))
}

geom_edge <- function(mapping = aes(x = x, y = y, xend = x_parent, yend = y_parent),
                      ...){
  do.call("geom_segment", list(mapping, ...))
}
