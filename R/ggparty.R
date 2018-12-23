library(ggplot2)

ggparty <- function(party) {
  ggplot(data = get_plot_data(party),
         mapping = aes(x = x, y = y))
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

geom_node_terminal <- function(mapping = aes(label = terminal),
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
