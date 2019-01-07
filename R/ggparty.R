library(ggplot2)
library(gridExtra)

ggparty <- function(party) {
  plot_data <- get_plot_data(party)

  # if (is(party, "modelparty")) {
  #
  #   tree <-
  #     ggplot(data = plot_data, mapping = aes(x = x, y = y))
  #
  #
  #   plot_list <- list(tree)
  #   terminal <- plot_data[!is.na(plot_data$terminal), ]
  #
  #   for (i in 1:nrow(terminal)) {
  #     plot_list <- c(plot_list, get_terminal_plot(party[[1]], party[[terminal$id[i]]]))
  #   }
  #
  #   grid.arrange(
  #   grobs = plot_list,
  #   widths = c(1, 1, 1),
  #   layout_matrix = rbind(c(1,1,1),
  #                         c(2, 3, 4))
  #   )
  # } else {
  ggplot(data = plot_data,
         mapping = aes(x = x, y = y)) +
    xlim(c(0, 1)) +
    ylim(c(-0.5, 1)) +
    theme_void()
  # }
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
