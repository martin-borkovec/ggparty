library(ggplot2)
library(gridExtra)


# StatParty ---------------------------------------------------------------


StatParty <- ggproto("StatParty", Stat,
                     compute_group = function(data, scales = scales) {
                       data <- data[!duplicated(data$id), ]
                     }
)


# adjust_mapping () -------------------------------------------------------


adjust_mapping <- function(default_mapping, mapping) {
  if(!is.null(mapping)){
    mapping <- `class<-`(modifyList(default_mapping, mapping), "uneval")
  } else {
    mapping <- default_mapping
  }
}

# ggparty() ---------------------------------------------------------------


ggparty <- function(party, horizontal = FALSE) {
  plot_data <- get_plot_data(party, horizontal = horizontal)
  node_data <- select(plot_data, starts_with("data_"))
  mapping <- aes(x = x, y = y, id = id, kids = kids, info = info)

  for (column_i in names(node_data)) {
    mapping <- adjust_mapping(mapping, aes_string(var = paste0("`", column_i, "`")))
    names(mapping)[length(mapping)] <- column_i
    }

  ggplot(data = plot_data,
         mapping = mapping) +
    theme_void() +
    xlim(-0.1,1.1) +
    ylim(-0.1,1.1)



  #theme_void()
}


# geom_edge() -------------------------------------------------------------

geom_edge <- function(mapping = NULL, ...){

  default_mapping <- aes(x = x,
                         y = y,
                         xend = x_parent,
                         yend = y_parent)

  mapping <- adjust_mapping(default_mapping, mapping)

  layer(
    data = NULL,
    mapping = mapping,
    stat = StatParty,
    geom = "segment",
    position = "identity",
    inherit.aes = T,
    params = list(na.rm = T,
                  ...)
  )
}




# geom_edge_label() -------------------------------------------------------


geom_edge_label_continuous <- function(mapping = NULL, ...) {

  default_mapping <- aes(label = breaks,
                         x = x_edge,
                         y = y_edge)

  mapping <- adjust_mapping(default_mapping, mapping)

  layer(
    data = NULL,
    mapping = mapping,
    stat = StatParty,
    geom = "label",
    position = "identity",
    inherit.aes = T,
    params = list(na.rm = T,
                  ...)
  )
}

geom_edge_label_discrete <- function(mapping = NULL, ...) {

  default_mapping <- aes(label = index,
                         x = x_edge,
                         y = y_edge)

  mapping <- adjust_mapping(default_mapping, mapping)

  layer(
    data = NULL,
    mapping = mapping,
    stat = StatParty,
    geom = "label",
    position = "identity",
    inherit.aes = T,
    params = list(na.rm = T,
                  ...)
  )
}


# geom_node ------------------------------------------------


geom_node_terminal_label <- function(mapping = NULL, ...) {
  default_mapping <- aes(label = info)
  mapping <- adjust_mapping(default_mapping, mapping)

  layer(
    data = NULL,
    mapping = mapping,
    stat = StatParty,
    geom = "label",
    position = "identity",
    inherit.aes = T,
    params = list(na.rm = T,
                  ...)
  )
}

geom_node_inner <- function(mapping = NULL, ...){
  default_mapping <- aes(label = splitvar)
  mapping <- adjust_mapping(default_mapping, mapping)
  layer(
    data = NULL,
    mapping = mapping,
    stat = StatParty,
    geom = "label",
    position = "identity",
    inherit.aes = T,
    params = list(na.rm = T,
                  ...)
  )
}
