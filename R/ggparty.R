#' ggplot an object of the party class
#'
#' @param party partyobject to plot
#' @param horizontal horizontal plot?
#' @param terminal_space proportion of the plot that should be reserved for
#' the terminal nodeplots
#' @seealso [geom_edge()], [geom_edge_label()], [geom_node_splitvar()],
#'  [geom_node_info()], [geom_nodeplot()]
#' @export
#' @import partykit
#' @import ggplot2
#' @import gtable
#' @import grid


# ggparty() ---------------------------------------------------------------


ggparty <- function(party, horizontal = FALSE, terminal_space = 0.2) {
  #browser()
  plot_data <- get_plot_data(party, horizontal = horizontal, terminal_space = terminal_space)
  node_data <- dplyr::select(plot_data, dplyr::starts_with("data_"))
  mapping <- aes(x = x, y = y, x_parent = x_parent,
                 y_parent = y_parent, id = id, kids = kids, info = info)

  for (column_i in names(node_data)) {
    mapping <- adjust_mapping(mapping, aes_string(var = paste0("`", column_i, "`")))
    names(mapping)[length(mapping)] <- column_i
  }

  ggplot(data = plot_data,
         mapping = mapping) +
    theme_void() +
    xlim(0, 1) +
    ylim(0, 1)
}


# geom_edge() -------------------------------------------------------------
#' Draw edges between children and parents. wrapper of geom_segment
#'
#' @param mapping not recommended to change
#' @param ... additional arguments for [geom_segment()]
#' @param x_nudge,y_nudge nudge label
#' @param ids choose which edges to draw by their children's ids
#' @export
#' @md
#'
geom_edge <- function(mapping = NULL, x_nudge = 0, y_nudge = 0, ids = NULL, ...){

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
    position = position_nudge(x = x_nudge, y = y_nudge),
    inherit.aes = T,
    params = list(ids = ids,
                  na.rm = TRUE,
                  ...)
  )
}




# geom_edge_label() -------------------------------------------------------

#' Label edge with corresponding splitbreak
#'
#' @param mapping not recommended to change
#' @param shift value in (0,1). Move label along corresponding edge.
#' @param ids choose which splitbreaks to label by their children's ids
#' @param x_nudge,y_nudge nudge label
#' @param ... additional arguments for [geom_label()]
#'
#' @export
#' @md
#'
geom_edge_label <- function(mapping = NULL,
                            x_nudge = 0,
                            y_nudge = 0,
                            ids = NULL,
                            shift = 0.5,
                            label.size = 0, ...) {

  default_mapping <- aes(label = index)

  mapping <- adjust_mapping(default_mapping, mapping)

  layer(
    data = NULL,
    mapping = mapping,
    stat = StatParty,
    geom = "label",
    position = position_nudge(x = x_nudge, y = y_nudge),
    inherit.aes = T,
    params = list(ids = ids,
                  shift = shift,
                  label.size = label.size,
                  na.rm = TRUE,
                  ...)
  )
}


# geom_node ------------------------------------------------

#' Draw labels containing node's info
#'
#' @param mapping not recommended to change
#' @param ids choose which nodes to label by their ids
#' @param x_nudge,y_nudge nudge label
#' @param label.padding Amount of padding around label. Defaults to 0.5 lines.
#' @param ... additional arguments for [geom_label()]
#' @export
#' @md
#'
geom_node_info <- function(mapping = NULL, x_nudge = 0, y_nudge = 0, ids = NULL,
                           label.padding = unit(0.5, "lines"), ...) {
  default_mapping <- aes(label = info)
  mapping <- adjust_mapping(default_mapping, mapping)
  layer(
    data = NULL,
    mapping = mapping,
    stat = StatParty,
    geom = "label",
    position = position_nudge(x = x_nudge, y = y_nudge),
    inherit.aes = T,
    params = list(ids = ids,
                  label.padding = label.padding,
                  na.rm = TRUE,
                  ...)
  )
}

#' Draw labels containing node's split variable
#'
#' @param mapping not recommended to change
#' @param ids choose which terminal nodes to label by their ids
#' @param x_nudge,y_nudge nudge label
#' @param label.padding Amount of padding around label. Defaults to 0.5 lines.
#' @param ... additional arguments for [geom_label()]
#' @export
#' @md
#'
geom_node_splitvar <- function(mapping = NULL, x_nudge = 0, y_nudge = 0,
                               label.padding = unit(0.5, "lines"), ids = NULL, ...) {
  default_mapping <- aes(label = splitvar)
  mapping <- adjust_mapping(default_mapping, mapping)
  layer(
    data = NULL,
    mapping = mapping,
    stat = StatParty,
    geom = "label",
    position = position_nudge(x = x_nudge, y = y_nudge),
    inherit.aes = T,
    params = list(ids = ids,
                  label.padding = label.padding,
                  na.rm = TRUE,
                  ...)
  )
}


# StatParty ---------------------------------------------------------------


# StatParty <- ggproto("StatParty", Stat,
#                      compute_group = function(data, scales = scales) {
#                        data <- data[!duplicated(data$id), ]
#                      }
# )

StatParty <- ggproto(
  "StatParty", Stat,
  compute_group = function(data, ids, shift = NULL, scales = scales) {
    if (!is.null(ids)) data <- data[ids, ]
    if (is.character(ids) && ids == "terminal") data <- data[data$kids == 0, ]
    # shift of edge_label
    if (!is.null(shift)){
      #browser()
      data$x <- (data$x * shift + data$x_parent * (1 - shift))
      data$y <- (data$y * shift + data$y_parent * (1 - shift))
    }
    data
  }
)


# adjust_mapping () -------------------------------------------------------


adjust_mapping <- function(default_mapping, mapping) {
  if (!is.null(mapping)) {
    mapping <- `class<-`(modifyList(default_mapping, mapping), "uneval")
  } else {
    mapping <- default_mapping
  }
}
