#' ggplot an object of the party class
#'
#' @param party partyobject to plot
#' @param horizontal horizontal plot?
#' @param terminal_space proportion of the plot that should be reserved for
#' the terminal nodeplots
#' @param layout optional layout adjustment. Must be data.frame containing the
#'  columns "id", "x" and "y".
#' @seealso [geom_edge()], [geom_edge_label()], [geom_node_splitvar()],
#'  [geom_node_info()], [geom_nodeplot()]
#' @export
#' @import partykit
#' @import ggplot2
#' @import gtable
#' @import grid
#' @import checkmate


# ggparty() ---------------------------------------------------------------


ggparty <- function(party, horizontal = FALSE, terminal_space, layout = NULL,
                    add_vars = NULL) {

  if(missing(terminal_space)) terminal_space <- 2 / (depth(party) + 2)
  if (!is.null(layout)) {
    checkmate::assert_data_frame(layout, any.missing = FALSE)
    checkmate::assert_names(colnames(layout), permutation.of = c("id", "x", "y"))
    checkmate::assert_integerish(layout$id, lower = 1, unique = TRUE)
    checkmate::assert_numeric(layout$x, lower = 0, upper = 1)
    checkmate::assert_numeric(layout$y, lower = 0, upper = 1)
  }

  plot_data <- get_plot_data(party,
                             horizontal = horizontal,
                             terminal_space = terminal_space,
                             add_vars = add_vars)

  if(!is.null(layout)) plot_data <- adjust_layout(plot_data, layout)
  node_data <- dplyr::select(plot_data, dplyr::starts_with("data_"))
  mapping <- aes_string(x = "x", y = "y", x_parent = "x_parent",
                 y_parent = "y_parent", id = "id", kids = "kids", info = "info")

  for (column_i in names(node_data)) {
    mapping <- adjust_mapping(mapping, aes_string(var = paste0("`", column_i, "`")))
    names(mapping)[length(mapping)] <- column_i
  }

  ggplot(data = plot_data,
         mapping = mapping) +
    theme_void() +
    xlim(0, 1)
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

  default_mapping <- aes_string(x = "x",
                         y = "y",
                         xend = "x_parent",
                         yend = "y_parent")

  mapping <- adjust_mapping(default_mapping, mapping)

  layer(
    data = NULL,
    mapping = mapping,
    stat = StatParty,
    geom = "segment",
    position = position_nudge(x = x_nudge, y = y_nudge),
    inherit.aes = TRUE,
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
#' @param splitlevels which levels of split to plot
#' @param label.size see [geom_label()]
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
                            label.size = 0,
                            splitlevels = seq_len(100),
                            ...) {

  default_mapping <- aes(label = index)

  mapping <- adjust_mapping(default_mapping, mapping)

  layer(
    data = NULL,
    mapping = mapping,
    stat = StatParty,
    geom = "label",
    position = position_nudge(x = x_nudge, y = y_nudge),
    inherit.aes = TRUE,
    params = list(ids = ids,
                  shift = shift,
                  label.size = label.size,
                  na.rm = TRUE,
                  splitlevels = splitlevels,
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
  default_mapping <- aes_string(label = "info")
  mapping <- adjust_mapping(default_mapping, mapping)
  layer(
    data = NULL,
    mapping = mapping,
    stat = StatParty,
    geom = "label",
    position = position_nudge(x = x_nudge, y = y_nudge),
    inherit.aes = TRUE,
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
#' @param extract_info work in progress
#' @export
#' @md
#'
geom_node_splitvar <- function(mapping = NULL, x_nudge = 0, y_nudge = 0,
                               label.padding = unit(0.5, "lines"), ids = NULL,
                               extract_info = NULL,...) {
  default_mapping <- aes_string(label = "splitvar")
  mapping <- adjust_mapping(default_mapping, mapping)
  layer(
    data = NULL,
    mapping = mapping,
    stat = StatParty,
    geom = "label",
    position = position_nudge(x = x_nudge, y = y_nudge),
    inherit.aes = TRUE,
    params = list(ids = ids,
                  label.padding = label.padding,
                  na.rm = TRUE,
                  extract_info = extract_info,
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
  compute_group = function(data, ids, shift = NULL, scales = scales, splitlevels = NULL,
                           extract_info = NULL) {
    if (!is.null(ids)) data <- data[ids, ]
    if (is.character(ids) && ids == "terminal") data <- data[data$kids == 0, ]
    # shift of edge_label
    if (!is.null(shift)){

      data$x <- (data$x * shift + data$x_parent * (1 - shift))
      data$y <- (data$y * shift + data$y_parent * (1 - shift))
    }

    if (!is.null(splitlevels))
      data$label <- vapply(data$label, function(x) {
        index <- seq_along(x) %in% splitlevels
        output <- x[index]
        paste(output, collapse = " ")
      },
      character(1))
    # browser()
    # if (!is.null(extract_info)) {
    #   for (j in seq_along(data$id)) {
    #     if (j == 1) extract_info_data <- extract_info(lapply(data[j, 1:8], unlist))
    #     else extract_info_data <- rbind(extract_info_data, extract_info(data[j, ]))
    #   }
    #   data <- cbind(data, extract_info_data)
    # }

    # browser()
    # for (j in seq_along(data$id)) {
    #   for (i in seq_along(data$info[[1]])) {
    #     data[j, names(data$info[[1]])[i]] <- data$info[[j]][i]
    #   }}
    data
  }
)


# adjust_mapping () -------------------------------------------------------


adjust_mapping <- function(default_mapping, mapping) {
  if (!is.null(mapping)) {
    mapping <- `class<-`(utils::modifyList(default_mapping, mapping), "uneval")
  } else {
    mapping <- default_mapping
  }
}

# adjust_layout()

adjust_layout <- function(plot_data, layout) {
  for (id in layout$id) {
    plot_data$y[plot_data$id == id] <- layout$y[layout$id == id]
    plot_data$y_parent[plot_data$parent == id] <- layout$y[layout$id == id]
    plot_data$x[plot_data$id == id] <- layout$x[layout$id == id]
    plot_data$x_parent[plot_data$parent == id] <- layout$x[layout$id == id]
  }
  plot_data
}





# StatText <- ggproto(
#   "StatText", Stat,
#   compute_group = function(data, ids, shift = NULL, scales = scales, splitlevels = NULL,
#                            extract_info = NULL) {
#   #browser()
#     if (!is.null(ids)) data <- data[ids, ]
#     if (is.character(ids) && ids == "terminal") data <- data[data$kids == 0, ]
#     # shift of edge_label
#     # if (!is.null(shift)){
#     #
#     #   data$x <- (data$x * shift + data$x_parent * (1 - shift))
#     #   data$y <- (data$y * shift + data$y_parent * (1 - shift))
#     # }
#     #
#     # if (!is.null(splitlevels))
#     #   data$label <- vapply(data$label, function(x) {
#     #     index <- seq_along(x) %in% splitlevels
#     #     output <- x[index]
#     #     paste(output, collapse = " ")
#     #   },
#     #   character(1))
#
# #browser()
#     if (!is.null(extract_info)) {
#       for (j in seq_along(data$id)) {
#          if (is.null(data$info[[j]])) next
#         #   }
#         #if (j == 1) extract_info_data <- extract_info(data$info[[j]])
#         #else extract_info_data <- rbind(extract_info_data, extract_info(data$info[[j]]))
#         data[j, "label"] <- extract_info(data$info[[j]])
#
#       }
#       #browser()
#       #data <- cbind(data, extract_info_data)
#     }
#
#     data
#   }
# )

