#' `ggplot2` extension for objects of class `party`
#'
#' Converts an object of class `party` and calls ggplot
#'
#'  `ggparty` can be callled directly with an object of class `party`, which will
#' convert it to a suitbale data.frame and create a call to `ggplot` with it as
#' the `data` argument. As usual additional components can then be added with
#' `+`.
#' The nodes will be spaced equally in the unit square. Specifying
#' `terminal_size` allows to increase or decrease the area for plots of the
#' terminal nodes.
#'
#'
#' @param party Object of class `party`. The elements necessary for the plot will
#' be extracted and converted to a `data.frame`.
#' @param horizontal If `TRUE` plot will be horizontal.
#' @param terminal_space Proportion of the plot that should be reserved for
#' the terminal nodeplots.
#' @param layout Optional layout adjustment. Overrides the coordinates of the
#' specified nodes. Must be `data.frame` containing the
#'  columns "id", "x" and "y". With `x` and `y` values between 0 and 1.
#' @param add_vars Named list containing either strings specifying the locations
#'  of elements to be extracted from
#'  each node of `party`  or functions of `plot_data()` and node. In either case
#'   returned object
#'  has to be of length 1.
#' @seealso [geom_edge()], [geom_edge_label()], [geom_node_label()],
#'  [autoplot.party()], [geom_node_plot()]
#' @export
#' @import partykit
#' @import ggplot2
#' @import gtable
#' @import grid
#' @import checkmate
#' @examples
#' library(ggparty)
#' data("WeatherPlay", package = "partykit")
#' sp_o <- partysplit(1L, index = 1:3)
#' sp_h <- partysplit(3L, breaks = 75)
#' sp_w <- partysplit(4L, index = 1:2)
#' pn <- partynode(1L, split = sp_o, kids = list(
#'   partynode(2L, split = sp_h, kids = list(
#'     partynode(3L, info = "yes"),
#'     partynode(4L, info = "no"))),
#'   partynode(5L, info = "yes"),
#'   partynode(6L, split = sp_w, kids = list(
#'     partynode(7L, info = "yes"),
#'     partynode(8L, info = "no")))))
#' py <- party(pn, WeatherPlay)
#'
#' ggparty(py) +
#'   geom_edge() +
#'   geom_edge_label() +
#'   geom_node_label(aes(label = splitvar),
#'                   ids = "inner") +
#'   geom_node_label(aes(label = info),
#'                   ids = "terminal")


# ggparty() ---------------------------------------------------------------


ggparty <- function(party, horizontal = FALSE, terminal_space, layout = NULL,
                    add_vars = NULL) {

  if (missing(terminal_space)) terminal_space <- 2 / (depth(party) + 2)

  checkmate::assert_class(party, "party")
  checkmate::assert_logical(horizontal)
  checkmate::assert_numeric(terminal_space, lower = 0, upper = 1)
  checkmate::assert_list(add_vars, names = "named", null.ok = TRUE)
  for (i in seq_along(add_vars))
    checkmate::assert(check_character(add_vars[[i]]),
                      check_function(add_vars[[i]], nargs = 2),
                      combine = "or")

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

  if (!is.null(layout)) plot_data <- adjust_layout(plot_data, layout)
  data_columns <- substring(names(plot_data), first = 1, last = 9) == "nodedata_"
  node_data <-  plot_data[, data_columns]
  mapping <- aes_string(x = "x", y = "y", x_parent = "x_parent",
                 y_parent = "y_parent", id = "id", kids = "kids", info = "info",
                 info_list = "info_list", p.value = "p.value",
                 splitvar = "splitvar", horizontal = "horizontal",
                 nodesize = "nodesize")


# add tree data to mapping ------------------------------------------------

  for (column_i in names(node_data)) {
    mapping <- adjust_mapping(mapping, aes_string(var = paste0("`", column_i, "`")))
    names(mapping)[length(mapping)] <- column_i
  }


# add add_vars to mapping -------------------------------------------------

  for (column_i in names(add_vars)) {
    mapping <- adjust_mapping(mapping, aes_string(var = paste0(column_i)))
    names(mapping)[length(mapping)] <- column_i
  }

  ggplot(data = plot_data,
         mapping = mapping) +
    theme_void()
}


# geom_edge() -------------------------------------------------------------
#' Draw edges between children and parents. wrapper of geom_segment
#'
#' @param mapping not recommended to change
#' @param ... additional arguments for [geom_segment()]
#' @param nudge_x,nudge_y nudge label
#' @param ids choose which edges to draw by their children's ids
#' @export
#' @md
#' @examples
#' library(ggparty)
#' data("WeatherPlay", package = "partykit")
#' sp_o <- partysplit(1L, index = 1:3)
#' sp_h <- partysplit(3L, breaks = 75)
#' sp_w <- partysplit(4L, index = 1:2)
#' pn <- partynode(1L, split = sp_o, kids = list(
#'   partynode(2L, split = sp_h, kids = list(
#'     partynode(3L, info = "yes"),
#'     partynode(4L, info = "no"))),
#'   partynode(5L, info = "yes"),
#'   partynode(6L, split = sp_w, kids = list(
#'     partynode(7L, info = "yes"),
#'     partynode(8L, info = "no")))))
#' py <- party(pn, WeatherPlay)
#'
#' ggparty(py) +
#'   geom_edge() +
#'   geom_edge_label() +
#'   geom_node_label(aes(label = splitvar),
#'                   ids = "inner") +
#'   geom_node_label(aes(label = info),
#'                   ids = "terminal")

geom_edge <- function(mapping = NULL, nudge_x = 0, nudge_y = 0, ids = NULL, ...){

  default_mapping <- aes_string(x = "x",
                                y = "y",
                                xend = "x_parent",
                                yend = "y_parent")

  mapping <- adjust_mapping(default_mapping, mapping)

  layer(
    data = NULL,
    mapping = mapping,
    stat = "identity",
    geom = GeomSegment,
    position = position_nudge(x = nudge_x, y = nudge_y),
    inherit.aes = TRUE,
    params = list(na.rm = TRUE,
                  ...)
  )
}




# geom_edge_label() -------------------------------------------------------

#' Label edge with corresponding splitbreak
#'
#' @param mapping not recommended to change
#' @param shift value in (0,1). Move label along corresponding edge.
#' @param ids choose which splitbreaks to label by their children's ids
#' @param nudge_x,nudge_y nudge label
#' @param splitlevels which levels of split to plot
#' @param label.size see [geom_label()]
#' @param ... additional arguments for [geom_label()]
#'
#' @export
#' @md
#' @examples
#' library(ggparty)
#' data("WeatherPlay", package = "partykit")
#' sp_o <- partysplit(1L, index = 1:3)
#' sp_h <- partysplit(3L, breaks = 75)
#' sp_w <- partysplit(4L, index = 1:2)
#' pn <- partynode(1L, split = sp_o, kids = list(
#'   partynode(2L, split = sp_h, kids = list(
#'     partynode(3L, info = "yes"),
#'     partynode(4L, info = "no"))),
#'   partynode(5L, info = "yes"),
#'   partynode(6L, split = sp_w, kids = list(
#'     partynode(7L, info = "yes"),
#'     partynode(8L, info = "no")))))
#' py <- party(pn, WeatherPlay)
#'
#' ggparty(py) +
#'   geom_edge() +
#'   geom_edge_label() +
#'   geom_node_label(aes(label = splitvar),
#'                   ids = "inner") +
#'   geom_node_label(aes(label = info),
#'                   ids = "terminal")


geom_edge_label <- function(mapping = NULL,
                            nudge_x = 0,
                            nudge_y = 0,
                            ids = NULL,
                            shift = 0.5,
                            label.size = 0,
                            splitlevels = seq_len(100),
                            ...) {

  default_mapping <- aes_string(label = "breaks_label")

  mapping <- adjust_mapping(default_mapping, mapping)

  layer(
    data = NULL,
    mapping = mapping,
    stat = StatParty,
    geom = "label",
    position = position_nudge(x = nudge_x, y = nudge_y),
    inherit.aes = TRUE,
    params = list(ids = ids,
                  shift = shift,
                  label.size = label.size,
                  na.rm = TRUE,
                  splitlevels = splitlevels,
                  ...)
  )
}



# StatParty ---------------------------------------------------------------

StatParty <- ggproto(
  "StatParty", Stat,
  compute_group = function(data, ids = NULL, shift = NULL, scales = scales, splitlevels = NULL,
                           extract_info = NULL) {
    #browser()
    if (is.numeric(ids)) data <- data[ids, ]
    if (is.character(ids) && ids == "terminal") data <- data[data$kids == 0, ]
    if (is.character(ids) && ids == "inner") data <- data[data$kids != 0, ]
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


# adjust_layout() ---------------------------------------------------------

adjust_layout <- function(plot_data, layout) {
  for (id in layout$id) {
    plot_data$y[plot_data$id == id] <- layout$y[layout$id == id]
    plot_data$y_parent[plot_data$parent == id] <- layout$y[layout$id == id]
    plot_data$x[plot_data$id == id] <- layout$x[layout$id == id]
    plot_data$x_parent[plot_data$parent == id] <- layout$x[layout$id == id]
  }
  plot_data

}



