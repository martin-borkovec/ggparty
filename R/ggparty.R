#' Create a new ggparty plot
#'
#' `ggplot2` extension for objects of class `party`. Creates a `data.frame` from
#' an object of class `party` and calls [ggplot()]
#'
#'  `ggparty` can be callled directly with an object of class `party`, which will
#' convert it to a suitbale `data.frame` and pass it to a call to `ggplot` with as
#' the `data` argument. As usual, additional components can then be added with
#' `+`.
#'
#' The nodes will be spaced equally in the unit square. Specifying
#' `terminal_size` allows to increase or decrease the area for plots of the
#' terminal nodes.
#'
#' If one of the list entries supplied to `add_vars` is a function, it has to take
#'  exactly two arguments,
#'  namely `data` (the corresponding row of the plot_data data frame) and `node`
#'   (the corresponding node, i.e. `party_object[i]`)
#'
#' @param party Object of class `party`.
#' @param horizontal If `TRUE` plot will be horizontal.
#' @param terminal_space Proportion of the plot that should be reserved for
#' the terminal nodeplots. Defaults to `2 / (depth(party) + 2)`.
#' @param layout Optional layout adjustment. Overwrites the coordinates of the
#' specified nodes. Must be `data.frame` containing the
#'  columns `id`, `x` and `y`. With `x` and `y` values between 0 and 1.
#' @param add_vars Named list containing either string(s) specifying the locations
#'  of elements to be extracted from
#'  each node of `party`  or function(s) of corresponding row of plot data and node.
#'   In either case returned object  has to be of length 1.
#'   If the data is supposed to be accessible by [geom_node_plot()] the resprective
#'   list entry has
#'   to be named with the prefix `"nodedata_"` and be a function returning a list
#'   of same length as `nodesize`.
#'
#'
#'
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
  for (i in seq_along(add_vars)) {
    checkmate::assert(check_character(add_vars[[i]]),
                      check_function(add_vars[[i]], nargs = 2),
                      combine = "or")
    if (is.function(add_vars[[i]])) assert_set_equal(names(formals(add_vars[[i]])),
                                                     c("data", "node"),
                                                     .var.name = "function-arguments of add_vars")
  }


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
  mapping <- aes(x = !!sym("x"),
                 y = !!sym("y"),
                 x_parent = !!sym("x_parent"),
                 birth_order = !!sym("birth_order"),
                 y_parent = !!sym("y_parent"),
                 id = !!sym("id"),
                 kids = !!sym("kids"),
                 info = !!sym("info"),
                 info_list = !!sym("info_list"),
                 p.value = !!sym("p.value"),
                 splitvar = !!sym("splitvar"),
                 horizontal = !!sym("horizontal"),
                 nodesize = !!sym("nodesize"))


# add tree data to mapping ------------------------------------------------

  for (column_i in names(node_data)) {
    mapping <- adjust_mapping(mapping, aes(var = !!sym(column_i)))
    names(mapping)[length(mapping)] <- column_i
  }


# add add_vars to mapping -------------------------------------------------

  for (column_i in names(add_vars)) {
    mapping <- adjust_mapping(mapping, aes(var = !!sym(column_i)))
    names(mapping)[length(mapping)] <- column_i
  }

  ggplot(data = plot_data,
         mapping = mapping) +
    theme_void()
}


# geom_edge() -------------------------------------------------------------

#' Draw edges
#'
#' Draws edges between children and parent nodes. Wrapper for [ggplot2::geom_segment()]
#'
#' @param mapping Mapping of `x`, `y`, `xend` and `yend` defaults to `ids`' and
#'  their parent's coordinates. Other mappings can be added here as `aes()`.
#' @param nudge_x,nudge_y Nudge labels.
#' @param ids Choose which edges to draw by their children's ids.
#' @param show.legend `logical` See [layer()].
#' @param ... Additional arguments for [geom_segment()].
#'
#' @seealso [ggparty()], [geom_edge()]
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

geom_edge <- function(mapping = NULL, nudge_x = 0, nudge_y = 0, ids = NULL,
                      show.legend = NA, ...){

  default_mapping <- aes(x = !!sym("x"),
                         y = !!sym("y"),
                         xend = !!sym("x_parent"),
                         yend = !!sym("y_parent"))

  mapping <- adjust_mapping(default_mapping, mapping)

  layer(
    data = NULL,
    mapping = mapping,
    stat = StatParty,
    geom = GeomSegment,
    position = position_nudge(x = nudge_x, y = nudge_y),
    inherit.aes = TRUE,
    show.legend = show.legend,
    params = list(na.rm = TRUE,
                  ids = ids,
                  ...)
  )
}




# geom_edge_label() -------------------------------------------------------

#'Draw edge labels
#'
#' Label edges with corresponding split breaks
#'
#' @param mapping Mapping of `label` label defaults to **breaks_label**. Other
#'  mappings can be added here as `aes()`.
#' @param shift Value in (0,1). Moves label along corresponding edge.
#' @param ids Choose which splitbreaks to label by their children's ids.
#' @param nudge_x,nudge_y Nudge label.
#' @param splitlevels Which levels of split to plot. This may be usefull in the
#' presence of many factor levels for one split break.
#' @param max_length If provided **breaks_label** levels will be truncated to the specified length.
#' @param label.size See [geom_label()].
#' @param parse Needs to be true in order to parse inequality signs of **breaks_label**.
#' @param parse_all Defaults to `FALSE`, in which case everything but the inequality
#'  signs of **breaks_label** are deparsed. If `TRUE` complete **breaks_label** are parsed.
#' @param ... Additional arguments for [geom_label()].
#' @seealso [ggparty()]
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
                            max_length = NULL,
                            parse_all = FALSE,
                            parse = TRUE,
                            ...) {

  default_mapping <- aes(label = !!sym("breaks_label"))
  # if (!parse)
  #   default_mapping <- aes(label = paste(breaks_label))
  if (!parse_all & parse)
    default_mapping <- aes(label = !!expr(parse_signs(!!sym("breaks_label"),
                                           splitlevels = splitlevels,
                                           max_length = max_length)))

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
                  parse = parse,
                  ...)
  )
}



# StatParty ---------------------------------------------------------------

StatParty <- ggproto(
  "StatParty",
  Stat,
  compute_panel = function(data, ids = NULL, shift = NULL, scales = scales,
                        splitlevels = NULL, extract_info = NULL) {
    if (is.numeric(ids)) data <- data[ids, ]
    if (is.character(ids) && ids == "terminal") data <- data[data$kids == 0, ]
    if (is.character(ids) && ids == "inner") data <- data[data$kids != 0, ]
    # shift of edge_label
    if (!is.null(shift)) {
      data$x <- (data$x * shift + data$x_parent * (1 - shift))
      data$y <- (data$y * shift + data$y_parent * (1 - shift))
    }

    # if (!is.null(splitlevels))
    #   browser()
    #   data$label <- vapply(data$label, function(x) {
    #     index <- seq_along(x) %in% splitlevels
    #     output <- x[index]
    #     paste(output, collapse = " ")
    #   },
    #   character(1))
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



# parse_signs() -----------------------------------------------------------
# prevent rest of label from being parsed
parse_signs <- function(label, splitlevels = NULL, max_length = NULL) {
  first <- substring(label, 1, 9)
  last <- substring(label, 10)
  for (i in seq_along(label)) {
    if (first[i] %in% c("NA <= NA*", "NA >= NA*", "NA <  NA*", "NA >  NA*"))
      label[i] <- paste0(first[i], unlist(lapply(last[i], FUN = deparse)))
    else {
      if (!is.null(splitlevels)) {
        label[i] <- vapply(label[i], function(x) {
          index <- seq_along(x) %in% splitlevels
          output <- x[index]
          if (!is.null(max_length))
            output <- substring(output, 1, max_length)
          paste(output, collapse = " ")
        },
        character(1))
      }
      label[i] <- unlist(lapply(label[i], FUN = deparse))
    }
  }
  label
}

