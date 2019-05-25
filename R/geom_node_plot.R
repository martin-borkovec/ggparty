#' Draw plots at nodes
#'
#' Additional component for a [ggparty()] that allows to create in each node a
#' ggplot with its data. #'
#'
#' @param plot_call Any function that generates a `ggplot2` object.
#' @param gglist List of additional `gg` components. Columns of `data` of nodes can be
#'  mapped. Additionally `fitted_values` and `residuals` can be mapped if present in
#'  `party` of `ggparty()`
#' @param width Expansion factor for viewport's width.
#' @param height Expansion factor for viewport's height.
#' @param size Expansion factor for viewport's size.
#' @param ids Id's to plot. Numeric, "terminal", "inner" or "all". Defaults
#' to "terminal".
#' @param scales See [facet_wrap()]
#' @param nudge_x,nudge_y Nudges node plot.
#' @param shared_axis_labels If TRUE only one pair of axes labels is plotted in
#' the terminal space. Only recommended if `ids`  "terminal" or "all".
#' @param predict Character string specifying variable for which predictions should be plotted.
#' @param predict_gpar Named list containing arguments to be passed to the
#' `geom_line()` call of predicted values.
#' @param shared_legend If `TRUE` one shared legend is plotted at the bottom of
#' the tree.
#' @param legend_separator If `TRUE` line between legend and tree is drawn.
#' @import checkmate
#'
#' @export
#' @seealso [ggparty()]
#' @examples
#'
#' library(ggparty)
#'
#' airq <- subset(airquality, !is.na(Ozone))
#' airct <- ctree(Ozone ~ ., data = airq)
#'
#' ggparty(airct, horizontal = TRUE, terminal_space = 0.6) +
#'   geom_edge() +
#'   geom_edge_label() +
#'   geom_node_splitvar() +
#'   geom_node_plot(gglist = list(
#'     geom_density(aes(x = Ozone))),
#'     shared_axis_labels = TRUE)
#'
#' #############################################################
#'
#' ## Plot with ggparty
#'
#'
#' ## Demand for economics journals data
#' data("Journals", package = "AER")
#' Journals <- transform(Journals,
#'                       age = 2000 - foundingyear,
#'                       chars = charpp * pages)
#'
#' ## linear regression tree (OLS)
#' j_tree <- lmtree(log(subs) ~ log(price/citations) | price + citations +
#'                    age + chars + society, data = Journals, minsize = 10, verbose = TRUE)
#'
#' pred_df <- get_predictions(j_tree, ids = "terminal", newdata =  function(x) {
#'   data.frame(
#'     citations = 1,
#'     price = exp(seq(from = min(x$`log(price/citations)`),
#'                     to = max(x$`log(price/citations)`),
#'                     length.out = 100)))
#' })
#'
#' ggparty(j_tree, terminal_space = 0.8) +
#'   geom_edge() +
#'   geom_edge_label() +
#'   geom_node_splitvar() +
#'   geom_node_plot(gglist =
#'                    list(aes(x = `log(price/citations)`, y = `log(subs)`),
#'                         geom_point(),
#'                         geom_line(data = pred_df,
#'                                   aes(x = log(price/citations),
#'                                       y = prediction),
#'                                   col = "red")))



# geom_node_plot () --------------------------------------------------------


geom_node_plot <- function(plot_call = "ggplot",
                           gglist = NULL,
                           width = 1,
                           height = 1,
                           size = 1,
                           ids = "terminal",
                           scales = "fixed",
                           nudge_x = 0,
                           nudge_y = 0,
                           shared_axis_labels = FALSE,
                           shared_legend = TRUE,
                           predict = NULL,
                           predict_gpar = NULL,
                           legend_separator = FALSE) {

  #input_checks
  assert_list(gglist)
  assert_numeric(width, lower = 0, finite = TRUE, any.missing = FALSE, max.len = 1)
  assert_numeric(height, lower = 0, finite = TRUE, any.missing = FALSE, max.len = 1)
  assert_subset(scales, c("fixed", "free", "free_x", "free_y"))
  assert_numeric(nudge_x, lower = -1, upper = 1, finite = TRUE, any.missing = FALSE,
                 max.len = 1)
  assert_logical(shared_axis_labels)
  # assert_list(predict_gpar, null.ok = TRUE, names = "unique")
  assert_logical(legend_separator)

  nodeplot_layer <- ggplot2::layer(
    data = NULL,
    mapping = NULL,
    stat = "identity",
    geom = GeomNodeplot,
    position = position_nudge(x = nudge_x, y = nudge_y),
    params = list(
      gglist = gglist,
      plot_call = enquote(plot_call),
      width = width,
      height = height,
      size = size,
      ids = ids,
      scales = scales,
      shared_axis_labels = shared_axis_labels,
      shared_legend = shared_legend,
      predict = predict,
      predict_gpar = predict_gpar,
      nudge_x = nudge_x,
      nudge_y = nudge_y))

  # set correct plot specs and draw legend separator
  if (shared_axis_labels) {
    nodeplot_layer <- list(nodeplot_layer, coord_cartesian(ylim = c(-0.075, 1.05),
                                                           xlim = c(-0.05, 1.05),
                                                           default = TRUE))
    if (legend_separator) nodeplot_layer <- list(nodeplot_layer,
                                                 geom_hline(yintercept = -0.05))
  } else {
    nodeplot_layer <- list(nodeplot_layer, coord_cartesian(ylim = c(-0.025, 1.05),
                                                           xlim = c(-0.05, 1.05),
                                                           default = TRUE))
    if (legend_separator) nodeplot_layer <- list(nodeplot_layer,
                                                 geom_hline(yintercept = 0))
  }
  nodeplot_layer

}


# GeomNodeplot ------------------------------------------------------------

GeomNodeplot <- ggproto(
  "GeomNodeplot",
  Geom,
  required_aes = c("x", "y", "id"),
  draw_panel = function(self,
                        data,
                        panel_params,
                        coord,
                        gglist,
                        plot_call,
                        width,
                        height,
                        size,
                        ids,
                        shared_axis_labels,
                        shared_legend,
                        scales,
                        predict,
                        predict_gpar,
                        nudge_x,
                        nudge_y) {

    data <- coord$transform(data, panel_params)

    vertical <- all(data$horizontal == FALSE)

    # rescale some constants and inputs
    y_0 <- scales::rescale(0, from = panel_params$y.range)
    y_1 <- scales::rescale(1, from = panel_params$y.range)
    x_1 <- scales::rescale(1, from = panel_params$x.range)
    x_0 <- scales::rescale(0, from = panel_params$x.range)
    legend_x <- scales::rescale(0.5, from = panel_params$x.range)
    legend_y <- scales::rescale(-0.05, from = panel_params$y.range)
    xlab_y <- scales::rescale(-0.025, from = panel_params$y.range)
    nudge_y <- scales::rescale(nudge_y, from = panel_params$y.range) - y_0
    nudge_x <- scales::rescale(nudge_x, from = panel_params$x.range) - x_0


    # calculate node node sizes ------------------------------------------------

    # for vertical trees
    if (vertical) {
      node_width <- mean(abs(diff(data$x[data$kids == 0])))
      node_height <- mean(abs(diff(data$y[data$kids != 0])))
      xlab_x <- legend_x
      ylab_y <- (min(data$y) + y_0) * 0.5
      ylab_x <- scales::rescale(-0.0, from = panel_params$x.range)
    } else {
      # for horizontal trees
      node_width <- mean(abs(diff(data$x[data$kids != 0])))
      node_height <- mean(abs(diff(data$y[data$kids == 0])))
      xlab_x <- (x_1 + max(data$x)) / 2
      ylab_x <- max(data$x)
      ylab_y <- scales::rescale(0.5, from = panel_params$y.range)
    }

    grob_list <- list()
    if (all(ids == "all")) ids <- unique(data$id)
    if (all(ids == "terminal")) ids <- unique(data$id[data$kids == 0])
    if (all(ids == "inner")) ids <- unique(data$id[data$kids != 0])

    #  transform nodedata_* columns from lists to full dataframe -------------------
    data_columns <- substring(names(data), first = 1, last = 9) == "nodedata_"
    nodeplot_data_lists <-  data[, data_columns]
    names(nodeplot_data_lists) <- substring(names(nodeplot_data_lists), 10)
    lengths <- sapply(nodeplot_data_lists[[2]], length)
    # initialize dataframe with right number of rows per id
    nodeplot_data <- data.frame(id = rep(data$id, times = lengths))
    # fill dataframe
    for (column in names(nodeplot_data_lists)) {
      # browser()
      content <- vector()
      for (i in seq_along(data$id)) {
        rows <- nodeplot_data_lists[[column]][[i]]
        if (is.factor(rows)) rows <- as.character(rows)
        content <- c(content, rows)
      }
      if (is.factor(nodeplot_data_lists[[column]][[1]]))
        content <- factor(content,
                          levels = levels(nodeplot_data_lists[[column]][[1]]))
      nodeplot_data[[column]] <- content
    }

    # prepare data -------------------------------------------------------------

    base_data <- nodeplot_data
    facet_data <- base_data[nodeplot_data$id %in% ids, ]
    data <- data[data$id %in% ids, ]

    # nodesize ----------------------------------------------------------------

    nodesize <- data$nodesize[data$id %in% ids]
    if (length(width) == 1) width <- rep(width, length(ids))
    else assert_subset(length(width), choices = c(1, length(ids)))

    if (length(height) == 1) height <- rep(height, length(ids))
    else assert_subset(length(height), choices = c(1, length(ids)))

    if (size[1] == "nodesize") size <-
      scales::rescale(nodesize, to = c(0, 1), from = c(0, max(nodesize)))
    if (size[1] == "log(nodesize)")
      size <- scales::rescale(log(nodesize), to = c(0,1),
                              from = c(0, max(log(nodesize))))
    if (length(size) == 1) size <- rep(size, length(ids))
    else assert_subset(length(size), choices = c(1, length(ids)))


    # calculate newdata and resulting predictions -----------------------------
    if (!is.null(predict))
      predicted_data <- predict_data(data$info_list, facet_data, predict)

    # generate faceted base_plot ----------------------------------------------
    # facet_wrap for indiviual panels
    facet_gtable <- ggplotGrob(do.call(plot_call,
                                       args = list(data = facet_data)) +
                                 lapply(gglist, eval.parent, n = 1) +
                                 theme(legend.position = "bottom") +
                                 facet_wrap( ~ id, scales = scales, nrow = 1) +
                                 if (!is.null(predict)) {
                                   do.call(geom_line, c(list(
                                     data = predicted_data,
                                     mapping = aes(x = !!sym(predict),
                                                   y = prediction)),
                                     predict_gpar))
                                 }
                               )

    # lab specifications for base table
    ggxlab <- ifelse(shared_axis_labels,
                     list(theme(axis.title.x = element_blank())),
                     list())
    ggylab <- list()
    if (shared_axis_labels & vertical) ggylab <- list(theme(axis.title.y = element_blank()))
    if (shared_axis_labels & !vertical) ggylab <- list(ylab(" "))

    # base plot as template
    base_gtable <- ggplotGrob(do.call(plot_call,
                                      args = list(data = base_data)) +
                                lapply(gglist, eval.parent, n = 1) +
                                ggxlab +
                                ggylab +
                                if (shared_legend) theme(legend.position = "none")
                              )
    # legend ------------------------------------------------------------------
    if (shared_legend) {
      if (any(facet_gtable$layout$name == "guide-box")) {
        # extract gtable containing legend
        legend_gtable <- reduce_gtable(facet_gtable, "guide-box")
        legend_gtable$layout$t <- 1
        legend_gtable$layout$b <- 1
        legend_gtable$heights <- unit(1,"pt")

        #call nodeplotGrob on legend_gtable
        legend_gtable <- nodeplotGrob(
          x = legend_x,
          y = legend_y,
          width = 1,
          height = abs(legend_y - y_0),
          just = ifelse(shared_axis_labels, "top", "bottom"),
          node_gtable = legend_gtable
        )
        # add to groblist
        grob_list <- c(grob_list, list(legend_gtable))
      }
    }

    # shared axis labels ------------------------------------------------------

    if (shared_axis_labels) {


      # x axis label -----------------------------------------------------------

      if (any(facet_gtable$layout$name == "xlab-b")) {
        xlab_gtable <- reduce_gtable(facet_gtable, "xlab-b")
        xlab_gtable$layout$t <- 1
        xlab_gtable$layout$b <- 1
        xlab_gtable$heights <- unit(1,"pt")

        #call nodeplotGrob on xlab_gtable
        xlab_gtable <- nodeplotGrob(
          x = xlab_x,
          y = y_0,
          width = 0,#1,
          height = 0.01,#(y_0 - xlab_y),
          just = "top",
          node_gtable = xlab_gtable
          )
        grob_list <- c(grob_list, list(xlab_gtable))
      }


      # y axis label ------------------------------------------------------------

      if (any(facet_gtable$layout$name == "ylab-l")) {
        ylab_gtable <- reduce_gtable(facet_gtable, "ylab-l")
        ylab_gtable$layout$l <- 1
        ylab_gtable$layout$r <- 1
        ylab_gtable$layout$t <- 1
        ylab_gtable$layout$b <- 1
        ylab_gtable$widths <- unit(0,"pt")
        ylab_gtable$heights <- unit(0,"pt")
        # ylab_gtable$grobs[[1]]$vp <- NULL
        # ylab_gtable$grobs[[1]]$widths <- unit(0,"pt")
        # ylab_gtable$grobs[[1]]$heights <- unit(0,"pt")

        #call nodeplotGrob on ylab_gtable
        ylab_gtable <- nodeplotGrob(
          y = ylab_y,
          x = ylab_x,
          width = unit(1, "lines"),
          height = 0,
          just = "right",
          node_gtable = ylab_gtable
        )
        grob_list <- c(grob_list, list(ylab_gtable))
      }
    }

    # nodeplots ---------------------------------------------------------------

    # iterate through all ids to get all nodeplots
    nodeplot_gtable <- lapply(seq_along(ids), function(i) {

      # panel ------------------------------------------------------------------

      # create node_gtable as copy of base_gtable
      node_gtable <- base_gtable
      # get index of panel grob in base_gtable
      base_panel_index <- find_grob(base_gtable$grobs, "panel")
      # get index of ith panel in facet_gtable
      panel_i <- paste0("panel-", i,".")
      facet_panel_index <- find_grob(facet_gtable$grobs, panel_i)
      # replace panel grob in node_gtable with ith panel grob of facet_gtable
      node_gtable$grobs[[base_panel_index]] <- facet_gtable$grobs[[facet_panel_index]]

      # y axis ------------------------------------------------------------------

      base_y_axis_index <-  which(base_gtable$layout$name == "axis-l")

      if (scales == "fixed" | scales == "free_x" ) {
        facet_y_axis_index <- which(facet_gtable$layout$name == "axis-l-1-1")
      }
      if (scales == "free_y" | scales == "free") {
        y_axis_i <- paste0("axis-l-1-", i)
        facet_y_axis_index <-  which(facet_gtable$layout$name == y_axis_i)
      }
      node_gtable$grobs[[base_y_axis_index]] <- facet_gtable$grobs[[facet_y_axis_index]]


      # x axis ------------------------------------------------------------------

      base_x_axis_index <-  which(base_gtable$layout$name == "axis-b")
      x_axis_i <- paste0("axis-b-", i,"-1")
      facet_x_axis_index <-  which(facet_gtable$layout$name == x_axis_i)
      node_gtable$grobs[[base_x_axis_index]] <- facet_gtable$grobs[[facet_x_axis_index]]

      # generate nodeplotGrob ---------------------------------------------------

      # get x and y coords of nodeplot
      x <- unique(data[data$id == ids[i], "x"])
      y <- unique(data[data$id == ids[i], "y"])
      #vertical tree
      if (vertical) {
        nodeplotGrob(
          x = x,
          y = y,
          width = node_width * width[i] * size[i],
          height = ifelse(data$kids[data$id == ids[i]] == 0,
                          abs(y - nudge_y - y_0),
                          node_height) * height[i] * size[i],
          just = ifelse(data$kids[data$id == ids[i]] == 0,
                        "top",
                        "center"),
          node_gtable = node_gtable
        )
      } else {#horizontal tree
        nodeplotGrob(
          x = x,
          y = y,
          width = ifelse(data$kids[data$id == ids[i]] == 0,
                         abs(x - nudge_x - x_1),
                         node_width) * width[i] * size[i],
          height = node_height * height[i] * size[i],
          just = ifelse(data$kids[data$id == ids[i]] == 0,
                        "left",
                        "center"),
          node_gtable = node_gtable
        )
      }
    })
    #combine nodeplots and legend
    grob_list <- c(nodeplot_gtable, grob_list)
    class(grob_list) <- "gList"
    ggname("geom_node_plots", grid::grobTree(children = grob_list))
  }
)


nodeplotGrob <- function(x, y, node_gtable, width, height, just) {

  grid::gTree(x = x,
              y = y,
              node_gtable = node_gtable,
              width = width,
              height = height,
              just = just,
              cl = "nodeplotgrob")

}

#' appearantly needs to be exported
#' @param x nodeplotgrob
#' @export
#' @md
makeContent.nodeplotgrob <- function(x) {

  r <- x$node_gtable
  r$vp <- grid::viewport(x = x$x,
                         y = x$y,
                         width = x$width,
                         height = x$height,
                         just = x$just)

  grid::setChildren(x, grid::gList(r))
}

find_grob <- function(x, name) {
  which(sapply(x, function(x)
    substring(x$name, first = 1, last = nchar(name)) == name))
}

# function to remove selected elements from gtables, keeping widths
remove_grob <- function(g, what = "guide-box") {
  matches <- c(grepl(pattern = what, g$layout$name))
  g$layout <- g$layout[!matches, , drop = FALSE]
  g$grobs <- g$grobs[!matches]
  return(g)
}

reduce_gtable <- function(g, what = "guide-box") {
  matches <- c(grepl(pattern = what, g$layout$name))
  g$layout <- g$layout[matches, , drop = FALSE]
  g$grobs <- g$grobs[matches]
  return(g)
}

ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}



predict_data <- function(info_list, data, predict) {
  ids <- unique(data$id)
  for (i in seq_along(ids)) {
    pred_data <- data[[predict]]
    newdata <- data.frame(predict = seq(min(pred_data),
                                        max(pred_data),
                                       length.out = 100))
    names(newdata) <- predict
    predict_data <- newdata
    predict_data$id <- ids[i]
    predict_data$prediction <- stats::predict(info_list[[i]]$object,
                                              newdata = newdata)

    if (i == 1) resulting_data <- predict_data
    else resulting_data <- rbind(resulting_data, predict_data)
  }
  resulting_data
}

#' Create data.frame with predictions for each node
#'
#' @param party_object object of class `party`
#' @param ids Id's to plot. Numeric, "terminal", "inner" or "all". MUST be identical
#' to `ids` of [geom_node_plot()] used to plot this data.
#' @param newdata_fun function which takes `data` of node and returns `newdata`
#' for `predict()`
#' @param predict_arg list of additional arguments passed to [predict()]
#'
#' @export

get_predictions <- function(party_object, ids, newdata_fun, predict_arg = NULL) {

  if (all(ids == "all")) ids <- seq_along(party_object)
  if (all(ids == "terminal")) ids <- unique(fitted_node(party_object$node,
                                                        party_object$data))
  if (all(ids == "inner"))
    ids <-
      seq_along(party_object)[-intersect(seq_along(party_object),
                                         unique(fitted_node(party_object$node,
                                                            party_object$data)))]

  for (i in ids) {
    predict_arg$newdata <- do.call(newdata_fun, list(party_object$data))
    predict_arg$object <- party_object[i]$node$info$object
    predict_data <- predict_arg$newdata
    predict_data$id <- i
    predict_data$prediction <- do.call(stats::predict, predict_arg)

    if (i == ids[1]) resulting_data <- predict_data
    else resulting_data <- rbind(resulting_data, predict_data)
  }
  resulting_data
}

