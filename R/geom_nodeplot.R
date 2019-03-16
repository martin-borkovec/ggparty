#' generate geom of nodeplot
#'
#' @param plot_call any function that generates a ggplot2 object.
#' @param gglist list of additional ggplot components. Data of nodes can be
#'  mapped. Fitted values of modelparty objects can be mapped with "fitted_values".
#' @param width expansion factor for viewport's width
#' @param height expansion factor for viewport's height
#' @param ids which ids to plot. numeric, "terminal", "inner" ar "all". defaults
#' to "terminal"
#' @param scales see [facet_wrap()]
#' @param x_nudge,y_nudge nudge nodeplot
#' @param shared_axes_labels if TRUE only one pair of axes labels is plotted in
#'  the terminal space. Only recommended if `ids`  "terminal" or "all".
#' @export
#' @seealso [ggparty()]
#' @md


# geom_nodeplot () --------------------------------------------------------


geom_nodeplot <- function(plot_call = "ggplot",
                          gglist = NULL,
                          width = 1,
                          height = 1,
                          ids = "terminal",
                          scales = "fixed",
                          x_nudge = 0,
                          y_nudge = 0,
                          shared_axes_labels = FALSE,
                          predict_arg = NULL) {

NULL
  ggplot2::layer(
    data = NULL,
    mapping = NULL,
    stat = "identity",
    geom = GeomNodeplot,
    position = position_nudge(x = x_nudge, y = y_nudge),
    params = list(
      gglist = gglist,
      plot_call = enquote(plot_call),
      width = width,
      height = height,
      ids = ids,
      scales = scales,
      shared_axes_labels = shared_axes_labels,
      predict_arg = predict_arg
    )
  )

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
                        ids,
                        shared_axes_labels,
                        scales,
                        predict_arg) {


    data <- coord$transform(data, panel_params)

    y_0 <- scales::rescale(0, from = panel_params$y.range)
    x_1 <- scales::rescale(1, from = panel_params$x.range)
    x_0 <- scales::rescale(0, from = panel_params$x.range)
    legend_x <- scales::rescale(0.5, from = panel_params$x.range)
    legend_y <- scales::rescale(-0.05, from = panel_params$y.range)
    lab_x <- scales::rescale(0.05, from = panel_params$x.range)

    # for vertical trees
    v_width <- abs(diff(data$x[data$kids == 0]))[1] * width
    v_height <- abs(diff(data$y[data$kids != 0]))[1] * height

    # for horizontal trees
    h_width <- abs(diff(data$x[data$kids != 0]))[1] * width
    h_height <- abs(diff(data$y[data$kids == 0]))[1] * height

    grob_list <- list()
    #if (any(is.na(ids))) ids <- unique(data$id)
    if (all(ids == "all")) ids <- unique(data$id)
    if (all(ids == "terminal")) ids <- unique(data$id[data$kids == 0])
    if (all(ids == "inner")) ids <- unique(data$id[data$kids != 0])


    #  transform data_* columns from lists to full dataframe -------------------

    #browser()
    nodeplot_data_lists <-  dplyr::select(data, dplyr::starts_with("data_"))
    names(nodeplot_data_lists) <- substring(names(nodeplot_data_lists), 6)
    lengths <- sapply(nodeplot_data_lists[[2]], function(x) length(unlist(x)))

    nodeplot_data <- data.frame(id = rep(data$id, times = lengths))



    for (column in names(nodeplot_data_lists)) {
      nodeplot_data[[column]] <- numeric(nrow(nodeplot_data))
      for (i in seq_along(data$id)) {
        rows <- nodeplot_data_lists[[column]][[i]]
        nodeplot_data[nodeplot_data$id == i, column] <- rows
        #nodeplot_data[column] <- content
      }
    }

    # add_fit -----------------------------------------------------------------


    # draw plots --------------------------------------------------------------


    base_data <- nodeplot_data
    #base_data <- add_prediction(info = data$info, data = base_data)



    facet_data <- base_data[nodeplot_data$id %in% ids, ]
    if (!is.null(predict_arg))
      predict_data <- predict_data(data$info, facet_data, predict_arg)

    # generate faceted base_plot
    browser()


    facet_gtable <- ggplotGrob(do.call(plot_call,
                                       args = list(data = facet_data)) +
                                 lapply(gglist, eval.parent, n = 1) +
                                 theme(legend.position = "bottom") +
                                 facet_wrap( ~ id, scales = scales, nrow = 1))


    lab <- list()
    if (shared_axes_labels & T) lab <- ylab(" ")

    base_gtable <- ggplotGrob(do.call(plot_call,
                                      args = list(data = base_data)) +
                                lapply(gglist, eval.parent, n = 1) +
                                lab +
                                theme(legend.position = "none"
                                      # ,
                                      # axis.title = eval(
                                      #   ifelse(shared_axes_labels,
                                      #          quote(element_blank()),
                                      #          quote(element_text())))
                                      #
                                      ))

    # get base_gtable's base_layout
    base_layout <- base_gtable$layout

    # legend ------------------------------------------------------------------

    if (any(facet_gtable$layout$name == "guide-box")) {
      # extract gtable containing legend

      legend_gtable <- reduce_gtable(facet_gtable, "guide-box")
      legend_gtable$layout$t <- 1
      legend_gtable$layout$b <- 1
      legend_gtable$heights <- unit(1,"pt")

      #call nodeplotGrob on legend_gtable
      legend_gtable <- nodeplotGrob(
        x = legend_x,
        y = y_0,
        width = 1,
        height = abs(legend_y - y_0),
        just = "top",
        node_gtable = legend_gtable
      )
      grob_list <- c(grob_list, list(legend_gtable))
    }
    # shared axes labels
    if (shared_axes_labels) {

      # x axis label
      if (any(facet_gtable$layout$name == "xlab-b")) {
        # extract gtable containing bottom x_axis

        xlab_gtable <- reduce_gtable(facet_gtable, "xlab-b")
        xlab_gtable$layout$t <- 1
        xlab_gtable$layout$b <- 1
        xlab_gtable$heights <- unit(1,"pt")

        #call nodeplotGrob on legend_gtable
        xlab_gtable <- nodeplotGrob(
          x = legend_x,
          y = y_0,
          width = 1,
          height = abs(legend_y - y_0),
          just = "bottom",
          node_gtable = xlab_gtable
        )
        grob_list <- c(grob_list, list(xlab_gtable))
      }

      # y axis label
      if (any(facet_gtable$layout$name == "ylab-l")) {
        # extract gtable containing bottom x_axis
        ylab_gtable <- reduce_gtable(facet_gtable, "ylab-l")
        ylab_gtable$layout$l <- 1
        ylab_gtable$layout$r <- 1
        ylab_gtable$layout$t <- 1
        ylab_gtable$layout$b <- 1
        ylab_gtable$widths <- unit(0,"pt")
        ylab_gtable$heights <- unit(0,"pt")

        #call nodeplotGrob on ylab_gtable
        ylab_gtable <- nodeplotGrob(
          y = (min(data$y) + y_0) * 0.5 + abs(legend_y - y_0),
          x = x_0,
          width = abs(lab_x - x_0),
          height = min(data$y),
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
      if(data$y[data$id == 1] == max(data$y)) {
        nodeplotGrob(
          x = x,
          y = ifelse(data$kids[data$id == ids[i]] == 0,
                     (y + y_0) * 0.5 + abs(legend_y - y_0),
                     y),
          width = v_width,
          height = ifelse(data$kids[data$id == ids[i]] == 0,
                          abs(y - y_0),
                          v_height),
          just = "center",
          node_gtable = node_gtable
        )
      } else { #horizontal tree
        nodeplotGrob(
          x = ifelse(data$kids[data$id == ids[i]] == 0,
                     (x + x_1) * 0.5,
                     x),
          y = y,
          width = ifelse(data$kids[data$id == ids[i]] == 0,
                         abs(x - x_1),
                         h_width),
          height = h_height,
          just = "center",
          node_gtable = node_gtable
        )
      }
    })
    #combine nodeplots and legend
    grob_list <- c(nodeplot_gtable, grob_list)
    class(grob_list) <- "gList"
    ggname("geom_nodeplots", grid::grobTree(children = grob_list))
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
remove_grob <- function (g, what = "guide-box") {
  matches <- c(grepl(pattern = what, g$layout$name))
  g$layout <- g$layout[!matches, , drop = FALSE]
  g$grobs <- g$grobs[!matches]
  return(g)
}

reduce_gtable <- function (g, what = "guide-box") {
  matches <- c(grepl(pattern = what, g$layout$name))
  g$layout <- g$layout[matches, , drop = FALSE]
  g$grobs <- g$grobs[matches]
  return(g)
}


ggname <- function (prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}

predict_data <- function(info, data, predict_arg) {
  ids <- unique(data$id)
  newdata_function <- predict_arg$newdata
  for (i in 1:length(ids)) {
    #browser()
    predict_arg$newdata <- do.call(newdata_function, list(data))
    predict_arg$object <- info[[ids[i]]]$object
    predict_data <- data.frame(newdata = predict_arg$newdata)
    predict_data$id <- ids[i]
    predict_data$prediction <- do.call(predict,# list(info$object, type = "quantile"))#,
            predict_arg)

    if (i == 1) resulting_data <- predict_data
    else resulting_data <- rbind(resulting_data, predict_data)
  }
  resulting_data
}



#   data$fit.v <- numeric(nrow(data))
#   for (i in ids) {
#     model <- info[[i]]$object
#     data$fit.v[data$id == i] <- predict(model)
#   }
#   data
# }


