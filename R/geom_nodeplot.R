#' generate geom of nodeplot
#'
#' @param mapping mapping of nodeplots
#' @param gglist list of additional ggplot components
#' @param width,height size of the nodeplot's viewport
#' @param ids which ids to plot. numeric or string "terminal"
#' @param scales see [facet_wrap()]
#' @param xnudge,ynudge nudge nodeplot
#' @export
#' @md


# geom_nodeplot () --------------------------------------------------------


geom_nodeplot <- function(mapping = NULL,
                          data = NULL,
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = FALSE,
                          gglist = NULL,
                          plot_call = "ggplot",
                          width = 0.1,
                          height = 0.1,
                          ids = NA,
                          scales = "fixed",
                          xnudge = 0,
                          ynudge = 0,
                          ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomNodeplot,
    position = position,
    show.legend = show.legend,
    inherit.aes = TRUE,
    params = list(
      na.rm = na.rm,
      gglist = gglist,
      plot_call = enquote(plot_call),
      width = width,
      height = height,
      ids = ids,
      scales = scales,
      xnudge = xnudge,
      ynudge = ynudge,
      ...
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
                        na.rm = FALSE,
                        gglist,
                        plot_call,
                        width,
                        height,
                        ids,
                        xnudge,
                        ynudge,
                        scales) {

    data <- coord$transform(data, panel_params)
    grob_list <- list()
    if (any(is.na(ids))) ids <- unique(data$id)
    if (ids == "terminal") ids <- unique(data$id[data$kids == 0])


    #  transform data_* columns from lists to full dataframe -------------------

    nodeplot_data_lists <-  dplyr::select(data, dplyr::starts_with("data_"))
    names(nodeplot_data_lists) <- substring(names(nodeplot_data_lists), 6)
    lengths <- sapply(nodeplot_data_lists[[1]], function(x) length(unlist(x)))
    nodeplot_data <- data.frame(id = rep(1:nrow(data), times = lengths))


    for (column in names(nodeplot_data_lists)) {
      content <- numeric(0)
      for (id in data$id) {
        rows <- nodeplot_data_lists[[column]][[id]]
        content <- rbind(content, rows)
      }
      nodeplot_data[column] <- content
    }


    # draw plots --------------------------------------------------------------


    base_data <- nodeplot_data[nodeplot_data$id == 1, ]
    facet_data <- nodeplot_data[nodeplot_data$id %in% ids, ]
    # generate faceted base_plot

    facet_gtable <- ggplotGrob(do.call(plot_call, args = list(data = facet_data))+
                                 gglist +
                                 theme(legend.position = "bottom") +
                                 facet_wrap( ~ id, scales = scales, nrow = 1))

    base_gtable <- ggplotGrob(do.call(plot_call, args = list(data = base_data))+
                                gglist +
                                theme(legend.position = "none"))

    # get base_gtable's base_layout
    base_layout <- base_gtable$layout

    # legend ------------------------------------------------------------------

    if (any(facet_gtable$layout$name == "guide-box")) {
      # extract gtable containing legend

      legend_gtable <- reduce_gtable(facet_gtable, "guide-box")

      #call nodeplotGrob on legend_gtable
      legend_gtable <- nodeplotGrob(
        x = 0.5,
        y = min(data$y) - 0.1,
        width = width,
        height = height,
        node_gtable = legend_gtable
      )
      grob_list <- c(grob_list, list(legend_gtable))
    }


    # nodeplots ---------------------------------------------------------------


    # iterate through all ids to get all nodeplots
    nodeplot_gtable <- lapply(seq_along(ids), function(i) {

      # panel ------------------------------------------------------------------

      # create node_gtable as copyo of base_gtable
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
      #browser()
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

      # get x any y coords of nodeplot
      x <- unique(data[data$id == ids[i], "x"])
      y <- unique(data[data$id == ids[i], "y"])
      nodeplotGrob(
        x = x + xnudge,
        y = y + ynudge,
        width = width,
        height = height,
        node_gtable = node_gtable
      )
    })
    #combine nodeplots and legend
    grob_list <- c(grob_list, nodeplot_gtable)
    class(grob_list) <- "gList"
    ggname("geom_nodeplots", grobTree(children = grob_list))
  }
)

nodeplotGrob <- function(x, y, node_gtable, width, height) {
  gTree(x = x,
        y = y,
        node_gtable = node_gtable,
        width = width,
        height = height,
        cl = "nodeplotgrob")
}


makeContent.nodeplotgrob <- function(x) {
  r <- x$node_gtable
  r$vp <- viewport(x = x$x, y = x$y, width = x$width, height = x$height)
  setChildren(x, gList(r))
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
