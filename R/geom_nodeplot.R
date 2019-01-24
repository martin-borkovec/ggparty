library(ggplot2)
library(grid)
library(gtable)

ggname <- function (prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}

geom_nodeplot <- function(mapping = NULL,
                          data = NULL,
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = FALSE,
                          gglist = NULL,
                          plot_call = call("ggplot", data = quote(data)),
                          width = 0.1,
                          height = 0.1,
                          ids = NA,
                          scales = "fixed",
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
      ...
    )
  )
}

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
                        width = width,
                        height = height,
                        ids = ids,
                        scales = scales) {

    data <- coord$transform(data, panel_params)
    grob_list <- list()
    if (any(is.na(ids))) ids <- unique(data$id)
    if (ids == "terminal") ids <- unique(data$id[data$kids == 0])
    # base data (root node data)
    base_data <- data[data$id == 1, ]
    facet_data <- data[data$id %in% ids, ]

    # generate faceted base_plot
    facet_gtable <- ggplotGrob(ggplot(facet_data) +
                                 gglist +
                                 facet_wrap( ~ id, scales = scales, nrow = 1))
    base_gtable <- ggplotGrob(ggplot(base_data) + gglist) # + facet_grid( ~ id))

    # get base_gtable's base_layout
    base_layout <- base_gtable$layout

    if (any(base_layout$name == "guide-box")) {
      # extract gtable containing legend
      legend_gtable <- base_gtable[, base_layout$l[base_layout$name == "guide-box"]]
      base_gtable <- base_gtable[, -base_layout$l[base_layout$name == "guide-box"]]
      #call nodeplotGrob on legend_gtable
      legend_gtable <- nodeplotGrob(
        x = min(data$x),
        y = data$y[1],
        width = width,
        height = height,
        node_gtable = legend_gtable
      )
      grob_list <- c(grob_list, list(legend_gtable))
    }


    # iterate through all ids to get all nodeplots
    nodeplot_gtable <- lapply(seq_along(ids), function(i) {
      #browser()
      # create node_gtable as copyo of base_gtable
      node_gtable <- base_gtable

      # get index of panel grob in base_gtable
      base_panel_index <- find_grob(base_gtable$grobs, "panel")
      # get index of ith panel in facet_gtable
      panel_i <- paste0("panel-", i,".")
      facet_panel_index <- find_grob(facet_gtable$grobs, panel_i)
      # replace panel grob in node_gtable with ith panel grob of facet_gtable
      node_gtable$grobs[[base_panel_index]] <- facet_gtable$grobs[[facet_panel_index]]

      # get index of panel grob in base_gtable
      base_axis_index <-  which(base_gtable$layout$name == "axis-l")
      # get index of ith panel in facet_gtable
      if (scales == "fixed") {
      facet_axis_index <-  which(facet_gtable$layout$name == "axis-l-1-1")
      } else {
        axis_i <- paste0("axis-l-1-", i)
        facet_axis_index <-  which(facet_gtable$layout$name == axis_i)
      }

      # facet_panel_index <- find_grob(facet_gtable$grobs, panel_i)
      # replace panel grob in node_gtable with ith panel grob of facet_gtable
      node_gtable$grobs[[base_axis_index]] <- facet_gtable$grobs[[facet_axis_index]]

      # get x any y coords of nodeplot
      x <- unique(data[data$id == ids[i], "x"])
      y <- unique(data[data$id == ids[i], "y"])
      nodeplotGrob(
        x = x,
        y = y,
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
