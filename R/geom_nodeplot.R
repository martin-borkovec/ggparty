library(ggplot2)
library(grid)
library(stringi)

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
                          plot_call = NULL,
                          same_axes_limits = TRUE,
                          shared_legend = FALSE,
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
                        same_axes_limit,
                        shared_legend) {

    data <- coord$transform(data, panel_params)

    # same axes limits --------------------------------------------------------
    # generate ggplot object

    # generate faceted base_plot
    base_plot <- eval(plot_call) + gglist + facet_grid( ~ id)
    # get its base_gtable
    base_gtable <- ggplotGrob(base_plot)
    # get base_gtable's base_layout
    base_layout <- base_gtable$layout
    # extract gtable containing legend
    legend_gtable <- base_gtable[, base_layout$l[base_layout$name == "guide-box"]]
    # get layout of xlab from base_layout
    xlab_layout <- base_layout[base_layout$name == "xlab-b", ]
    # extract individual grobs from base_gtable
    base_grobs <- base_gtable$grobs
    # determine index of grob of xlab
    f <- function(x) substring(x$name, first = 1, last = 12) == "axis.title.x"
    xlab_index <- which(sapply(base_grobs, f))
    # extract grob of xlab
    xlab_grob <- base_grobs[[xlab_index]]

    # call nodeplotGrob on legend_gtable
    legend_gtable <- nodeplotGrob(
      x = min(data$x),
      y = data$y[1],
      node_gtable = legend_gtable
    )

    # iterate through all ids to get all nodeplots
    grobs <- lapply(1:max(data$id), function(i) {
      # generate vector of all panel names except the ith
      panels <- paste0("panel-", unique(data$id)[-i],"-1")
      # drop all panels except the ith
      node_gtable <- base_gtable[, -base_gtable$layout$l[base_gtable$layout$name %in% panels]]
      # drop legend
      node_gtable <- node_gtable[, -node_gtable$layout$l[node_gtable$layout$name == "guide-box"]]
      # add xlab
      node_gtable <- gtable_add_grob(node_gtable,
                                   xlab_grob,
                                   t = xlab_layout$t,
                                   l= xlab_layout$l,
                                   r = max(node_gtable$layout$r),
                                   name = "xlab-b")
      # get x and y coordinates of node
      x <- unique(data[data$id == i, "x"])
      y <- unique(data[data$id == i, "y"])
      nodeplotGrob(
        x = x,
        y = y,
        node_gtable = node_gtable
      )
    })
    #combine nodeplots and legend
    grobs <- c(grobs, list(legend_gtable))
    class(grobs) <- "gList"
    ggname("geom_nodeplots", grobTree(children = grobs))
  }
)

nodeplotGrob <- function(x, y, node_gtable) {

  gTree(x = x,
        y = y,
        node_gtable = node_gtable,
        cl = "nodeplotgrob")
}


makeContent.nodeplotgrob <- function(x) {
  r <- x$node_gtable
  r$vp <- viewport(x = x$x, y = x$y, width = 0.1, height = 0.1)
  setChildren(x, gList(r))
}
