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
                        height = height) {

    data <- coord$transform(data, panel_params)
    grob_list <- list()

    # base data (root node data)
    base_data <- data[data$id == 1, ]
    # generate faceted base_plot
    facet_gtable <- ggplotGrob(eval(plot_call) + gglist + facet_grid( ~ id))
    base_gtable <- ggplotGrob(ggplot(base_data)+ gglist) # + facet_grid( ~ id))

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
    nodeplot_gtable <- lapply(1:max(data$id), function(i) {
      # create node_gtable as copyo of base_gtable
      node_gtable <- base_gtable
      # get index of panel grob in base_gtable
      base_panel_index <- find_grob(base_gtable$grobs, "panel")
      # get index of ith panel in facet_gtable
      panel_i <- paste0("panel-", unique(data$id)[i],".")
      facet_panel_index <- find_grob(facet_gtable$grobs, panel_i)
      # replace panel grob in node_gtable with ith panel grob of facet_gtable
      node_gtable$grobs[[base_panel_index]] <- facet_gtable$grobs[[facet_panel_index]]
      # get x any y coords of nodeplot
      x <- unique(data[data$id == i, "x"])
      y <- unique(data[data$id == i, "y"])
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
