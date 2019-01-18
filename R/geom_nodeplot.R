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
      same_axes_limits = same_axes_limits,
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
                        same_axes_limits) {

    data <- coord$transform(data, panel_params)
    if (same_axes_limits) {
      x <- list()
      x$node_data <- data[data$id == 1, , drop = FALSE]
      g <- eval(plot_call) +
        gglist
      xlim <- ggplot_build(g)$layout$panel_params[[1]]$x.range
      ylim <- ggplot_build(g)$layout$panel_params[[1]]$y.range
      gglist <- c(gglist,
                  list(xlim(xlim),
                       ylim(ylim)))
    }

    grobs <- lapply(1:max(data$id), function(i) {
      node_data <- data[data$id == i, , drop = FALSE]

      nodeplotGrob(
        x = node_data$x[1],
        y = node_data$y[1],
        node_data = node_data,
        gglist = gglist,
        plot_call = plot_call
      )
    })
    class(grobs) <- "gList"
    ggname("geom_nodeplot", grobTree(children = grobs))
  }
)

nodeplotGrob <- function(x, y, node_data, gglist, plot_call) {
  gTree(x = x,
        y = y,
        node_data = node_data,
        plot_call = plot_call,
        gglist = gglist,
        cl = "nodeplotgrob")
}


makeContent.nodeplotgrob <- function(x) {
  r <- ggplotGrob(
      eval(x$plot_call) +
      x$gglist
  )

  r$vp <- viewport(x = x$x, y = x$y, width = 0.1, height = 0.1)
  setChildren(x, gList(r))
}


