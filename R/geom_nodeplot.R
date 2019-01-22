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
      same_axes_limits = same_axes_limits,
      shared_legend = shared_legend,
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
                        same_axes_limits,
                        shared_legend) {

    data <- coord$transform(data, panel_params)

    # same axes limits --------------------------------------------------------
    # generate ggplot object
    x <- list()
    x$node_data <- data[data$id == 1, , drop = FALSE]
    base_plot <- eval(plot_call) + gglist

    if (same_axes_limits) {
      xlim <- ggplot_build(base_plot)$layout$panel_params[[1]]$x.range
      ylim <- ggplot_build(base_plot)$layout$panel_params[[1]]$y.range
      gglist <- c(gglist,
                  list(xlim(xlim),
                       ylim(ylim)))
    }

    if (shared_legend) {
      base_grob <- ggplotGrob(base_plot)$grobs

      legend <- base_grob[[which(sapply(base_grob, function(x) x$name) == "guide-box")]]
      legend$vp <- viewport(x = 0.1, y = 0.9, width = 1, height = 1)


      base_data  <- layer_data(base_plot)
      cols <- base_data[["colour"]]
      names(cols) <- rownames(base_data)

      gglist <- c(gglist,
                  list(theme(legend.position="none"),
                       quote(scale_color_manual(values = unique(x$cols[names(x$cols) %in%
                                                                         rownames(x$node_data)])))
                  )
      )

      # lheight <- sum(legend$height)
      # lwidth <- sum(legend$width)
    }

    grobs <- lapply(1:max(data$id), function(i) {

      node_data <- data[data$id == i, ]
      nodeplotGrob(
        x = node_data$x[1],
        y = node_data$y[1],
        node_data = node_data,
        gglist = gglist,
        plot_call = plot_call,
        cols = cols
      )
    })

    grobs <- c(grobs, list(legend))
    class(grobs) <- "gList"
    ggname("geom_nodeplot", grobTree(children = grobs))
  }
)

nodeplotGrob <- function(x, y, node_data, gglist, plot_call, cols) {

  gTree(x = x,
        y = y,
        node_data = node_data,
        plot_call = plot_call,
        gglist = gglist,
        cols = cols,
        cl = "nodeplotgrob")
}


makeContent.nodeplotgrob <- function(x) {
  browser()
  r <- ggplotGrob(
      eval(x$plot_call) +
      x$gglist[1:4] + eval(x$gglist[[5]])
  )

  r$vp <- viewport(x = x$x, y = x$y, width = 0.1, height = 0.1)
  setChildren(x, gList(r))
}

# eval(x$plot_call) +
#   geom_point(aes(colour = play, size = temperature)) +
#   scale_color_manual(values = unique(x$cols[names(x$cols) %in%
#                                               rownames(x$node_data)]))
#   scale_color_manual(values = c("red", "blue"))
