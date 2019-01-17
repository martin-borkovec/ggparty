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
                        parse = FALSE,
                        na.rm = FALSE,
                        gglist) {

    data <- coord$transform(data, panel_params)

    grobs <- lapply(1:max(data$id), function(i) {
      node_data <- data[data$id == i, , drop = FALSE]
      nodeplotGrob(
        x = node_data$x[1],
        y = node_data$y[1],
        node_data = node_data,
        gglist = gglist
      )
    })
    class(grobs) <- "gList"
    ggname("geom_nodeplot", grobTree(children = grobs))
  }
)

nodeplotGrob <- function(x, y, node_data, gglist) {
  gTree(x = x,
        y = y,
        node_data = node_data,
        gglist = gglist,
        cl = "nodeplotgrob")
}


makeContent.nodeplotgrob <- function(x) {
  r <- ggplotGrob(
    ggplot(data=x$node_data) +
      x$gglist
  )
  r$vp <- viewport(x = x$x, y = x$y, width = 0.1, height = 0.1)
  setChildren(x, gList(r))
}


