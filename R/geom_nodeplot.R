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
                          ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomNodeplot,
    position = position,
    show.legend = show.legend,
    inherit.aes = TRUE,
    params = list(na.rm = na.rm,
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
                        na.rm = FALSE) {

    data <- coord$transform(data, panel_params)

    grobs <- lapply(1:max(data$id), function(i) {
      node <- data[data$id == i, , drop = FALSE]
      nodeplotGrob(
        x = node$x,
        y = node$y,
        xdat = node$xdat,
        ydat = node$ydat
      )
    })
    class(grobs) <- "gList"
    ggname("geom_nodeplot", grobTree(children = grobs))
  }
)

nodeplotGrob <- function(x, y, xdat, ydat) {
  gTree(x = x,
        y = y,
        xdat = xdat,
        ydat = ydat,
        cl = "nodeplotgrob")
}


makeContent.nodeplotgrob <- function(x) {

  r <- ggplotGrob(ggplot() + geom_point(aes(x$xdat,x$ydat)))
  r$vp <- viewport(x = x$x[1], y = x$y[1], width = 0.1, height = 0.1)
  setChildren(x, gList(r))
}


