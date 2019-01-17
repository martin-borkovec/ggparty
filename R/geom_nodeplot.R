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


# StatNodeplot <- ggproto("StatNodeplot", Stat,
#                     compute_group = function(data, scales = scales) {
#                       print(data$x)
#                       print(str(data))
#
#                       lengths <- sapply(data$x, function(x) length(unlist(x)))
#
#                       data.frame("x" = unname(unlist(data$x)),
#                                  "y" = unname(unlist(data$y)),
#                                  "xcord" = rep(data$xcord, lengths),
#                                  "ycord" = rep(data$ycord, lengths),
#                                  id = rep(1:nrow(data), times = lengths))
#
#
#                     },
#                     required_aes = c("x", "y", "xcord", "ycord", "id")
# )

GeomNodeplot <- ggproto("GeomNodeplot", Geom,
                      required_aes = c("x", "y", "id"),
                      draw_panel = function(self,
                                            data,
                                            panel_params,
                                            coord,
                                            parse = FALSE,
                                            na.rm = FALSE) {

                        print(panel_params)
                        print(coord)
                        panel_params$x.range <- c(0,1)
                        panel_params$y.range <- c(0,1)
                        print(panel_params)
                        data <- coord$transform(data, panel_params)


                        grobs <- lapply(1:max(data$id), function(i) {
                          node <- data[data$id == i, , drop = FALSE]
                          nodeplotGrob(
                                     x = node$x,
                                     xcord = node$xcord,
                                     y = node$y,
                                     ycord = node$ycord
                          )
                        })
                        class(grobs) <- "gList"

                        ggname("geom_nodeplot", grobTree(children = grobs))
                      }
)

nodeplotGrob <- function(x ,
                         y ,
                         xcord,
                         ycord) {


  r <- ggplotGrob(ggplot() + geom_point(aes(x,y)))
  r$vp <- viewport(x = 0.5, y = 0.5, width = 0.1, height = 0.1)
  return(r)
}




