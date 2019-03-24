#' @export
#' @rdname geom_text
#' @param label.padding Amount of padding around label. Defaults to 0.25 lines.
#' @param label.r Radius of rounded corners. Defaults to 0.15 lines.
#' @param label.size Size of label border, in mm.
geom_node_label <- function(mapping = NULL,
                            data = NULL,
                            aes_list,
                            line_gpar = NULL,
                            stat = "identity",
                            position = "identity",
                            ...,
                            parse = FALSE,
                            nudge_x = 0,
                            nudge_y = 0,
                            label.padding = unit(0.25, "lines"),
                            label.r = unit(0.15, "lines"),
                            label.size = 0.25,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {

  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("You must specify either `position` or `nudge_x`/`nudge_y`.", call. = FALSE)
    }

    position <- position_nudge(nudge_x, nudge_y)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomNodeLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      aes_list = aes_list,
      parse = parse,
      label.padding = label.padding,
      label.r = label.r,
      label.size = label.size,
      na.rm = na.rm,
      line_gpar = line_gpar,
      ...
    )
  )
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomNodeLabel <- ggproto("GeomNodeLabel", Geom,
                     required_aes = c("x", "y"),

                     default_aes = aes(
                       colour = "black", fill = "white", size = 3.88, angle = 0,
                       hjust = 0.5, vjust = 0.5, alpha = NA, family = "", fontface = 1,
                       lineheight = 1.2
                     ),

                     draw_panel = function(self, data, panel_params, coord, parse = FALSE,
                                           na.rm = FALSE,
                                           label.padding = unit(0.25, "lines"),
                                           label.r = unit(0.15, "lines"),
                                           label.size = 0.25,
                                           aes_list,
                                           line_gpar) {
                       #lab <- data$label


                       attach(data)
                       lab <- list()
                       for (j in seq_along(data$id)) {
                         labs <- character()
                         for (i in seq_along(aes_list)) {
                           labs[i] <- rlang::eval_tidy(aes_list[[i]]$label)[j]

                         }
                         lab[[j]] <- labs

                       }
                       detach(data)

                       # if (parse) {
                       #   lab <- parse_safe(as.character(lab))
                       # }

                       data <- coord$transform(data, panel_params)
                       if (is.character(data$vjust)) {
                         data$vjust <- compute_just(data$vjust, data$y)
                       }
                       if (is.character(data$hjust)) {
                         data$hjust <- compute_just(data$hjust, data$x)
                       }






                       grobs <- lapply(1:nrow(data), function(i) {
                         row <- data[i, , drop = FALSE]
                         # browser()
                         text_gp <- list()
                         for (j in seq_along(line_gpar)) {

                         text_gp <- c(text_gp, list(gpar(
                           col = ifelse(is.null(line_gpar[[j]]$col),
                                        row$colour,
                                        line_gpar[[j]]$col),
                           fontsize = ifelse(is.null(line_gpar[[j]]$size),
                                             row$size * .pt,
                                             line_gpar[[j]]$size),
                           fontfamily = ifelse(is.null(line_gpar[[j]]$family),
                                               row$family,
                                               line_gpar[[j]]$family),
                           fontface = ifelse(is.null(line_gpar[[j]]$fontface),
                                             row$fontface,
                                             line_gpar[[j]]$fontface),
                           lineheight = ifelse(is.null(line_gpar[[j]]$lineheight),
                                               row$lineheight,
                                               line_gpar[[j]]$lineheight)
                           )))
                         }



                         nodelabelGrob(lab[[i]],
                                   x = unit(row$x, "native"),
                                   y = unit(row$y, "native"),
                                   just = c(row$hjust, row$vjust),
                                   padding = label.padding,
                                   r = label.r,
                                   text.gp = text_gp,
                                   rect.gp = gpar(
                                     col = if (isTRUE(all.equal(label.size, 0))) NA else row$colour,
                                     fill = alpha(row$fill, row$alpha),
                                     lwd = label.size * .pt
                                   )
                         )
                       })
                       class(grobs) <- "gList"

                       ggname("geom_label", grobTree(children = grobs))
                     },

                     draw_key = draw_key_label
)

nodelabelGrob <- function(label, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                      just = "center", padding = unit(0.25, "lines"), r = unit(0.1, "snpc"),
                      default.units = "npc", name = NULL,
                      text.gp = gpar(), rect.gp = gpar(fill = "white"), vp = NULL) {

  #stopifnot(length(label) == 1)

  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)

  gTree(label = label, x = x, y = y, just = just, padding = padding, r = r,
        name = name, text.gp = text.gp, rect.gp = rect.gp, vp = vp, cl = "nodelabelgrob")
}

#' @export
makeContent.nodelabelgrob <- function(x) {
  hj <- resolveHJust(x$just, NULL)
  vj <- resolveVJust(x$just, NULL)

  #browser()
y_shift <- unit(c(1, 0, -1), "lines")

  text_list <- list()
# browser()
  for (i in seq_along(x$label)) {
    text_list <- c(text_list, list(textGrob(
      x$label[i],
      x$x + 2 * (0.5 - hj) * x$padding,
      x$y + 2 * (0.5 - vj) * x$padding + y_shift[i] ,
      just = c(hj, vj),
      gp = x$text.gp[[i]],
      name = paste0("text", i))))

    if (i == 1) widths <- grobWidth(text_list[[i]])
    else        widths <- unit.c(widths, grobWidth(text_list[[i]]))

    if (i == 1) heights <- grobHeight(text_list[[i]])
    else        heights <- unit.c(heights, grobHeight(text_list[[i]]))


  }


  r <- list(roundrectGrob(x$x, x$y, default.units = "native",
                     width = max(widths) + 2 * x$padding,
                     height = sum(heights) + unit(1, "line") +  2 * x$padding,
                     just = c(hj, vj),
                     r = x$r,
                     gp = x$rect.gp,
                     name = "box"
  ))

  grob_list <- c(r, text_list)
  class(grob_list) <- "gList"
  ggname("geom_nodelabels", grid::grobTree(children = grob_list))

  #setChildren(x, gList(r, t1, t2))
}
