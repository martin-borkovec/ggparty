#' Draw labels containing node's info
#'
#' @inheritParams geom_edge
#' @inheritParams ggplot2::layer
#' @param line_list List of labels per line.
#' @param line_gpar List of graphical parameters per line.
#' @param parse If `TRUE`, the labels will be parsed into expressions.
#' @param label.col Border colour.
#' @param label.fill Background colour.
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param ids choose which nodes to label by their ids
#' @param ... additional arguments for [geom_label()]#'
#' @param label.padding Amount of padding around label. Defaults to 0.25 lines.
#' @param label.r Radius of rounded corners. Defaults to 0.15 lines.
#' @param label.size Size of label border, in mm.
#' @export
#' @md
geom_node_label <- function(mapping = NULL,
                            data = NULL,
                            line_list = NULL,
                            line_gpar = NULL,
                            ids = NULL,
                            position = "identity",
                            ...,
                            parse = FALSE,
                            nudge_x = 0,
                            nudge_y = 0,
                            label.padding = unit(0.25, "lines"),
                            label.r = unit(0.15, "lines"),
                            label.size = 0.25,
                            label.col = NULL,
                            label.fill = NULL,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {


  # input checks ------------------------------------------------------------

  if(is.null(mapping$lab)) {
    assert_list(line_gpar)
    assert_list(line_list, len = length(line_gpar), any.missing = FALSE)
    for (i in seq_along(line_gpar)) {
      assert_class(line_list[[i]], "uneval")
      assert_list(line_gpar[[i]], names = "unique")
    }
  }


  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("You must specify either `position` or `nudge_x`/`nudge_y`.", call. = FALSE)
    }

    position <- position_nudge(nudge_x, nudge_y)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = StatParty,
    geom = GeomNodeLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      line_list = line_list,
      parse = parse,
      label.padding = label.padding,
      label.r = label.r,
      label.size = label.size,
      label.fill = label.fill,
      label.col = label.col,
      na.rm = na.rm,
      line_gpar = line_gpar,
      ids = ids,
      ...
    )
  )
}

#' @rdname ggparty-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @md
GeomNodeLabel <- ggproto("GeomNodeLabel", Geom,
                         required_aes = c("x", "y"),

                         default_aes = aes(label = NULL,
                                           colour = "black", fill = "white", size = 3.88, angle = 0,
                                           alpha = NA, family = "", fontface = 1,
                                           lineheight = 1.2
                         ),

                         draw_panel = function(self, data, panel_params, coord, parse = FALSE,
                                               na.rm = FALSE,
                                               label.padding = unit(0.25, "lines"),
                                               label.r = unit(0.15, "lines"),
                                               label.size = 0.25,
                                               label.col = NULL,
                                               label.fill = NULL,
                                               line_list,
                                               line_gpar
                         ) {

                           on.exit(detach(data))
                           attach(data)
                           if (is.null(line_list)) lab <- data$label
                           else {
                             lab <- list()
                             for (j in seq_along(data$id)) {
                               labs <- character()
                               for (i in seq_along(line_list)) {
                                 labs[i] <- rlang::eval_tidy(line_list[[i]]$label)[j]
                                 if (i %in% parse) labs[i] <- ggplot2:::parse_safe(as.character(labs[i]))
                               }
                               lab[[j]] <- labs
                             }}

                           data <- coord$transform(data, panel_params)

                           y_shift <- NULL
                           box_up <- NULL
                           box_down <- NULL
                           if (!is.null(line_list)) {
                             # get vector telling textgrob how many lines to move y --------------------
                             y_shift <- get_y_shift(line_list)
                             # and how to move label border
                             box_up <- y_shift[[1]]
                             box_up[1] <- 1
                             box_down <- y_shift[[length(y_shift)]]
                             box_down[length(box_down)] <- -1
                           }


                           # get list of grobs ------------------------------------

                           grobs <- lapply(1:nrow(data), function(i) {
                             row <- data[i, , drop = FALSE]
                             text_gp <- list()
                             if (is.null(line_gpar))
                               text_gp <- list(
                                 gpar(
                                   col = row$colour,
                                   fontsize = row$size * .pt,
                                   fontfamily = row$family,
                                   fontface = row$fontface,
                                   lineheight = row$lineheight,
                                   fill = row$fill)
                               )
                             else
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
                                                       line_gpar[[j]]$lineheight),
                                   fill = ifelse(is.null(line_gpar[[j]]$fill),
                                                 row$fill,
                                                 line_gpar[[j]]$fill)
                                 )))
                               }

                             nodelabelGrob(lab[[i]],
                                           x = unit(row$x, "native"),
                                           y = unit(row$y, "native"),
                                           padding = label.padding,
                                           r = label.r,
                                           text.gp = text_gp,
                                           rect.gp = gpar(
                                             col = ifelse(is.null(label.col),
                                                          if (isTRUE(all.equal(label.size, 0))) NA else row$colour,
                                                          label.col),
                                             fill = ifelse(is.null(label.fill),
                                                           alpha(row$fill, row$alpha),
                                                           label.fill),
                                             lwd = label.size * .pt
                                           ),
                                           y_shift = y_shift,
                                           box_up = box_up,
                                           box_down = box_down
                             )
                           })
                           class(grobs) <- "gList"
                           ggname("geom_label", grobTree(children = grobs))
                         },
                         draw_key = draw_key_label
)

nodelabelGrob <- function(label,
                          x = unit(0.5, "npc"),
                          y = unit(0.5, "npc"),
                          padding = unit(0.25, "lines"),
                          r = unit(0.1, "snpc"),
                          default.units = "npc",
                          name = NULL,
                          text.gp = gpar(),
                          rect.gp = gpar(fill = "white"),
                          vp = NULL,
                          y_shift = y_shift,
                          box_up = box_up,
                          box_down = box_down) {

  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)

  gTree(label = label, x = x, y = y, padding = padding, r = r,
        name = name, text.gp = text.gp, rect.gp = rect.gp, vp = vp, y_shift = y_shift,
        box_up = box_up,
        box_down = box_down,
        cl = "nodelabelgrob")
}

#' @export
makeContent.nodelabelgrob <- function(x) {

  text_list <- list()

  # draw text grobs ---------------------------------------------------------

  for (i in seq_along(x$label)) {
    text_list <- c(text_list, list(textGrob(
      label = x$label[i],
      x = x$x,
      y = x$y + unit(sum(x$y_shift[[i]] * sapply(x$text.gp, line_size)),
                     "point"),
      gp = x$text.gp[[i]],
      name = paste0("text", i)))
    )

    #concatenate widths and heights of text grobs
    if (i == 1) widths <- grobWidth(text_list[[i]])
    else        widths <- unit.c(widths, grobWidth(text_list[[i]]))

    if (i == 1) heights <- grobHeight(text_list[[i]])
    else        heights <- unit.c(heights, grobHeight(text_list[[i]]))
  }


  # draw border ---------------------------------------------------------------

  r <- list(roundrectGrob(x = x$x,
                          y = x$y + unit(sum(
                            x$box_up * sapply(x$text.gp, line_size) +
                              x$box_down * sapply(x$text.gp, line_size)) / 2,
                            "point"),
                          default.units = "native",
                          width = max(widths) + 2 * x$padding,
                          height = unit(sum(sapply(x$text.gp, line_size)),
                                        "point") +  2 * x$padding,
                          r = x$r,
                          gp = x$rect.gp,
                          name = "box"
  ))

  grob_list <- c(r, text_list)
  class(grob_list) <- "gList"
  ggname("geom_nodelabels", grid::grobTree(children = grob_list))
}


# function that calculates the line shift for each row of labels ----------

get_y_shift <- function(label) {
  y_shift_list <- list()
  for (i in seq_along(label)) {
    y_shift <- rep(0, length(label))
    if(length(label) %% 2 == 0)
      for (j in seq_along(y_shift)) {
        if (j <= length(y_shift) / 2) {
          if (j == i) y_shift[j] <- 0.5
          if (j > i)  y_shift[j] <- 1
        }
        if (j > length(y_shift) / 2) {
          if (j == i) y_shift[j] <- -0.5
          if (j < i)  y_shift[j] <- -1
        }

      }
    else
      for (j in seq_along(y_shift)) {
        if (j < length(y_shift) / 2 + 0.5) {
          if (j == i) y_shift[j] <- 0.5
          if (j > i)  y_shift[j] <- 1
        }
        if (j == length(y_shift) / 2 + 0.5){
          if (j > i) y_shift[j] <- 0.5
          if (j == i) y_shift[j] <- 0
          if (j < i) y_shift[j] <- -0.5
        }
        if (j > length(y_shift) / 2 + 0.5) {
          if (j < i)  y_shift[j] <- -1
          if (j == i) y_shift[j] <- -0.5
        }
      }
    y_shift_list[[i]] <- y_shift
  }
  return(y_shift_list)
}


# calucaltes size of unit "lines" based on  fontsize and lineheight ------------

line_size <- function(x) x$fontsize * x$lineheight
