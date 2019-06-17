#' Draw (multi-line) labels at nodes
#'
#' `geom_node_splitvar()` and `geom_node_info()` are simplified versions of
#' `geom_node_label()` with the respective defaults to either label the split variables
#' for all inner nodes or the info for all terminal nodes.
#'
#' `geom_node_label()` is a modified version of [ggplot2::geom_label()]. This
#' modification allows for labels with multiple lines and line specific graphical
#' parameters.
#'
#' @inheritParams ggplot2::layer
#' @param mapping `x` and `y` are mapped per default to the node's coordinates. If
#' you don't want to set line specific graphical parameters, you can also map
#' `label` here. Otherwise set `labels` in `line_list`.
#' @param line_list Use this only if you want a multi-line label with the
#' possibility to override the aesthetics mapping for each line specifically
#' with fixed graphical parameters. In this case, don't map anything to
#'  `label` in the `aes()` supplied to `mapping`
#' , but instead pass here a list of `aes()` with the **only** mapped variable
#'  in each
#' being `label`.
#' Other aesthetic mappings still can be passed to `mapping` and will apply to
#' all lines and the border, unless overwritten by `line_gpar`.
#' The order of the list represents the order of the plotted lines.
#' @param line_gpar List of lists containing line-specific graphical parameters.
#'  Only use in
#' conjunction with `line_list`. Has to contain the same number of lists as are
#' `aes()` in `line_list`. First list applies to first line, and so on.
#' @param parse If `TRUE`, the labels will be parsed into expressions. Can also
#' be specified per line via `line_gpar`.
#' @param label.col Border colour.
#' @param label.fill Background colour.
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param ids Select for which nodes to draw a label. Can be `"inner"`, `"terminal"`,
#'  `"all"` or `numeric` vector of ids.
#' @param label.padding Amount of padding around label. Defaults to 0.25 lines.
#' @param label.r Radius of rounded corners. Defaults to 0.15 lines.
#' @param label.size Size of label border, in mm.
#' @param nudge_x,nudge_y Adjust position of label.
#' @param ... Additional arguments to layer.
#' @seealso [ggparty()]
#' @export
#' @md
#' @examples
#' library(ggparty)
#' data("WeatherPlay", package = "partykit")
#' sp_o <- partysplit(1L, index = 1:3)
#' sp_h <- partysplit(3L, breaks = 75)
#' sp_w <- partysplit(4L, index = 1:2)
#' pn <- partynode(1L, split = sp_o, kids = list(
#'   partynode(2L, split = sp_h, kids = list(
#'     partynode(3L, info = "yes"),
#'     partynode(4L, info = "no"))),
#'   partynode(5L, info = "yes"),
#'   partynode(6L, split = sp_w, kids = list(
#'     partynode(7L, info = "yes"),
#'     partynode(8L, info = "no")))))
#' py <- party(pn, WeatherPlay)
#'
#' ggparty(py) +
#'   geom_edge() +
#'   geom_edge_label() +
#'   geom_node_label(aes(label = splitvar),
#'                   ids = "inner") +
#'   geom_node_label(aes(label = info),
#'                   ids = "terminal")
#'
#'######################################
#'
#'data("TeachingRatings", package = "AER")
#'tr <- subset(TeachingRatings, credits == "more")
#'
#'tr_tree <- lmtree(eval ~ beauty | minority + age + gender + division + native +
#'                    tenure, data = tr, weights = students, caseweights = FALSE)
#'
#' data("TeachingRatings", package = "AER")
#' tr <- subset(TeachingRatings, credits == "more")
#'
#' tr_tree <- lmtree(eval ~ beauty | minority + age + gender + division + native +
#'                     tenure, data = tr, weights = students, caseweights = FALSE)
#'
#' ggparty(tr_tree,
#'         terminal_space = 0.5,
#'         add_vars = list(p.value = "$node$info$p.value")) +
#'   geom_edge(size = 1.5) +
#'   geom_edge_label(colour = "grey", size = 6) +
#'   geom_node_plot(gglist = list(geom_point(aes(x = beauty,
#'                                               y = eval,
#'                                               col = tenure,
#'                                               shape = minority),
#'                                           alpha = 0.8),
#'                                theme_bw(base_size = 15)),
#'                  scales = "fixed",
#'                  id = "terminal",
#'                  shared_axis_labels = TRUE,
#'                  shared_legend = TRUE,
#'                  legend_separator = TRUE,
#'                  predict = "beauty",
#'                  predict_gpar = list(col = "blue",
#'                                     size = 1.2)
#'   ) +
#'   geom_node_label(aes(col = splitvar),
#'                   line_list = list(aes(label = paste("Node", id)),
#'                                    aes(label = splitvar),
#'                                    aes(label = paste("p =", formatC(p.value,
#'                                     format = "e", digits = 2)))),
#'                   line_gpar = list(list(size = 12, col = "black", fontface = "bold"),
#'                                    list(size = 20),
#'                                    list(size = 12)),
#'                   ids = "inner") +
#'   geom_node_label(aes(label = paste0("Node ", id, ", N = ", nodesize)),
#'                   fontface = "bold",
#'                   ids = "terminal",
#'                   size = 5,
#'                   nudge_y = 0.01) +
#'   theme(legend.position = "none")

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

  if (is.null(mapping$lab)) {
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
                           # check if parse line specific
                           parse <- rep(parse, length(line_list))
                           for (i in seq_along(line_list))
                             if (!is.null(line_gpar[[i]]$parse))
                               parse[i] <- line_gpar[[i]]$parse

                           #get labels for each line
                           lab <- get_labs(data, line_list, parse)

                           data <- coord$transform(data, panel_params)

                           y_shift <- NULL
                           box_up <- NULL
                           box_down <- NULL
                           if (!is.null(line_list)) {
                             # get vector telling textgrob how many lines to move
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
                             alignment <- rep.int(list("center"),
                                                  max(1, length(line_gpar)))
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

                                 if (!is.null(line_gpar[[j]]$alignment))
                                   alignment[[j]] <- line_gpar[[j]]$alignment
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
                                           alignment = alignment,
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
                          y_shift = 1,
                          alignment = "center",
                          box_up = 0,
                          box_down = 0) {

  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)

  gTree(label = label, x = x, y = y, padding = padding, r = r,
        name = name, text.gp = text.gp, rect.gp = rect.gp, vp = vp, y_shift = y_shift,
        box_up = box_up,
        alignment = alignment,
        box_down = box_down,
        cl = "nodelabelgrob")
}

#' @export
makeContent.nodelabelgrob <- function(x) {

  text_list <- list()

  # draw text grobs ---------------------------------------------------------
  for (i in seq_along(x$label)) {
    text_list <- c(text_list, list(textGrob(
      label = x$label[[i]],
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

  for (i in seq_along(x$label)) {
    if (x$alignment[[i]] == "left")
      text_list[[i]] <- textGrob(
        label = x$label[i],
        x = x$x - max(widths) * 0.5,
        y = x$y + unit(sum(x$y_shift[[i]] * sapply(x$text.gp, line_size)),
                       "point"),
        gp = x$text.gp[[i]],
        just = "left",
        name = paste0("text", i))
    if (x$alignment[[i]] == "right")
      text_list[[i]] <- textGrob(
        label = x$label[i],
        x = x$x + max(widths) * 0.5,
        y = x$y + unit(sum(x$y_shift[[i]] * sapply(x$text.gp, line_size)),
                       "point"),
        gp = x$text.gp[[i]],
        just = "right",
        name = paste0("text", i))
}




  # draw border ---------------------------------------------------------------
  r <- list(roundrectGrob(x = x$x,
                          y = x$y + unit(sum(
                            x$box_up * sapply(x$text.gp, line_size) +
                              x$box_down * sapply(x$text.gp, line_size)) / 2,
                            "point"),
                          default.units = "native",
                          width = max(widths) + 2 * x$padding,
                          height =
                          {if (length(x$label) == 1) grobHeight(text_list[[1]])
                            else unit(sum(sapply(x$text.gp, line_size)),"point")}
                          +  2 * x$padding,
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
    if (length(label) %% 2 == 0)
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
        if (j == length(y_shift) / 2 + 0.5) {
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


# caluclates size of unit "lines" based on fontsize and lineheight ------------

line_size <- function(x) x$fontsize * x$lineheight


# geom_node_info ----------------------------------------------------------

#' @rdname geom_node_label
#' @export
#'
geom_node_info <- function(mapping = NULL, nudge_x = 0, nudge_y = 0, ids = NULL,
                           label.padding = unit(0.5, "lines"), ...) {
  default_mapping <- aes(label = !!sym("info"))
  mapping <- adjust_mapping(default_mapping, mapping)
  layer(
    data = NULL,
    mapping = mapping,
    stat = StatParty,
    geom = "label",
    position = position_nudge(x = nudge_x, y = nudge_y),
    inherit.aes = TRUE,
    params = list(ids = ids,
                  label.padding = label.padding,
                  na.rm = TRUE,
                  ...)
  )
}



# geom_node_splitvar ------------------------------------------------------

#' @rdname geom_node_label
#' @export
#'
geom_node_splitvar <- function(mapping = NULL, nudge_x = 0, nudge_y = 0,
                               label.padding = unit(0.5, "lines"), ids = NULL,
                               ...) {
  default_mapping <- aes(label = !!sym("splitvar"))
  mapping <- adjust_mapping(default_mapping, mapping)
  layer(
    data = NULL,
    mapping = mapping,
    stat = StatParty,
    geom = "label",
    position = position_nudge(x = nudge_x, y = nudge_y),
    inherit.aes = TRUE,
    params = list(ids = ids,
                  label.padding = label.padding,
                  na.rm = TRUE,
                  ...)
  )
}


get_labs <- function(data, line_list, parse) {
  if (is.null(line_list)) lab <- data$label
  else {
    lab <- list()
    for (j in seq_along(data$id)) {
      labs <- list()
      # on.exit(detach(data[j, ]))
      # attach(data[j, ], warn.conflicts = FALSE)
      for (i in seq_along(line_list)) {
        # browser()
        evaluated <- rlang::eval_tidy(line_list[[i]]$label, data = data[j, ])
        # in case mapped to string
        if (length(evaluated) == 1) labs[i] <- evaluated
        else labs[[i]] <- evaluated[j]
        if (parse[i]) labs[[i]] <- parse_safe(as.character(labs[i]))
      }
      lab[[j]] <- labs
    }}
  lab
}


# parse safe (from ggplot2) -----------------------------------------------


parse_safe <- function(text)
{
  stopifnot(is.character(text))
  out <- vector("expression", length(text))
  for (i in seq_along(text)) {
    expr <- parse(text = text[[i]])
    out[[i]] <- if (length(expr) == 0)
      NA
    else expr[[1]]
  }
  out
}


