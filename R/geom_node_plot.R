#' Generate geom of node plot
#'
#' @param plot_call Any function that generates a ggplot2 object.
#' @param gglist List of additional ggplot components. Data of nodes can be
#'  mapped. Fitted values of modelparty objects can be mapped with "fitted_values".
#' @param width Expansion factor for viewport's width
#' @param height Expansion factor for viewport's height
#' @param size Expansion factor for viewport's size
#' @param ids Id's to plot. Numeric, "terminal", "inner" or "all". Defaults
#' to "terminal"
#' @param scales see [facet_wrap()]
#' @param nudge_x,nudge_y Nudges node plot
#' @param shared_axis_labels If TRUE only one pair of axes labels is plotted in
#'  the terminal space. Only recommended if `ids`  "terminal" or "all".
#' @param predict_arg Named list containing arguments to be passed to call of
#' predict on node$info$object. Caution: newdata has to be a function with
#' a single argument, i.e. the ggplot data whose result will be used for the predict call as
#' the newdata argument.
#' Predictions and newdata will be stored in `predict_data` and
#' can be accessed via geoms within gglist. These geoms need to be expressions to ensure
#' correct evaluation. See examples.
#' @param legend_separator If 'TRUE' line between legend and tree is drawn.
#'
#' @import checkmate
#'
#' @export
#' @seealso [ggparty()]
#' @examples
#'
#' library(ggparty)
#'
#' airq <- subset(airquality, !is.na(Ozone))
#' airct <- ctree(Ozone ~ ., data = airq)
#'
#' ggparty(airct, horizontal = TRUE, terminal_space = 0.6) +
#'   geom_edge() +
#'   geom_edge_label() +
#'   geom_node_splitvar() +
#'   geom_node_plot(gglist = list(
#'     geom_density(aes(x = Ozone))),
#'     shared_axis_labels = TRUE)
#'
#' #############################################################
#'
#' ## Demand for economics journals data
#' data("Journals", package = "AER")
#' Journals <- transform(Journals,
#'                       age = 2000 - foundingyear,
#'                       chars = charpp * pages)
#'
#' ## linear regression tree (OLS)
#' j_tree <- lmtree(log(subs) ~ log(price/citations) | price + citations +
#'                    age + chars + society, data = Journals, minsize = 10, verbose = TRUE)
#'
#' ## Plot with ggparty
#'
#' ggparty(j_tree, terminal_space = 0.8) +
#'   geom_edge() +
#'   geom_edge_label() +
#'   geom_node_splitvar() +
#'   geom_node_plot(gglist =
#'                   list(aes(x = `log(price/citations)`, y = `log(subs)`),
#'                        geom_point(),
#'                        expression(geom_line(data = predict_data,
#'                                             aes(x = log(price/citations),
#'                                                 y = prediction),
#'                                             col = "red"))),
#'                 predict_arg = list(newdata = function(x) {
#'                   data.frame(
#'                     citations = 1,
#'                     price = exp(seq(from = min(x$`log(price/citations)`),
#'                                     to = max(x$`log(price/citations)`),
#'                                     length.out = 100)))
#'                 })
#'   )
#'
#' #########################################################################
#'
#' data("GBSG2", package = "TH.data")
#' GBSG2$time <- GBSG2$time/365
#'
#' library("survival")
#' wbreg <- function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
#'   survreg(y ~ 0 + x, weights = weights, dist = "weibull", ...)
#' }
#'
#'
#' logLik.survreg <- function(object, ...)
#'   structure(object$loglik[2], df = sum(object$df), class = "logLik")
#'
#' gbsg2_tree <- mob(Surv(time, cens) ~ horTh + pnodes | age + tsize +
#'                     tgrade + progrec + estrec + menostat, data = GBSG2,
#'                   fit = wbreg, control = mob_control(minsize = 80))
#'
#' # horizontal Tree with individual axis labes
#' ggparty(gbsg2_tree, terminal_space = 0.8, horizontal = TRUE) +
#'   geom_edge() +
#'   geom_node_splitvar() +
#'   geom_edge_label() +
#'   geom_node_plot(
#'     gglist = list(geom_point(aes(y = `Surv(time, cens).time`,
#'                                  x = pnodes,
#'                                  col = horTh),
#'                              alpha = 0.6),
#'                   expression(
#'                     geom_line(data = predict_data,
#'                               aes(x = pnodes,
#'                                   y = prediction,
#'                                   col = horTh),
#'                               size = 1.2)),
#'                   theme_bw(),
#'                   #theme(axis.title.y = element_text(size = 20)),
#'                   ylab("Survival Time")
#'     ),
#'     predict_arg = list(newdata = function(data){
#'       z <- data.frame(horTh = factor(rep(c("yes", "no"),
#'                                          each = length(data$pnodes)),
#'                                      levels = c("no", "yes")),
#'                       pnodes = rep(seq(from = min(data$pnodes),
#'                                        to = max(data$pnodes),
#'                                        length.out = length(data$pnodes)),
#'                                    2))
#'       z$x <- model.matrix(~ ., data = z)
#'       z},
#'       type = "quantile",
#'       p = 0.5),
#'     shared_axis_labels = FALSE
#'   )
#'
#' ########################################################################
#'
#' data("TeachingRatings", package = "AER")
#' tr <- subset(TeachingRatings, credits == "more")
#'
#' tr_tree <- lmtree(eval ~ beauty | minority + age + gender + division + native +
#'                     tenure, data = tr, weights = students, caseweights = FALSE)
#'
#' ggparty(tr_tree, terminal_space = 0.5, horizontal = FALSE) +
#'   geom_edge(size = 1.5) +
#'   geom_node_splitvar(fontface = "bold", size = 8) +
#'   geom_edge_label(colour = "grey", size = 6) +
#'   geom_node_plot(gglist = list(geom_point(aes(x = fitted_values,
#'                                              y = residuals,
#'                                              col = tenure,
#'                                              shape = minority)),
#'                               geom_hline(yintercept = 0),
#'                               theme_bw(base_size = 15)),
#'                 scales = "free_x",
#'                 id = "terminal",
#'                 shared_axis_labels = TRUE,
#'                 predict_arg = list(newdata = function(x) {
#'                   data.frame(beauty = seq(min(x$beauty),
#'                                           max(x$beauty),
#'                                           length.out = 100))
#'                 }))
#'
#' ## Boston housing data
#' data("BostonHousing", package = "mlbench")
#' BostonHousing <- transform(BostonHousing,
#'                            chas = factor(chas, levels = 0:1, labels = c("no", "yes")),
#'                            rad = factor(rad, ordered = TRUE))
#'
#' ## linear model tree
#' bh_tree <- lmtree(medv ~ log(lstat) + I(rm^2) | zn +
#'                     indus + chas + nox + age + dis + rad + tax + crim + b + ptratio,
#'                   data = BostonHousing, minsize = 40)
#'
#' ggparty(bh_tree, terminal_space = 0.5) +
#'   geom_edge() +
#'   geom_edge_label() +
#'   geom_node_splitvar() +
#'   geom_node_plot(gglist = list(
#'     geom_point(aes(y = medv, x = `log(lstat)`, col = chas))),
#'     height = 0.5) +
#'   geom_node_plot(gglist = list(
#'     geom_point(aes(y = medv, x = `I(rm^2)`, col = chas))),
#'     height = 0.5,
#'     nudge_y = -0.25)
#'

# geom_node_plot () --------------------------------------------------------


geom_node_plot <- function(plot_call = "ggplot",
                          gglist = NULL,
                          width = 1,
                          height = 1,
                          size = 1,
                          ids = "terminal",
                          scales = "fixed",
                          nudge_x = 0,
                          nudge_y = 0,
                          shared_axis_labels = FALSE,
                          predict_arg = NULL,
                          legend_separator = FALSE) {

  #input_checks
  assert_list(gglist)
  assert_numeric(width, finite = TRUE, any.missing = FALSE, max.len = 1)
  assert_numeric(height, finite = TRUE, any.missing = FALSE, max.len = 1)
  assert_subset(scales, c("fixed", "free", "free_x", "free_y"))
  assert_numeric(nudge_x, lower = -1, upper = 1, finite = TRUE, any.missing = FALSE,
                 max.len = 1)
  assert_logical(shared_axis_labels)
  assert_list(predict_arg, null.ok = TRUE)
  assert_logical(legend_separator)

  nodeplot_layer <- ggplot2::layer(
    data = NULL,
    mapping = NULL,
    stat = "identity",
    geom = GeomNodeplot,
    position = position_nudge(x = nudge_x, y = nudge_y),
    params = list(
      gglist = gglist,
      plot_call = enquote(plot_call),
      width = width,
      height = height,
      size = size,
      ids = ids,
      scales = scales,
      shared_axis_labels = shared_axis_labels,
      predict_arg = predict_arg,
      nudge_x = nudge_x,
      nudge_y = nudge_y))

  # set correct plot specs and draw legend separator
  if (shared_axis_labels) {
    nodeplot_layer <- list(nodeplot_layer, coord_cartesian(ylim = c(-0.075, 1.05),
                                                           xlim = c(-0.05, 1.05),
                                                           default = TRUE))
    if (legend_separator) nodeplot_layer <- list(nodeplot_layer,
                                                 geom_hline(yintercept = -0.05))
  } else {
    nodeplot_layer <- list(nodeplot_layer, coord_cartesian(ylim = c(-0.025, 1.05),
                                                           xlim = c(-0.05, 1.05),
                                                           default = TRUE))
    if (legend_separator) nodeplot_layer <- list(nodeplot_layer,
                                                 geom_hline(yintercept = 0))
  }
  nodeplot_layer

}


# GeomNodeplot ------------------------------------------------------------

GeomNodeplot <- ggproto(
  "GeomNodeplot",
  Geom,
  required_aes = c("x", "y", "id"),
  draw_panel = function(self,
                        data,
                        panel_params,
                        coord,
                        gglist,
                        plot_call,
                        width,
                        height,
                        size,
                        ids,
                        shared_axis_labels,
                        scales,
                        predict_arg,
                        nudge_x,
                        nudge_y) {

    data <- coord$transform(data, panel_params)

    vertical <- all(data$horizontal == FALSE)

    # rescale some constants and inputs
    y_0 <- scales::rescale(0, from = panel_params$y.range)
    y_1 <- scales::rescale(1, from = panel_params$y.range)
    x_1 <- scales::rescale(1, from = panel_params$x.range)
    x_0 <- scales::rescale(0, from = panel_params$x.range)
    legend_x <- scales::rescale(0.5, from = panel_params$x.range)
    legend_y <- scales::rescale(-0.05, from = panel_params$y.range)
    xlab_y <- scales::rescale(-0.025, from = panel_params$y.range)
    nudge_y <- scales::rescale(nudge_y, from = panel_params$y.range) - y_0
    nudge_x <- scales::rescale(nudge_x, from = panel_params$x.range) - x_0


    # calculate node node sizes ------------------------------------------------

    # for vertical trees
    if (vertical) {
      node_width <- abs(diff(data$x[data$kids == 0]))[1]
      node_height <- abs(diff(data$y[data$kids != 0]))[1]
      xlab_x <- legend_x
      ylab_y <- (min(data$y) + y_0) * 0.5
      ylab_x <- scales::rescale(-0.045, from = panel_params$x.range)
    } else {
      # for horizontal trees
      node_width <- abs(diff(data$x[data$kids != 0]))[1]
      node_height <- abs(diff(data$y[data$kids == 0]))[1]
      xlab_x <- (x_1 + max(data$x)) / 2
      ylab_x <- max(data$x)
      ylab_y <- scales::rescale(0.5, from = panel_params$y.range)
    }

    grob_list <- list()
    if (all(ids == "all")) ids <- unique(data$id)
    if (all(ids == "terminal")) ids <- unique(data$id[data$kids == 0])
    if (all(ids == "inner")) ids <- unique(data$id[data$kids != 0])


    #  transform data_* columns from lists to full dataframe -------------------

    data_columns <- substring(names(data), first = 1, last = 5) == "data_"
    nodeplot_data_lists <-  data[, data_columns]
    names(nodeplot_data_lists) <- substring(names(nodeplot_data_lists), 6)
    lengths <- sapply(nodeplot_data_lists[[2]], function(x) length(unlist(x)))
    # initialize dataframe with right number of rows per id
    nodeplot_data <- data.frame(id = rep(data$id, times = lengths))
    # fill dataframe
    for (column in names(nodeplot_data_lists)) {
      content <- numeric(0)
      for (i in seq_along(data$id)) {
        rows <- nodeplot_data_lists[[column]][[i]]
        content <- rbind(content, rows)
      }
      nodeplot_data[column] <- content
    }


    # draw plots --------------------------------------------------------------

    base_data <- nodeplot_data
    facet_data <- base_data[nodeplot_data$id %in% ids, ]


    # nodesize ----------------------------------------------------------------

    nodesize <- data$nodesize[data$id %in% ids]
    if(length(width) == 1) width <-rep(width, length(ids))
    else assert_subset(length(width), choices = c(1, length(ids)))

    if(length(height) == 1) height <-rep(height, length(ids))
    else assert_subset(length(height), choices = c(1, length(ids)))

    if (size[1] == "nodesize") size <-
      scales::rescale(nodesize, to = c(0, 1), from = c(0, max(nodesize)))
    if (size[1] == "log(nodesize)")
      size <- scales::rescale(log(nodesize), to = c(0,1),
                              from = c(0, max(log(nodesize))))
    if (length(size) == 1) size <- rep(size, length(ids))
    else assert_subset(length(size), choices = c(1, length(ids)))


    # calculate newdata and resulting predictions -----------------------------

    if (!is.null(predict_arg))
      predict_data <- predict_data(data$info_list, facet_data, predict_arg)


    # generate faceted base_plot ----------------------------------------------

    # facet_wrap for indiviual panels
    facet_gtable <- ggplotGrob(do.call(plot_call,
                                       args = list(data = facet_data)) +
                                 lapply(gglist, eval.parent, n = 1) +
                                 theme(legend.position = "bottom") +
                                 facet_wrap( ~ id, scales = scales, nrow = 1)
                               )

    # lab specifications for base table
    ggxlab <- ifelse(shared_axis_labels,
                     list(theme(axis.title.x = element_blank())),
                     list())
    ggylab <- list()
    if (shared_axis_labels & vertical) ggylab <- list(theme(axis.title.y = element_blank()))
    if (shared_axis_labels & !vertical) ggylab <- list(ylab(" "))

    # base plot as template
    base_gtable <- ggplotGrob(do.call(plot_call,
                                      args = list(data = base_data)) +
                                lapply(gglist, eval.parent, n = 1) +
                                ggxlab +
                                ggylab +
                                theme(legend.position = "none")
                              )
    base_layout <- base_gtable$layout


    # legend ------------------------------------------------------------------

    if (any(facet_gtable$layout$name == "guide-box")) {
      # extract gtable containing legend
      legend_gtable <- reduce_gtable(facet_gtable, "guide-box")
      legend_gtable$layout$t <- 1
      legend_gtable$layout$b <- 1
      legend_gtable$heights <- unit(1,"pt")

      #call nodeplotGrob on legend_gtable
      legend_gtable <- nodeplotGrob(
        x = legend_x,
        y = legend_y,
        width = 1,
        height = abs(legend_y - y_0),
        just = ifelse(shared_axis_labels, "top", "bottom"),
        node_gtable = legend_gtable
      )
      # add to groblist
      grob_list <- c(grob_list, list(legend_gtable))
    }


    # shared axis labels ------------------------------------------------------

    if (shared_axis_labels) {


      # x axis label -----------------------------------------------------------

      if (any(facet_gtable$layout$name == "xlab-b")) {
        # extract gtable containing bottom x_axis
        xlab_gtable <- reduce_gtable(facet_gtable, "xlab-b")
        xlab_gtable$layout$t <- 1
        xlab_gtable$layout$b <- 1
        xlab_gtable$heights <- unit(1,"pt")

        #call nodeplotGrob on xlab_gtable
        xlab_gtable <- nodeplotGrob(
          x = xlab_x,
          y = y_0,
          width = 1,
          height = (y_0 - xlab_y),
          just = "top",
          node_gtable = xlab_gtable
          )
        grob_list <- c(grob_list, list(xlab_gtable))
      }


      # y axis label ------------------------------------------------------------

      if (any(facet_gtable$layout$name == "ylab-l")) {
        # extract gtable containing bottom x_axis
        ylab_gtable <- reduce_gtable(facet_gtable, "ylab-l")
        ylab_gtable$layout$l <- 1
        ylab_gtable$layout$r <- 1
        ylab_gtable$layout$t <- 1
        ylab_gtable$layout$b <- 1
        ylab_gtable$widths <- unit(0,"pt")
        ylab_gtable$heights <- unit(0,"pt")
        ylab_gtable$grobs[[1]]$vp <- NULL
        ylab_gtable$grobs[[1]]$widths <- unit(0,"pt")
        ylab_gtable$grobs[[1]]$heights <- unit(0,"pt")

        #call nodeplotGrob on ylab_gtable
        ylab_gtable <- nodeplotGrob(
          y = ylab_y,
          x = ylab_x,
          width = 0.5,
          height = 1,
          just = "center",
          node_gtable = ylab_gtable
        )
        grob_list <- c(grob_list, list(ylab_gtable))
      }
    }

    # nodeplots ---------------------------------------------------------------

    # iterate through all ids to get all nodeplots
    nodeplot_gtable <- lapply(seq_along(ids), function(i) {

      # panel ------------------------------------------------------------------

      # create node_gtable as copy of base_gtable
      node_gtable <- base_gtable
      # get index of panel grob in base_gtable
      base_panel_index <- find_grob(base_gtable$grobs, "panel")
      # get index of ith panel in facet_gtable
      panel_i <- paste0("panel-", i,".")
      facet_panel_index <- find_grob(facet_gtable$grobs, panel_i)
      # replace panel grob in node_gtable with ith panel grob of facet_gtable
      node_gtable$grobs[[base_panel_index]] <- facet_gtable$grobs[[facet_panel_index]]

      # y axis ------------------------------------------------------------------

      base_y_axis_index <-  which(base_gtable$layout$name == "axis-l")

      if (scales == "fixed" | scales == "free_x" ) {
        facet_y_axis_index <- which(facet_gtable$layout$name == "axis-l-1-1")
      }
      if (scales == "free_y" | scales == "free") {
        y_axis_i <- paste0("axis-l-1-", i)
        facet_y_axis_index <-  which(facet_gtable$layout$name == y_axis_i)
      }
      node_gtable$grobs[[base_y_axis_index]] <- facet_gtable$grobs[[facet_y_axis_index]]


      # x axis ------------------------------------------------------------------

      base_x_axis_index <-  which(base_gtable$layout$name == "axis-b")
      x_axis_i <- paste0("axis-b-", i,"-1")
      facet_x_axis_index <-  which(facet_gtable$layout$name == x_axis_i)
      node_gtable$grobs[[base_x_axis_index]] <- facet_gtable$grobs[[facet_x_axis_index]]

      # generate nodeplotGrob ---------------------------------------------------

      # get x and y coords of nodeplot
      x <- unique(data[data$id == ids[i], "x"])
      y <- unique(data[data$id == ids[i], "y"])
      #vertical tree
      if(vertical) {
        nodeplotGrob(
          x = x,
          y = y,
          width = node_width * width[i] * size[i],
          height = ifelse(data$kids[data$id == ids[i]] == 0,
                          abs(y - nudge_y - y_0),
                          node_height) * height[i] * size[i],
          just = ifelse(data$kids[data$id == ids[i]] == 0,
                        "top",
                        "center"),
          node_gtable = node_gtable
        )
      } else { #horizontal tree
        nodeplotGrob(
          x = x,
          y = y,
          width = ifelse(data$kids[data$id == ids[i]] == 0,
                         abs(x - nudge_x - x_1),
                         node_width) * width[i] * size[i],
          height = node_height * height[i] * size[i],
          just = ifelse(data$kids[data$id == ids[i]] == 0,
                        "left",
                        "center"),
          node_gtable = node_gtable
        )
      }
    })
    #combine nodeplots and legend
    grob_list <- c(nodeplot_gtable, grob_list)
    class(grob_list) <- "gList"
    ggname("geom_node_plots", grid::grobTree(children = grob_list))
  }
)


nodeplotGrob <- function(x, y, node_gtable, width, height, just) {

  grid::gTree(x = x,
              y = y,
              node_gtable = node_gtable,
              width = width,
              height = height,
              just = just,
              cl = "nodeplotgrob")

}

#' appearantly needs to be exported
#' @param x nodeplotgrob
#' @export
#' @md
makeContent.nodeplotgrob <- function(x) {

  r <- x$node_gtable
  r$vp <- grid::viewport(x = x$x,
                         y = x$y,
                         width = x$width,
                         height = x$height,
                         just = x$just)

  grid::setChildren(x, grid::gList(r))
}

find_grob <- function(x, name) {
  which(sapply(x, function(x)
    substring(x$name, first = 1, last = nchar(name)) == name))
}

# function to remove selected elements from gtables, keeping widths
remove_grob <- function (g, what = "guide-box") {
  matches <- c(grepl(pattern = what, g$layout$name))
  g$layout <- g$layout[!matches, , drop = FALSE]
  g$grobs <- g$grobs[!matches]
  return(g)
}

reduce_gtable <- function (g, what = "guide-box") {
  matches <- c(grepl(pattern = what, g$layout$name))
  g$layout <- g$layout[matches, , drop = FALSE]
  g$grobs <- g$grobs[matches]
  return(g)
}

ggname <- function (prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}

predict_data <- function(info_list, data, predict_arg) {
  ids <- unique(data$id)
  newdata_function <- predict_arg$newdata
  for (i in 1:length(ids)) {
    predict_arg$newdata <- do.call(newdata_function, list(data))
    predict_arg$object <- info_list[[ids[i]]]$object
    predict_data <- predict_arg$newdata
    predict_data$id <- ids[i]
    predict_data$prediction <- do.call(stats::predict, predict_arg)

    if (i == 1) resulting_data <- predict_data
    else resulting_data <- rbind(resulting_data, predict_data)
  }
  resulting_data
}

