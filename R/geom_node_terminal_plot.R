
geom_node_terminal_plot <- function(party, gglist = NULL, shared_legend = T, ...) {
  plot_data <- get_plot_data(party)
  endnodes_n <- sum(!is.na(plot_data$terminal))
  endnodes_ids <- plot_data$id[!is.na(plot_data$terminal)]
  plot_list <- vector("list", endnodes_n)

  for (i in 1:endnodes_n) {
    id <- endnodes_ids[i]
    if (is(party, "glmtree")) {
    plot_list[i] <- terminal_spineplot(party[[1]], party[[id]], gglist, ...)
    }
    if (is(party, "constparty")) {
      plot_list[i] <- terminal_barplot(party[[id]], gglist, ...)
    }

  }


  if (shared_legend == T) {
    g <- ggplotGrob(plot_list[[1]] + theme(legend.position = "bottom"))$grobs
    legend <- g[[which(sapply(g, function(x)
      x$name) == "guide-box")]]
    # lheight <- sum(legend$height)
    # lwidth <- sum(legend$width)
    plot_list <- lapply(plot_list, function(x)
    x + theme(legend.position = "none"))
    grob <- arrangeGrob(grobs = plot_list,
                        nrow = 1)
    grob <- arrangeGrob(grob, legend,
                        nrow = 2,
                        heights = c(0.5, 0.1))
  } else {
    grob <- arrangeGrob(grobs = plot_list,
                        nrow = 1)
  }

  annotation_custom(grob = grob, xmin = 0, xmax = 1, ymin = -0.5, ymax = 0)
}


terminal_spineplot <- function(root, terminal_node, gglist, ...) {
  require(ggmosaic)
  mf <- model.frame(terminal_node)
  y <- Formula::model.part(terminal_node$info$Formula, mf, lhs = 1L,
                           rhs = 0L)
  y <- y[[1L]]
  x <- Formula::model.part(terminal_node$info$Formula, mf, lhs = 0L,
                           rhs = 1L)
  x <- x[[1L]]
  x_cat <- cut(x, quantile(ct[[1]]$dat$glucose))
  plot_data_terminal_node <- data.frame(x_cat, y)
  list(ggplot(plot_data_terminal_node) +
         geom_mosaic(aes(x = product(x_cat),
                         fill = y),
                     na.rm = TRUE,
                     ...) +
         gglist

  )
}

terminal_spineplot <- function(root, terminal_node, gglist, ...) {
  require(ggmosaic)
  mf <- model.frame(terminal_node)
  y <- Formula::model.part(terminal_node$info$Formula, mf, lhs = 1L,
                           rhs = 0L)
  y <- y[[1L]]
  x <- Formula::model.part(terminal_node$info$Formula, mf, lhs = 0L,
                           rhs = 1L)
  x <- x[[1L]]
  x_cat <- cut(x, quantile(ct[[1]]$dat$glucose))
  plot_data_terminal_node <- data.frame(x_cat, y)
  list(ggplot(plot_data_terminal_node) +
         geom_mosaic(aes(x = product(x_cat),
                         fill = y),
                     na.rm = TRUE,
                     ...) +
         gglist

  )
}

terminal_barplot <- function(terminal_node, gglist, ...) {
  list(ggplot(data = data.frame(response = terminal_node$fitted$`(response)`),
              aes("")) +
        geom_bar(aes(fill = response),
                 position =  position_stack(),
                 ...) +
         gglist

  )
}

# str(t2)
# t2[[1]]$node$split
# t2

