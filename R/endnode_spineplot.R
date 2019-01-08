
endnode_spineplot <- function(party, gglist = NULL, shared_legend = T, ...) {
  plot_data <- get_plot_data(party)
  endnodes_n <- sum(!is.na(plot_data$terminal))
  endnodes_ids <- plot_data$id[!is.na(plot_data$terminal)]
  plot_list <- vector("list", endnodes_n)

  for (i in 1:endnodes_n) {
    id <- endnodes_ids[i]
    plot_list[i] <- get_terminal_plot(party[[1]], party[[id]], gglist, ...)
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

