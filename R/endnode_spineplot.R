
endnode_spineplot <- function(party, gglist = NULL, ...) {
  plot_data <- get_plot_data(party)
  endnodes_n <- sum(!is.na(plot_data$terminal))
  endnodes_ids <- plot_data$id[!is.na(plot_data$terminal)]
  grob_list <- vector("list", endnodes_n)

    for (i in 1:endnodes_n) {
    id <- endnodes_ids[i]
    grob_list[i] <- get_terminal_plot(party[[1]], party[[id]], gglist, ...)
    }
  grob <-  grid.arrange(grobs = grob_list,
                        nrow = 1)
  annotation_custom(grob = grob, xmin = 0, xmax = 1, ymin = -0.5, ymax = 0)
}

endnode_unified_legend <- function(party, gglist = NULL, id = NULL ...) {
  plot_data <- get_plot_data(party)
  plot_data$id[!is.na(plot_data$terminal)]
  for (i in 1:endnodes_n) {
    id <- endnodes_ids[i]
    grob_list[i] <- get_terminal_plot(party[[1]], party[[id]], gglist, ...)
  }
  grob <-  grid.arrange(grobs = grob_list,
                        nrow = 1)
  annotation_custom(grob = grob, xmin = 0, xmax = 1, ymin = -0.5, ymax = 0)
}



