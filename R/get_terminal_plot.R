get_terminal_plot <- function(root, terminal_node) {
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
  treminal_plot <- list(ggplot(plot_data_terminal_node) +
    geom_mosaic(aes(x = product(x_cat), fill=y), na.rm=TRUE))
}
