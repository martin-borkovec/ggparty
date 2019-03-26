#' @export
autoplot.party <- function(x) {
  ggparty(x) +
    geom_edge() +
    geom_edge_label() +
    geom_node_label(aes(label = splitvar),
                    ids = "inner") +
    geom_node_label(aes(label = info),
                    ids = "terminal")
}

#' @export
autoplot.constparty <- function(x) {
  ggparty(x) +
    geom_edge() +
    geom_edge_label() +
    geom_node_label(aes(label = splitvar),
                    ids = "inner") +
    geom_nodeplot(gglist = list(geom_bar(aes(x = "",
                                             fill = !!x$terms[[2]]),
                                position = position_fill()),
                                theme(axis.title   = element_blank()))
                  )
}

#' @export
autoplot.modelparty <- function(x, plot_var) {
  ggparty(x) +
    geom_edge() +
    geom_edge_label() +
    geom_node_label(aes(label = splitvar),
                    ids = "inner") +
    geom_nodeplot(gglist = list(geom_point(aes(x = !!ensym(plot_var),
                                             y = !!x$terms[[2]])),
                                expression(
                                  geom_line(data = predict_data,
                                            aes(x = !!ensym(plot_var),
                                                y = prediction),
                                            size = 1.2,
                                            col = "blue")
                                )),
                  predict_arg =
                    list(newdata =
                           function(x) {
                             z <- data.frame(seq(from = min(x[, eval(plot_var)], na.rm = T),
                                                 to = max(x[, eval(plot_var)], na.rm = T),
                                                 length.out = 100))
                             names(z) <- eval(plot_var)
                             return(z)
                           }))
}
