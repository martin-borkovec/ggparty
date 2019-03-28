#' @export
#' @importFrom methods is
autoplot.party <- function(object, ...) {
  ggparty(object) +
    geom_edge() +
    geom_edge_label() +
    geom_node_label(aes_string(label = "splitvar"),
                    ids = "inner") +
    geom_node_label(aes_string(label = "info"),
                    ids = "terminal")
}

#' @export
autoplot.constparty <- function(object, ...) {
  ggparty(object) +
    geom_edge() +
    geom_edge_label() +
    geom_node_label(aes_string(label = "splitvar"),
                    ids = "inner") +
    geom_node_plot(gglist = list(geom_bar(aes(x = "",
                                             fill = !!object$terms[[2]]),
                                position = position_fill()),
                                theme(axis.title   = element_blank()))
                  )
}

#' @export
autoplot.modelparty <- function(object, plot_var = NULL, ...) {

  # if no plot var defined, use first covariable
  if (is.null(plot_var)) plot_var <- names(object$data)[2]

  y_var <- object$terms[[2]]
  if(all(is(object$data[[1]]) == "Surv"))
    y_var <- paste0(names(object$data)[1], ".time")

  ggparty(object) +
    geom_edge() +
    geom_edge_label() +
    geom_node_label(aes_string(label = "splitvar"),
                    ids = "inner") +
    geom_node_plot(gglist = list(geom_point(aes(x = !!sym(plot_var),
                                                y = !!sym(y_var)
                                                )
                                )))
}

#' @export
autoplot.lmtree <- function(object, plot_var = NULL, show_fit = TRUE, ...) {

  # if no plot var defined, use first covariable
  if (is.null(plot_var)) plot_var <- names(object$data)[2]

  # plot fitted values if show_fit
  plot_fit <- NULL
  if (show_fit == TRUE)
    plot_fit <- list(
      geom_line(aes(x = !!sym(plot_var),
                    y = !!sym("fitted_values")),
                col = "blue"))

  ggparty(object) +
    geom_edge() +
    geom_edge_label() +
    geom_node_label(aes_string(label = "splitvar"),
                    ids = "inner") +
    geom_node_plot(gglist = list(geom_point(aes(x = !!sym(plot_var),
                                                y = !!object$terms[[2]])),
                                 plot_fit
    ))
}



