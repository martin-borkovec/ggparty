#' autoplot methods for party objects
#' @export
#' @param object object of class party.
#' @param ... additional parameters
#' @importFrom methods is
#' @examples
#' library(ggparty)
#'
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
#' autoplot(py)

autoplot.party <- function(object, ...) {
  ggparty(object) +
    geom_edge() +
    geom_edge_label() +
    geom_node_label(aes(label = !!sym("splitvar")),
                    ids = "inner") +
    geom_node_label(aes(label = !!sym("info")),
                    ids = "terminal")
}

#' @rdname autoplot.party
#' @export
autoplot.constparty <- function(object, ...) {
  ggparty(object) +
    geom_edge() +
    geom_edge_label() +
    geom_node_label(aes(label = !!sym("splitvar")),
                    ids = "inner") +
    geom_node_plot(gglist = list(geom_bar(aes(x = "",
                                             fill = !!object$terms[[2]]),
                                position = position_fill()),
                                theme(axis.title   = element_blank()))
                  )
}
#' @rdname autoplot.party
#' @param plot_var Which covariate to plot against response. Defaults to second
#' column in `data` of tree.
#' @export
autoplot.modelparty <- function(object, plot_var = NULL, ...) {

  # if no plot var defined, use first covariable
  if (is.null(plot_var)) plot_var <- names(object$data)[2]

  y_var <- object$terms[[2]]
  if (all(is(object$data[[1]]) == "Surv"))
    y_var <- paste0(names(object$data)[1], ".time")

  ggparty(object) +
    geom_edge() +
    geom_edge_label() +
    geom_node_label(aes(label = !!sym("splitvar")),
                    ids = "inner") +
    geom_node_plot(gglist = list(geom_point(aes(x = !!sym(plot_var),
                                                y = !!sym(y_var)
                                                )
                                )))
}

#' @rdname autoplot.party
#' @param show_fit If TRUE `fitted_values` are drawn.
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
    geom_node_label(aes(label = !!sym("splitvar")),
                    ids = "inner") +
    geom_node_plot(gglist = list(geom_point(aes(x = !!sym(plot_var),
                                                y = !!object$terms[[2]])),
                                 plot_fit
    ))
}



