library(ggplot2)
library(gridExtra)


# plot_data <- get_plot_data(py)
# terminal <- plot_data[!is.na(plot_data$terminal), ]
# plot_data <- plot_data[is.na(plot_data$terminal), ]
#
#
# ggplot(plot_data,
#        mapping = aes(x = x, y = y)) +
#   geom_edge() +
#   geom_node_inner(fontface = "bold") +
#   geom_edge_label_discrete(colour = "grey") +
#   geom_edge_label_continuous(colour = "grey") +
#   geom_node_terminal(colour = "red", fontface = "bold") +
#   theme_void()

ggparty(ct)

g2 <- qplot(1:10, 1:10)

gl <- list(g2, g2, g2, g2)

grid.arrange(
  grobs = gl,
  widths = c(1, 1, 1),
  layout_matrix = rbind(c(1,1,1),
                        c(2, 3, 4))
)

data("PimaIndiansDiabetes", package = "mlbench")
ct <- glmtree(diabetes ~ glucose | pregnant +
                     pressure + triceps + insulin + mass + pedigree + age,
                     data = PimaIndiansDiabetes, family = binomial)


class(ct)


plot(ct[[2]])


str(ct[[2]])
y <- Formula::model.part(ct[[2]]$info$Formula, mf, lhs = 1L,
                    rhs = 0L)


mobobj <- ct[[2]]


y <- Formula::model.part(mobobj$info$Formula, mf, lhs = 1L,
                         rhs = 0L)
y <- y[[1L]]

y <- relevel(y, "pos")
x <- Formula::model.part(mobobj$info$Formula, mf, lhs = 0L,
                         rhs = 1L)
x <- x[[1L]]
fitted <- mobobj$fitted[["(fitted)"]]

x_cat <-cut(x, quantile(ct[[1]]$dat$glucose))

product()

party
t2 <- party(n1,
            data = WeatherPlay,
            fitted = data.frame(
              "(fitted)" = fitted_node(n1, data = WeatherPlay),
              "(response)" = WeatherPlay$play,
              check.names = FALSE),
            terms = terms(play ~ ., data = WeatherPlay),
)
t2 <- as.constparty(ct)
t2
class(t2)


library(ggmosaic)


plot(ct)
