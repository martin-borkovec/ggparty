library(ggplot2)

ggplot(plot_data, aes(x = x, y = y)) +
  geom_point() +
  theme_void()
