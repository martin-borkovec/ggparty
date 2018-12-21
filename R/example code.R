library(partykit)

source("R/get_plot_data.R")
### data ###
## artificial WeatherPlay data
data("WeatherPlay", package = "partykit")


### splits ###
## split in overcast, humidity, and windy
sp_o <- partysplit(1L, index = 1:3)
sp_h <- partysplit(3L, breaks = 75)
sp_w <- partysplit(4L, index = 1:2)

## query labels
character_split(sp_o)


### nodes ###
## set up partynode structure
pn <- partynode(1L, split = sp_o, kids = list(
  partynode(2L, split = sp_h, kids = list(
    partynode(3L, info = "yes"),
    partynode(4L, info = "no"))),
  partynode(5L, info = "yes"),
  partynode(6L, split = sp_w, kids = list(
    partynode(7L, info = "yes"),
    partynode(8L, info = "no")))))
pn


### tree ###
## party: associate recursive partynode structure with data
py <- party(pn, WeatherPlay)

plot(party)
partynode <- py$node
party <- py

plot_data <- get_plot_data(party)
plot_data <- add_layout(plot_data)

library(ggplot2)

varnames <- names(party[[1]]$data)

plot_data$x_parent <- c(NA, plot_data$x[plot_data$parent])
plot_data$y_parent <- c(NA, plot_data$y[plot_data$parent])


ggplot(plot_data, aes(x = x, y = y)) +
  geom_segment(aes(x = x, y = y, xend = x_parent, yend= y_parent)) +
  geom_label(aes(label = varnames[splitvar])) +
  geom_label(aes(label = terminal)) +
  #geom_label(aes(label = varnames[]))+
  theme_void()


