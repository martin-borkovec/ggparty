library(partykit)
library(ggplot2)

source("R/get_plot_data.R")
source("R/ggparty.R")
### data ###
## artificial WeatherPlay data
data("WeatherPlay", package = "partykit")


### splits ###
## split in overcast, humidity, and windy
sp_o <- partysplit(1L, index = 1:3)
sp_h <- partysplit(3L, breaks = c(60,80))
sp_w <- partysplit(4L, index = 1:2)

## query labels
character_split(sp_o)


### nodes ###
## set up partynode structure
pn <- partynode(1L, split = sp_o, kids = list(
  partynode(2L, split = sp_h, kids = list(
    partynode(3L, info = "yes"),
    partynode(4L, info = "no"),
    partynode(9L, info = "maybe"))),
  partynode(5L, info = "yes"),
  partynode(6L, split = sp_w, kids = list(
    partynode(7L, info = "yes"),
    partynode(8L, info = "no")))))
pn

pn[5]
### tree ###
## party: associate recursive partynode structure with data
py <- party(pn, WeatherPlay)

str(py)
plot(py)
pynode <- py$node


ggparty(py) +
  geom_edge() +
  geom_node_inner(fontface = "bold") +
  geom_edge_label_discrete(colour = "grey") +
  geom_edge_label_continuous(colour = "grey") +
  geom_node_terminal(colour = "red", fontface = "bold") +
  theme_void()
