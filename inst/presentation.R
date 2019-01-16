library(partykit)
library(ggplot2)

source("R/get_plot_data.R")
source("R/ggparty.R")
source("R/geom_node_terminal_plot.R")
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


# Party -------------------------------------------------------------------


py

str(py)

plot(py)
pynode <- py$node

ggparty(py) +
  geom_edge() +
  geom_node_inner() +
  geom_edge_label_discrete() +
  geom_edge_label_continuous() +
  geom_node_terminal_label() +
  theme_void() +
  ylim(c(0, 1))


ggparty(py) +
  geom_edge(size = 1.5) +
  geom_node_inner(fontface = "bold") +
  geom_edge_label_discrete(colour = "grey") +
  geom_edge_label_continuous(colour = "grey") +
  geom_node_terminal_label(colour = "red", fontface = "bold") +
  theme_void() +
  ylim(c(0, 1))

# constparty --------------------------------------------------------------

n1 <- partynode(id = 1L, split = sp_o, kids = lapply(2L:4L, partynode))

t2 <- party(n1,
            data = WeatherPlay,
            fitted = data.frame(
              "(fitted)" = fitted_node(n1, data = WeatherPlay),   "(response)" = WeatherPlay$play,
              check.names = FALSE),
            terms = terms(play ~ ., data = WeatherPlay)
)

t2 <- as.constparty(t2)
plot(t2)
class(t2)

ggparty(t2) +
  geom_edge(size = 1.5) +
  geom_node_inner(fontface = "bold") +
  geom_edge_label_discrete(colour = "grey") +
  geom_edge_label_continuous(colour = "grey") +
  geom_node_terminal_plot(t2, shared_legend = F)


ggparty(t2) +
  geom_edge(size = 1) +
  geom_node_inner(fontface = "bold") +
  geom_edge_label_discrete(colour = "grey") +
  geom_edge_label_continuous(colour = "grey") +
  geom_node_terminal_plot(t2,
                          shared_legend = T,
                          gglist = list(theme_bw(),
                                        ggtitle("Barplot"),
                                        scale_fill_brewer()))

# Titanic

data("Titanic", package = "datasets")
ttnc <- as.data.frame(Titanic)
ttnc <- ttnc[rep(1:nrow(ttnc), ttnc$Freq), 1:4]
names(ttnc)[2] <- "Gender"

library(RWeka)
j48 <- J48(Survived ~ ., data = ttnc)
party_j48 <- as.party(j48)

ggparty(party_j48) +
  geom_edge(size = 1) +
  geom_node_inner(fontface = "bold") +
  geom_edge_label_discrete(colour = "grey") +
  geom_edge_label_continuous(colour = "grey") +
  geom_node_terminal_plot(party_j48,
                          shared_legend = T,
                          gglist = list(theme_bw(),
                                        ylab("")))



# modelparty --------------------------------------------------------------


data("PimaIndiansDiabetes", package = "mlbench")
ct <- glmtree(diabetes ~ glucose | pregnant +
                pressure + triceps + insulin + mass + pedigree + age,
              data = PimaIndiansDiabetes, family = binomial)

ggparty(ct) +
  geom_edge(size = 1) +
  geom_node_inner(fontface = "bold") +
  geom_edge_label_discrete(colour = "grey") +
  geom_edge_label_continuous(colour = "grey") +
  geom_node_terminal_plot(ct, gglist = list(xlab("lol"),
                                            ylab("roflcopter"),
                                            scale_fill_brewer()))


# TO DO -------------------------------------------------------------------

# rewrite get_plot_data to be more efficient
# suggestions for arguments in geoms
# implement support for index + breaks
# horizontal layout
# user definied terminal plot
# shared scales etc.
# individually customizable plots?
# R package, manuals, documentation, etc...



# Questions for project partner -------------------------------------------
# how can you tell fit from ct terminal plots?

