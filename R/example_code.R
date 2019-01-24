library(partykit)
library(ggplot2)

source("R/get_plot_data.R")
source("R/ggparty.R")
#source("R/geom_node_terminal_plot.R")
source("R/geom_nodeplot.R")
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
py

str(py)

plot(py)
pynode <- py$node

ggparty(py, horizontal = F) +
  geom_edge() +
  geom_node_inner() +
  geom_edge_label_discrete() +
  geom_edge_label_continuous() +
  #geom_node_terminal_label() +
  geom_nodeplot(gglist = list(geom_point(aes(temperature,
                                             humidity,
                                             shape = play,
                                             col = humidity)),
                              theme_bw()))+
  theme_void() +
  ylim(-0.1,1.1) +
  xlim(-0.1,1.1)



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
  geom_nodeplot(gglist = list(geom_bar(aes(x = play)),
                              theme_bw())) +
  theme_void()+
  xlim(-0.1,1.1)+
  ylim(-0.1,1.1)



# Titanic -----------------------------------------------------------------

data("Titanic", package = "datasets")
ttnc <- as.data.frame(Titanic)
ttnc <- ttnc[rep(1:nrow(ttnc), ttnc$Freq), 1:4]
names(ttnc)[2] <- "Gender"

library(RWeka)
j48 <- J48(Survived ~ ., data = ttnc)
party_j48 <- as.party(j48)

plot(party_j48)

ggparty(party_j48) +
  geom_edge(size = 1) +
  geom_node_inner(fontface = "bold") +
  geom_edge_label_discrete(colour = "grey") +
  geom_edge_label_continuous(colour = "grey") +
  geom_nodeplot(gglist = list(geom_bar(aes(x = "", fill = Survived))))



# modelparty --------------------------------------------------------------


data("PimaIndiansDiabetes", package = "mlbench")
ct <- glmtree(diabetes ~ glucose | pregnant +
                pressure + triceps + insulin + mass + pedigree + age,
              data = PimaIndiansDiabetes, family = binomial)

plot(ct)

ggparty(ct) +
  geom_edge(size = 1) +
  geom_node_inner(fontface = "bold") +
  geom_edge_label_discrete(colour = "grey") +
  geom_edge_label_continuous(colour = "grey") +
  geom_node_terminal_plot(ct, gglist = scale_fill_brewer())



# Conditional Inference Trees ---------------------------------------------


ls <- data.frame(y = gl(3, 50, labels = c("A", "B", "C")),
                 x1 = rnorm(150) + rep(c(1, 0, 0), c(50, 50, 50)),
                 x2 = runif(150))

ct2 <- ctree(y ~ x1 + x2, data = ls)

class(ct2)

plot(ct2)

ggparty(tptree) +
  geom_edge(size = 1) +
  geom_node_inner(fontface = "bold") +
  geom_edge_label_discrete(colour = "grey") +
  geom_edge_label_continuous(colour = "grey") +
  geom_node_terminal_plot(ct2, gglist = list(xlab("lol"),
                                             ylab("roflcopter"),
                                             scale_fill_brewer()))


data("treepipit", package = "coin")
tptree <- ctree(counts ~ ., data = treepipit)

class(tptree)

plot(tptree)


# TO DO -------------------------------------------------------------------

# select nodes for plot
# fix xlab position etc.
# R package, manuals, documentation, etc...

# DONE --------------------------------------------------------------------

# implement support for index + breaks (nedds to be tested)
# horizontal layout
# user definied terminal plot
# shared scales etc.

# rewrite get_plot_data to be more efficient and using extractor functions to be
# robust against changes in partykit


