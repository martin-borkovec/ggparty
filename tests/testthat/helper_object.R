#different helper objects for examples and testing purposes

data("WeatherPlay", package = "partykit")
### splits ###
## split in overcast, humidity, and windy
sp_o <- partykit::partysplit(1L, index = 1:3)
sp_h <- partykit::partysplit(3L, breaks = 75)
sp_w <- partykit::partysplit(4L, index = 1:2)
## query labels
partykit::character_split(sp_o)
### nodes ###
## set up partynode structure
pn <- partykit::partynode(1L, split = sp_o, kids = list(
  partykit::partynode(2L, split = sp_h, kids = list(
    partykit::partynode(3L, info = "yes"),
    partykit::partynode(4L, info = "no"))),
  partykit::partynode(5L, info = "yes"),
  partykit::partynode(6L, split = sp_w, kids = list(
    partykit::partynode(7L, info = "yes"),
    partykit::partynode(8L, info = "no")))))
py <- partykit::party(pn, WeatherPlay)


