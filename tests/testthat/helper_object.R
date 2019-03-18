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

###
n1 <- partynode(id = 1L, split = sp_o, kids = lapply(2L:4L, partynode))

t2 <- party(n1,
            data = WeatherPlay,
            fitted = data.frame(
              "(fitted)" = fitted_node(n1, data = WeatherPlay),   "(response)" = WeatherPlay$play,
              check.names = FALSE),
            terms = terms(play ~ ., data = WeatherPlay)
)

t2 <- as.constparty(t2)

###

data("PimaIndiansDiabetes", package = "mlbench")
ct <- glmtree(diabetes ~ glucose | pregnant +
                pressure + triceps + insulin + mass + pedigree + age,
              data = PimaIndiansDiabetes, family = binomial)

###
ls <- data.frame(y = gl(3, 50, labels = c("A", "B", "C")),
                 x1 = rnorm(150) + rep(c(1, 0, 0), c(50, 50, 50)),
                 x2 = runif(150))

ct2 <- ctree(y ~ x1 + x2, data = ls)
###
data("treepipit", package = "coin")
tptree <- ctree(counts ~ ., data = treepipit)
###
data("BostonHousing", package = "mlbench")
BostonHousing <- transform(BostonHousing,
                           chas = factor(chas, levels = 0:1, labels = c("no", "yes")),
                           rad = factor(rad, ordered = TRUE))

bh_tree <- lmtree(medv ~ log(lstat) + I(rm^2) | zn + indus +
                    chas + nox +
                    +    age + dis + rad + tax + crim + b + ptratio, data = BostonHousing)

###
data("TeachingRatings", package = "AER")
tr <- subset(TeachingRatings, credits == "more")

tr_tree <- lmtree(eval ~ beauty | minority + age + gender + division + native +
                    tenure, data = tr, weights = students, caseweights = FALSE)
###

