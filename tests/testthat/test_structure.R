source("R/get_plot_data.R")
source("R/ggparty.R")
source("R/get_terminal_plot.R")

test_that("valid types in data frame", {
  expect_is(get_plot_data(party(pn, WeatherPlay)), "data.frame")
  expect_is(get_plot_data(py)$x, "numeric")
  expect_is(get_plot_data(py)$y, "numeric")
  expect_is(get_plot_data(py)$parent, "numeric")

})

test_that("valid structure", {
  data("WeatherPlay", package = "partykit")

  sp_o <- partysplit(1L, index = 1:3)
  sp_h <- partysplit(3L, breaks = c(60,80))
  sp_w <- partysplit(4L, index = 1:2)

  character_split(sp_o)
  pn <- partynode(1L, split = sp_o, kids = list(
    partynode(2L, split = sp_h, kids = list(
      partynode(3L, info = "yes"),
      partynode(4L, info = "no"),
      partynode(9L, info = "maybe"))),
    partynode(5L, info = "yes"),
    partynode(6L, split = sp_w, kids = list(
      partynode(7L, info = "yes"),
      partynode(8L, info = "no")))))
  py <- party(pn, WeatherPlay)

  plot <- ggparty(py)
  #vdiffr::expect_doppelganger("ggplot2", plot)
  expect_is(plot,"ggplot")
  expect_is(py,"party")
})
