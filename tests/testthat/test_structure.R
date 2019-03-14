test_that("valid types in data frame", {
  expect_is(get_plot_data(party(pn, WeatherPlay)), "data.frame")
  expect_is(get_plot_data(party(pn, WeatherPlay))$x, "numeric")
  expect_is(get_plot_data(party(pn, WeatherPlay))$y, "numeric")
  expect_is(get_plot_data(party(pn, WeatherPlay))$parent, "integer")
})

test_that("valid structure", {
  plot <- ggparty(party(pn, WeatherPlay))
  #vdiffr::expect_doppelganger("ggplot", plot)
  expect_is(plot,"ggplot")
  expect_is(party(pn, WeatherPlay),"party")
 })
