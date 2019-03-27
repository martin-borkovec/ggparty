context("visual unit tests")
test_that("plots drawn correctly", {
  vdiffr::expect_doppelganger("WeatherPlay Plot",
                              ggparty(py, horizontal = F) +
                                geom_edge() +
                                geom_node_splitvar(nudge_y = 0.0) +
                                geom_edge_label(shift = 0.5) +
                                geom_node_info() +
                                geom_node_plot(gglist = list(geom_point(aes(temperature,humidity,shape = play,col = humidity,size = temperature))),
                                ids = "terminal",
                                scales = "fixed",
                                width = 0.15,
                                height = 0.15,
                                y_nudge = 0,
                                x_nudge = 0
                                ) +
                                ylim(-0.3, 1))
  })

