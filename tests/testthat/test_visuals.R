context("visual unit tests")
test_that("plots drawn correctly", {
  vdiffr::expect_doppelganger("WeatherPlay Plot",
                              ggparty(py) +
                                geom_edge() +
                                geom_edge_label() +
                                geom_node_label(aes(label = splitvar),
                                                ids = "inner") +
                                geom_node_label(aes(label = info),
                                                ids = "terminal")
                              )
  })
