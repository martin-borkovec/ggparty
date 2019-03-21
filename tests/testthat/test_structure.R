context("general structure and type based tests")
test_dataframe <- function(party_object){
  test_that("valid types in data frame", {
    expect_is(get_plot_data(party_object), "data.frame")
    expect_is(get_plot_data(party_object)$x, "numeric")
    expect_is(get_plot_data(party_object)$y, "numeric")
    expect_is(get_plot_data(party_object)$parent, "integer")
  })
}
test_dataframe(py)
test_dataframe(t2)
test_dataframe(ct)
test_dataframe(ct2)
test_dataframe(tr_tree)


test_structure <- function(party_object){
  test_that("valid structure", {
    plot <- ggparty(party_object)
    expect_is(plot,"ggplot")
    expect_is(party_object,"party")
  })
}
test_structure(py)
test_structure(t2)
test_structure(ct)
test_structure(ct2)
test_structure(tr_tree)
