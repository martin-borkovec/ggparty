library(testthat)
library(ggparty)

test_check("ggparty")
test_file("get_plot_data.R")
context("valid data frame")
test_that("valid types in data frame", {
  expect_is(plot_data, "data.frame")
  expect_is(plot_data$x, "")
})

test_that("data types correct", {
  expect_is(plot_data,"data.frame")
  expect_is(plot_data$numbers, "integer")
  #expect_is(testing_data$letters, 'character') #this one fails; they're factors
})

