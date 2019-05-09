context("testthat.R")
library(testthat)
library(visualisationIMPACT)





test_that("colums that doesn't exist are found", {
  expect_error(add_parent_to_loop(loop_data1, parent_data1 , c("others")))
  expect_error(add_parent_to_loop(loop_data1, parent_data1 , c("ages","other")))
  expect_error(add_parent_to_loop(loop_data1, parent_data1 , c("Age_parent","Other")))
})
