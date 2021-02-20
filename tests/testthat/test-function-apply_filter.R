test_that("apply_filter() works", {

  #source("tests/testthat/helpers_test.R");library(testthat)
  capture <- capture.output
  
  f <- function(...) suppressMessages(kwb.prep:::apply_filter(...))

  data <- data.frame(x = 1:10)
  
  expect_error(f())
  expect_error(y <- f(data, element = "missing"), "No such list elements")
  
  out <- capture(y <- f(data, element = "empty_criteria"))
  expect_length(grep("keine Filterkriterien", out), 1L)
  expect_length(grep("keine Laengenangaben", out), 1L)

  out <- capture(y <- f(data, element = "x_greater_0"))
  expect_length(grep("keine Laengenangaben", out), 1L)
  
  expect_identical(
    data, kwb.utils::removeAttributes(y, c("matches", "details.filter"))
  )

  capture(y <- f(data, "x_greater_3"))
  expect_true(nrow(y) == 7L)
  
  capture(y <- f(data, "x_greater_3_less_5"))
  expect_true(nrow(y) == 1L)
})
