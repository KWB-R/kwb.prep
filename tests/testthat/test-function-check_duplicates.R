#library(testthat)
test_that("check_duplicates() works", {

  f <- kwb.prep:::check_duplicates

  expect_error(f())
  expect_silent(f(1:10))
  expect_error(f(c(1, 1)))
  expect_error(f(c(1, 2, 3, 4, 2)), "duplicates: 2")
  expect_error(f(c("a", "b", "a", "b")), "duplicates: a, b")
})
