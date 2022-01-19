#library(testthat)

test_that("eol_collapsed() works", {

  f <- kwb.prep:::eol_collapsed
  
  expect_identical(f(c("a", "b")), "a\nb")
  expect_identical(f(c("a", "b"), 1:2), "a1\nb2")
  expect_identical(f(c("a", "b"), 1:4, "_"), "a1_\nb2_\na3_\nb4_")
  
})
