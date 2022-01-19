test_that("getNames() works", {

  f <- kwb.prep:::getNames
  
  expect_error(f())

  expect_identical(f(1:3), c("x_1", "x_2", "x_3"))
  expect_identical(f(c(a = 1, 2, c = 3)), c("a", "x_2", "c"))
})
