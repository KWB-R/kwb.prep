test_that("delete_one_value_columns() works", {

  f <- kwb.prep:::delete_one_value_columns
  
  expect_error(capture.output(f()))

  capture.output(result <- f(data.frame(a = 1:3, b = 1)))
  expect_identical(result, data.frame(a = 1:3))
})
