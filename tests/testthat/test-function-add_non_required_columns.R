#library(testthat)
test_that("add_non_required_columns() works", {

  f <- kwb.prep:::add_non_required_columns

  expect_error(f())
  
  config <- list(pipes = list(fields = list(
    a = list(required = FALSE, default = "bye"),
    b = list(required = FALSE, default = "hi")
  )))
  
  result <- f(data.frame(a = 1:3), config = config, table_name = "pipes")
  
  expect_identical(result$b, rep("hi", 3L))
})
