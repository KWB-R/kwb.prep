test_that("set_column() works", {

  f <- kwb.prep:::set_column
  
  expect_error(f())

  df <- data.frame(a = 1:3)
  
  expect_error(f(df, "b"))
  expect_error(f(df, "a", NA, indices = 2))
  
  expect_identical(
    f(df, "a", NA), 
    data.frame(a = rep(NA, 3))
  )
  
  expect_identical(
    f(df, "a", NA, indices = 2L), 
    data.frame(a = c(1L, NA, 3L))
  )
})
