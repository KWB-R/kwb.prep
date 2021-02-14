test_that("set_columns() works", {

  f <- function(...) suppressMessages(kwb.prep:::set_columns(...))
  capt <- capture.output
  
  expect_error(capt(f()))

  capt(result <- f(data.frame(a = 1:2), b = "hello"))

  expect_identical(result$b, c("hello", "hello"))
})

