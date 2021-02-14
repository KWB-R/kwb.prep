test_that("applyFilterCriteria() works", {

  f <- function(...) {
    capture.output(
      result <- suppressMessages(kwb.prep:::applyFilterCriteria(...))
    )
    result
  }
  
  expect_error(f())

  x <- data.frame(a = 1:10, b = 2:11)
  
  expect_identical(f(x), x)
  
  result <- f(x, "a %% 2 == 0L")

  remove_details <- function(x) structure(
    x, details.filter = NULL, matches = NULL
  )
  
  expect_identical(remove_details(result), x[c(2, 4, 6, 8, 10), ])
})
