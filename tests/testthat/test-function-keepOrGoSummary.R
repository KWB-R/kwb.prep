test_that("keepOrGoSummary() works", {

  #library(testthat)
  f <- kwb.prep:::keepOrGoSummary
  
  expect_error(f())
  
  x.base <- data.frame(
    C1 = 1, 
    C2 = 10, 
    Length = 100
  )
  
  y.base <- data.frame(
    C1 = c(TRUE, TRUE, TRUE, TRUE),
    C2 = c(FALSE, FALSE, FALSE, FALSE),
    Length = 2:5
  )
  
  f(1, lengthColumn = "abc", x.base, y.base, criteriaNames = c("A", "B"))
  f(2, lengthColumn = "abc", x.base, y.base, criteriaNames = c("A", "B"))
})
