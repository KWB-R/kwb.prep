test_that("fieldSummary() works", {

  f <- kwb.prep:::fieldSummary
  
  expect_error(f())

  x <- data.frame(
    a = c( 1,   1,   2,   2,   2), 
    b = c("x", "y", "x", "y", "x"),
    Length = 1:5
  )
  
  result <- f(x, groupBy = "a")
  expect_identical(names(result), c("a", "Count", "Percentage"))
  
  result <- f(x, groupBy = c("a", "b"))
  expect_identical(names(result), c("a", "b", "Count", "Percentage"))

  result <- f(x, groupBy = c("a", "b"), lengthColumn = "Length")
  expect_equal(sum(result$Length), sum(x$Length))
  expect_equal(sum(result$Percentage), 100)
})
