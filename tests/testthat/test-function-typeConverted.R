#library(testthat)
test_that("typeConverted() works", {

  f <- kwb.prep:::typeConverted

  expect_error(f())
  
  # Integers are converted to numeric. This is not nice!
  expect_identical(f(1:10), as.numeric(1:10))
  
  expect_identical(f("a", to.factor = FALSE), "a")
  expect_identical(f("a", to.factor = TRUE), factor("a"))
  
  factorLevels <- c("a", "b")
  expect_identical(
    f("a", to.factor = TRUE, factorLevels = factorLevels), 
    factor("a", levels = factorLevels)
  )
  # factorLevels are ignored if to.factor is FALSE
  expect_identical(
    f("a", to.factor = FALSE, factorLevels = factorLevels), 
    "a"
  )
  
  # to.factor is ignored if all values look like numeric!
  x <- c("1", "2.3")
  expect_identical(
    f(x, to.factor = TRUE), 
    c(1, 2.3)
  )
  # use to.numeric = FALSE to override this strange behaviour
  expect_identical(
    f(x, to.factor = TRUE, to.numeric = FALSE), 
    factor(x)
  )
})
