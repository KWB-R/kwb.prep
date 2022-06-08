#library(testthat)
test_that("regroupedValues() works", {

  f <- function(...) kwb.prep::regroupedValues(..., dbg = FALSE)

  expect_error(f())

  x <- 1:3
  expect_identical(f(x), x)
  
  x <- c("a", "b", "c")
  expect_identical(f(x), x)

  # to.factor is ignored if there is no config!
  expect_identical(f(x, to.factor = TRUE), x)
  
  config <- list(
    values = c("a", "b", "c"),
    labels1 = c("ab", "ab", "c"),
    labels2 = c("a", "bc", "bc"),
    labels3 = c("1", "21", "22"),
    labels4 = c("1", "2.1", "2.2")
  )
  
  expect_identical(f(x, config), config$labels1)
  expect_identical(f(x, config, "labels2"), config$labels2)
  
  # Values looking like numerics are converted to numeric!
  expect_identical(
    f(x, config, "labels3"), 
    as.numeric(config$labels3)
  )
  # ... even though to.factor is TRUE!
  expect_identical(
    f(x, config, "labels3", to.factor = TRUE), 
    as.numeric(config$labels3)
  )

  # Values looking like numerics are converted to numeric!
  expect_identical(
    f(x, config, "labels4"), 
    as.numeric(config$labels4)
  )
  # ... even though to.factor is TRUE!
  expect_identical(
    f(x, config, "labels4", to.factor = TRUE), 
    as.numeric(config$labels4)
  )
  
  # to.factor is only considered if values do not look numeric
  expect_identical(
    f(x, config, to.factor = TRUE), 
    as.factor(config$labels1)
  )
})
