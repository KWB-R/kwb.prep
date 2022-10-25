test_that("is_required_field() works", {

  f <- kwb.prep:::is_required_field

  expect_error(f())
  
  expect_identical(
    f(list(
      a = list(required = TRUE),
      b = list(required = FALSE),
      c = list(maybe = TRUE)
    )),
    c(a = TRUE, b = FALSE, c = TRUE)
  )
    
})
