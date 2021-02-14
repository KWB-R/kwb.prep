test_that("cat_text() works", {

  f <- kwb.prep:::cat_text
  
  expect_error(f())
  expect_error(f("a"))
  
  result <- capture.output(f("applying_filter", "steps"))
  expect_identical(result, "Filterschritte 'steps' anwenden")
})
