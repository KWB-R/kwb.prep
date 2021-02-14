test_that("current_year() works", {

  f <- kwb.prep:::current_year
  
  expect_identical(f(), as.integer(format(Sys.Date(), "%Y")))
})
