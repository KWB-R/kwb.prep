test_that("stop_text() works", {

  f <- kwb.prep::stop_text
  
  expect_error(f())
  expect_error(f("merging_failed"), "Merging.*led to.*more rows")

})
