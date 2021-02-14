test_that("catf() works", {

  f <- kwb.prep:::catf
  
  expect_error(f())
  
  expect_identical(capture.output(f("abc")), "abc")
  expect_identical(capture.output(f("%s", "abc")), "abc")
  expect_identical(capture.output(f("%d items", 10L)), "10 items")
})
