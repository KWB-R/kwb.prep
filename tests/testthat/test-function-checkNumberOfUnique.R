test_that("checkNumberOfUnique() works", {

  f <- kwb.prep:::checkNumberOfUnique
  
  expect_error(capture.output(f()))
  expect_output(f(data.frame(a = 1:3, b = "x")), "- a: 3\n- b: 1")
})
