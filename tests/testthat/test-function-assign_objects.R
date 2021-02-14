test_that("assign_objects() works", {

  kwb.prep:::assign_objects()
  
  expect_true("assign_objects" %in% ls())
})
