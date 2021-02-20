test_that("read_filter_criteria() works", {

  expect_output(y <- kwb.prep:::read_filter_criteria())
  expect_is(y, "list")
  expect_true("empty_criteria" %in% names(y))
})
