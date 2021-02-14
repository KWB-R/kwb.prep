test_that("msaccess_to_r_type() works", {

  result <- kwb.prep:::msaccess_to_r_type()

  expect_is(result, "list")
  expect_identical(result$VARCHAR, "character")
})
