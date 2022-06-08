test_that("get_user_strings() works", {

  kwb.prep:::set_global("user_strings", NULL)
  result <- kwb.prep::get_user_strings()
  expect_null(result)
})
