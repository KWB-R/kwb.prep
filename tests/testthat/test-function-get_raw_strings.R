test_that("get_raw_strings() works", {

  result <- kwb.prep:::get_raw_strings()

  expect_true(is_character_list(result))
})
