test_that("read_string_definition() works", {

  result <- kwb.prep:::read_string_definition()

  expect_true(is_character_list(result))
})
