test_that("get_text() works", {

  result <- kwb.prep:::get_text()

  expect_true(is_character_list(result))
})
