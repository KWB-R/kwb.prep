test_that("config_file() works", {

  result <- kwb.prep:::config_file()
  
  expect_is(result, "character")
  expect_length(result, 1L)
  expect_true(file.exists(result))
})
