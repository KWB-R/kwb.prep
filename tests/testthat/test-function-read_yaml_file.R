test_that("read_yaml_file() works", {

  f <- kwb.prep:::read_yaml_file
  file <- kwb.prep:::config_file("text_constants.yml")
  
  expect_error(f())
  expect_output(result <- f(file))
  expect_type(result, "list")
})
