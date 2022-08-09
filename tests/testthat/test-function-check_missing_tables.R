test_that("check_missing_tables() works", {

  f <- kwb.prep:::check_missing_tables

  expect_error(f())

  # invalid_zip()
  zip_file <- invalid_zip("five-rows.zip")

  expect_error(f(zip_file, list(a = list(file = "a.txt"))), "a \\(a.txt\\)")

  filename <- "DatenCO_Haltungen_utf8.csv"

  expect_silent(f(zip_file, list(a = list(file = filename))))
})
