#source("tests/testthat/helpers_test.R")

test_that("read_rename_select() works", {

  f <- kwb.prep:::read_rename_select

  file <- kwb.prep:::invalid_zip("five-rows.zip")
  path_db <- kwb.prep:::unzip_archive(file, dbg = FALSE)

  expect_error(f())

  config <- list(
    default = list(sep = ";", dec = "."),
    inspections = list(
      file = "DatenCO_Untersuchungen_utf8.csv",
      fields = list()
    )
  )

  capture.output(suppressMessages(
    f(path_db, table_name = "inspections", config = config)
  ))
})
