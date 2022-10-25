#library(testthat)

test_that("check_missing_fields() works", {

  f <- kwb.prep:::check_missing_fields

  exdir <- kwb.utils::tempSubdirectory("test")

  unzip(kwb.prep:::invalid_zip("missing-fields.zip"), exdir = exdir)

  baseconf <- list(default = list(sep = ";"))

  (config <- c(baseconf, list(a = list(
    file = "a.txt", 
    format = "csv",
    fields = list()))
  ))

  expect_error(f(exdir, config), "No such file: 'a.txt'")

  filename <- "DatenCO_Haltungen_utf8.csv"

  (config <- c(baseconf, list(a = list(
    file = filename,
    fields = list(f1 = list(field = "x"))
  ))))

  expect_error(f(exdir, config), "fehlen in den CSV-Dateien")

  (config <- c(baseconf, list(a = list(
    file = filename,
    fields = list(f1 = list(field = "Haltung"))
  ))))

  expect_null(f(exdir, config))
})
