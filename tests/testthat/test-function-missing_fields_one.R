test_that("missing_fields_one() works", {

  f <- kwb.prep:::missing_fields_one

  expect_error(f())

  #source("tests/testthat/helpers_test.R")

  capture.output(path_db <- kwb.prep:::unzip_archive(
    zip_file = invalid_zip("missing-fields.zip"),
    target_dir = kwb.prep:::temp_import_dir(),
    dbg = FALSE
  ))

  config <- list(
    default = list(sep = ","),
    pipes = list(
      file = "DatenCO_Haltungen_utf8.csv",
      fields = list()
    )
  )

  expect_identical(f(path_db, config, table_name = "pipes"), character(0))

  config$pipes$fields <- list(cool_name = list(field = "ugly_name"))

  expect_identical(
    f(path_db, config, table_name = "pipes"),
    stats::setNames("ugly_name", "cool_name")
  )
})
