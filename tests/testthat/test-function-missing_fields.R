#library(testthat)
#source("tests/testthat/helpers_test.R")

test_that("missing_fields() works", {

  f <- kwb.prep:::missing_fields

  capture.output(target_dir <- kwb.prep:::unzip_archive(
    zip_file = kwb.prep:::invalid_zip("missing-fields.zip"),
    target_dir = kwb.prep:::temp_import_dir(),
    dbg = FALSE
  ))
  
  #kwb.utils::hsOpenWindowsExplorer(target_dir)
  
  expect_identical(f(target_dir, list()), list())

  config <- list(
    default = list(sep = ","),
    pipes = list(
      file = "DatenCO_Haltungen_utf8.csv",
      fields = list()
    )
  )

  expect_length(f(target_dir, table_names = "pipes", config = config), 0L)
  
  config2 <- config
  config2$pipes$fields <- list(
    material = list(
      field = "my-material"
    )
  )
  
  expect_identical(
    f(target_dir, config2, "pipes"),
    list(pipes = c(material = "my-material"))
  )
})
