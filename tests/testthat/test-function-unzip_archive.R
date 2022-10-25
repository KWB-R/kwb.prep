test_that("unzip_archive() works", {

  #source("tests/testthat/helpers_test.R")

  f <- kwb.prep:::unzip_archive

  zip_file <- invalid_zip("five-rows.zip")

  target_dir <- kwb.utils::tempSubdirectory("test")

  capture.output(tdir <- f(zip_file, target_dir))

  expect_identical(tdir, target_dir)

  expect_true("DatenCO_Haltungen_utf8.csv" %in% dir(target_dir))
})
