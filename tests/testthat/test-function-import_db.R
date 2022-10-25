#library(testthat)
#kwb.prep::assign_objects()
#source("tests/testthat/helpers_test.R")
#yaml::write_yaml(list(input_dir = "<app_dir>/input"), "inst/extdata/config/path-dictionary.yml")

test_that("import_db() works", {

  f <- function(...) {
    capture.output(result <- import_db(...))
    result
  }
  
  config_dir <- package_file("testdata", "config")
  
  read_cnf <- function(x) {
    read_config(dbg = FALSE, file = kwb.utils::safePath(config_dir, x))
  }

  expect_error(f())

  zip_file <- invalid_zip("two-tables-missing.zip")
  config <- read_cnf(x = "csv_config_two-tables-missing.yml")
  expect_error(f(zip_file, config), "fehlen:\n- regions.*\n- trees")

  zip_file <- invalid_zip("missing-fields.zip")
  config <- read_cnf("csv_config_missing-fields.yml")
  expect_error(f(zip_file, config), ": my_missing")

  zip_file <- invalid_zip("five-rows.zip")
  config <- read_cnf("csv_config_five-rows.yml")
  import_dir <- kwb.utils::tempSubdirectory("test-import_db")
  path_db <- f(zip_file, config, import_dir = import_dir)
  expect_true(all(get_csv_filenames(config) %in% dir(path_db)))
})
