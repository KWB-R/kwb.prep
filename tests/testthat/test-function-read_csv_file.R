test_that("read_csv_file() works", {

  #library(testthat)
  #source("tests/testthat/helpers_test.R")

  f <- kwb.prep:::read_csv_file
  
  expect_error(f())

  file <- tempfile("test_", fileext = ".csv")
  
  df <- data.frame(a = 1:3, b = c("a", "", "c"))
  write.csv2(df, file = file, row.names = FALSE)
  #kwb.utils::hsOpenWindowsExplorer(tempdir())
  #writeLines(readLines(file))
  
  result <- f(file, set_empty_string_to_na = TRUE)
  expect_identical(result$b, c("a", NA, "c"))
})

