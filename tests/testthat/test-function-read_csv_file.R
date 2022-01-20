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
  
  expect_message(capture.output(
    result <- f(file, set_empty_string_to_na = TRUE)
  ))
  
  expect_identical(result$b, c("a", NA, "c"))
  
  writeLines(con = file, c(
    "key;value1;value2",
    "a;1;2",
    "b;3;4",
    "#c;5;6"
  ))
  
  # Check that comment lines are removed by default
  expect_message(capture.output(result <- f(file)))
  expect_identical(names(result), c("key", "value1", "value2"))
  expect_identical(result$key, c("a", "b"))
  
  # Check that comment lines are kept if required
  expect_message(capture.output(result <- f(file, remove_comments = FALSE)))
  expect_identical(names(result), c("key", "value1", "value2"))
  expect_identical(result$key, c("a", "b", "#c"))
})
